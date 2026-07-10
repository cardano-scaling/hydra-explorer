import { useEffect, useRef } from 'react'
import { useStore } from '@/store/useStore'
import { toast } from 'sonner'

export function usePollingEngine() {
    const intervalTime = useStore((state) => state.intervalTime)
    const isAutoUpdate = useStore((state) => state.isAutoUpdate)
    const isOnline = useStore((state) => state.isOnline)
    const currentNetworkMagic = useStore((state) => state.currentNetworkMagic)

    const setHeads = useStore((state) => state.setHeads)
    const setTicks = useStore((state) => state.setTicks)
    const setHeadsError = useStore((state) => state.setHeadsError)
    const setTicksError = useStore((state) => state.setTicksError)

    const prevAutoUpdate = useRef(isAutoUpdate)
    const prevNetworkMagic = useRef(currentNetworkMagic)
    const isFirstMount = useRef(true)
    const headsInFlight = useRef(false)
    const ticksInFlight = useRef(false)

    useEffect(() => {
        // Track the auto-update and network transitions
        const isTransitionToPaused = prevAutoUpdate.current && !isAutoUpdate
        prevAutoUpdate.current = isAutoUpdate

        const isNetworkChanged = prevNetworkMagic.current !== currentNetworkMagic
        prevNetworkMagic.current = currentNetworkMagic

        // If offline, suspend polling immediately
        if (!isOnline) {
            return
        }

        const getUrl = (path: string) => {
            const baseUrl = process.env.NEXT_PUBLIC_EXPLORER_URL || ''
            if (!baseUrl) return path
            const normalizedBase = baseUrl.endsWith('/') ? baseUrl.slice(0, -1) : baseUrl
            return `${normalizedBase}${path}`
        }

        const headsUrl = getUrl('/heads')
        const ticksUrl = getUrl('/ticks')

        const controller = new AbortController()

        // active interval time for backoff, starting with base intervalTime
        let currentDelay = intervalTime

        // Classify fetch results: success, 5xx error, 4xx error, or connection timeout/error
        const fetchData = async (
            url: string,
            setter: (data: any) => void,
            setError: (err: string | null) => void,
            inFlightRef: React.MutableRefObject<boolean>
        ) => {
            if (inFlightRef.current) return { success: true }
            inFlightRef.current = true

            const fetchController = new AbortController()
            const onParentAbort = () => fetchController.abort()
            controller.signal.addEventListener('abort', onParentAbort)

            // Connection timeout (10 seconds)
            const timeoutId = setTimeout(() => {
                fetchController.abort()
            }, 10000)

            try {
                const res = await fetch(url, { signal: fetchController.signal })
                
                // If controller has been aborted in the meantime, ignore
                if (controller.signal.aborted) {
                    clearTimeout(timeoutId)
                    controller.signal.removeEventListener('abort', onParentAbort)
                    return { success: true }
                }

                if (!res.ok) {
                    clearTimeout(timeoutId)
                    controller.signal.removeEventListener('abort', onParentAbort)
                    
                    // Return failure status (e.g. 500 or 404)
                    return { success: false, status: res.status }
                }

                const data = await res.json()
                
                // If controller has been aborted in the meantime, ignore
                if (controller.signal.aborted) {
                    clearTimeout(timeoutId)
                    controller.signal.removeEventListener('abort', onParentAbort)
                    return { success: true }
                }

                clearTimeout(timeoutId)
                controller.signal.removeEventListener('abort', onParentAbort)
                
                setter(data)
                setError(null)
                return { success: true }
            } catch (err: any) {
                clearTimeout(timeoutId)
                controller.signal.removeEventListener('abort', onParentAbort)

                if (err.name === 'AbortError') {
                    if (controller.signal.aborted) {
                        // Aborted due to unmount/dependency change, ignore
                        return { success: true }
                    }
                    setError('Connection timeout')
                    return { success: false, isTimeout: true }
                }

                setError(err.message || 'Data connection error')
                return { success: false, isTimeout: true }
            } finally {
                inFlightRef.current = false
            }
        }

        const runFetchCycle = async () => {
            const [headsResult, ticksResult] = await Promise.all([
                fetchData(headsUrl, setHeads, setHeadsError, headsInFlight),
                fetchData(ticksUrl, setTicks, setTicksError, ticksInFlight)
            ])

            if (controller.signal.aborted) return

            // Backoff should only trigger on 5xx or connection timeouts/network errors
            const failedHeads = !headsResult.success && ((headsResult.status && headsResult.status >= 500) || headsResult.isTimeout)
            const failedTicks = !ticksResult.success && ((ticksResult.status && ticksResult.status >= 500) || ticksResult.isTimeout)
            const isBackoffTrigger = failedHeads || failedTicks

            if (isBackoffTrigger) {
                // Scale interval exponentially up to 60s
                currentDelay = Math.min(60000, currentDelay * 2)

                toast.warning(`API connection error. Retrying in ${currentDelay / 1000} seconds.`, {
                    id: 'api-polling-backoff',
                })
            } else {
                // Restore normal polling interval
                if (currentDelay !== intervalTime) {
                    toast.success('API connection restored.', {
                        id: 'api-polling-backoff',
                        duration: 3000
                    })
                }
                currentDelay = intervalTime
            }
        }

        // 1. Initial fetch (runs on mount, network change, or auto-update re-enable, ensures no stuck loading state)
        if (isFirstMount.current) {
            isFirstMount.current = false
            runFetchCycle()
        } else if (isNetworkChanged || (!isTransitionToPaused && isAutoUpdate)) {
            runFetchCycle()
        }

        // 2. Schedule polling only if auto-update is active
        let timeoutId: any = null
        if (isAutoUpdate) {
            const poll = async () => {
                await runFetchCycle()
                if (isAutoUpdate && !controller.signal.aborted) {
                    timeoutId = setTimeout(poll, currentDelay)
                }
            }
            timeoutId = setTimeout(poll, currentDelay)
        }

        return () => {
            controller.abort()
            if (timeoutId) clearTimeout(timeoutId)
        }
    }, [intervalTime, isAutoUpdate, isOnline, currentNetworkMagic, setHeads, setTicks, setHeadsError, setTicksError])
}
