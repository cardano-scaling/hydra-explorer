"use client"

import { useIntervalContext } from '@/providers/IntervalProvider'
import { useEffect, useRef } from 'react'

export interface FetchDataOptions<T> {
    url: string
    setFetchedData: (data: T) => void
    setError: (error: string) => void
}

function useDataFetcher<T>({
    url,
    setFetchedData,
    setError,
}: FetchDataOptions<T>) {
    const { intervalTime, isAutoUpdate } = useIntervalContext()
    const fetchDataRef = useRef<(() => void) | undefined>(undefined)

    useEffect(() => {
        const fetchData = async () => {
            try {
                const response = await fetch(url)
                if (!response.ok) {
                    throw new Error('Failed to fetch data')
                }
                const data: T = await response.json()
                setFetchedData(data)
            } catch {
                setError('Error fetching data. Please try again later.')
            }
        }

        fetchDataRef.current = fetchData

        if (isAutoUpdate) {
            fetchData()
        }

        const intervalId = setInterval(() => {
            if (isAutoUpdate && fetchDataRef.current) {
                fetchDataRef.current()
            }
        }, intervalTime)

        return () => {
            clearInterval(intervalId)
        }
    }, [url, intervalTime, isAutoUpdate, setFetchedData, setError])

    return {}
}

export default useDataFetcher
