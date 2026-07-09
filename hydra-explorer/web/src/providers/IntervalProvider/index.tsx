"use client"

import React, { useContext, useState } from 'react'

export interface IntervalSettingService {
    isAutoUpdate: boolean
    intervalTime: number
    toggleAutoUpdate: () => void
    updateIntervalTime: (seconds: number) => void
}

const IntervalContext: React.Context<IntervalSettingService> =
    React.createContext({} as IntervalSettingService)

export const useIntervalContext = () => {
    const context = useContext(IntervalContext)
    if (!context) {
        throw new Error("useIntervalContext must be used within a IntervalSettingProvider")
    }
    return context
}

const IntervalSettingProvider: React.FC<{ children: React.ReactNode }> = ({
    children
}) => {
    const defaultInterval = Number(process.env.NEXT_PUBLIC_PULL_INTERVAL) || 1000

    const [intervalTime, setIntervalTime] = useState<number>(defaultInterval)
    const [isAutoUpdateOn, setAutoUpdate] = useState(true)

    const handleToggleAutoUpdate = () => {
        setAutoUpdate((prev) => !prev)
    }

    const handleIntervalTimeChange = (ms: number) => {
        setIntervalTime(ms)
    }

    return (
        <IntervalContext.Provider value={{
            isAutoUpdate: isAutoUpdateOn,
            intervalTime,
            toggleAutoUpdate: handleToggleAutoUpdate,
            updateIntervalTime: handleIntervalTimeChange,
        }}>
            {children}
        </IntervalContext.Provider>
    )
}

export default IntervalSettingProvider
