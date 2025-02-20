"use client" // This is a client component 👈🏽

import { useSearchParams } from 'next/navigation'
import React, { useContext, useState } from 'react'

export interface NetworkSettingService {
    currentNetworkMagic: number
    updateNetwork: (networkMagic: number) => void
}

const NetworkContext: React.Context<NetworkSettingService> =
    React.createContext({} as NetworkSettingService)

export const useNetworkContext = () => {
    const context = useContext(NetworkContext)
    if (!context) {
        throw new Error("useNetworkContext must be used within a NetworkSettingProvider")
    }
    return context
}

const NetworkSettingProvider: React.FC<any> = ({
    children
}) => {
    const searchParams = useSearchParams()
    const currentNetwork = Number(searchParams.get("network")) || 764824073
    const [currNetworkMagic, setCurrentNetworkMagic] = useState(currentNetwork)

    const handleNetworkChange = (networkMagic: number) => {
        setCurrentNetworkMagic(networkMagic)
    }

    return (
        <NetworkContext.Provider value={{
            currentNetworkMagic: currNetworkMagic,
            updateNetwork: handleNetworkChange
        }}>
            {children}
        </NetworkContext.Provider>
    )
}

export default NetworkSettingProvider
