"use client" // This is a client component 👈🏽

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
    const [currNetworkMagic, setCurrentNetworkMagic] = useState(2)

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
