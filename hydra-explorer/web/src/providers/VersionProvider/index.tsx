"use client" // This is a client component 👈🏽

import React, { useContext, useState } from 'react'

export interface VersionSettingService {
    currentVersion: string
    updateVersion: (seconds: string) => void
}

const VersionContext: React.Context<VersionSettingService> =
    React.createContext({} as VersionSettingService)

export const useVersionContext = () => {
    const context = useContext(VersionContext)
    if (!context) {
        throw new Error("useVersionContext must be used within a VersionSettingProvider")
    }
    return context
}

const VersionSettingProvider: React.FC<any> = ({
    children
}) => {
    const [currVersion, setCurrentVersion] = useState("latest")

    const handleVersionChange = (version: string) => {
        setCurrentVersion(version)
    }

    return (
        <VersionContext.Provider value={{
            currentVersion: currVersion,
            updateVersion: handleVersionChange
        }}>
            {children}
        </VersionContext.Provider>
    )
}

export default VersionSettingProvider
