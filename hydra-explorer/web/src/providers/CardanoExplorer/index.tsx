"use client" // This is a client component 👈🏽

import React, { PropsWithChildren, useContext } from "react"

export interface CardanoExplorer {
    mintPolicy: (policyId: string) => string
    tx: (txIn: string) => string
    block: (blockHash: string) => string
    address: (addr: string) => string
}

const CardanoExplorerContext: React.Context<CardanoExplorer> =
    React.createContext({} as CardanoExplorer)

export const useCardanoExplorer = () => {
    const context = useContext(CardanoExplorerContext)
    if (!context) {
        throw new Error("useCardanoExplorer must be used within a CardanoExplorerProvider")
    }
    return context
}

export type CardanoExplorerProps = {
    network: string
}

export const CardanoExplorerProvider: React.FC<PropsWithChildren<CardanoExplorerProps>> =
    ({ children }) => {

        const { currentNetworkMagic } = useNetworkContext()

        let explorerUrl;
        switch (currentNetworkMagic) {
            case 764824073:
                explorerUrl = "cexplorer.io";
            case 1:
                explorerUrl = "preprod.cexplorer.io";
            case 2:
                explorerUrl = "preview.cexplorer.io";
            default:
                throw new Error("Unsupported network magic")
        }

        const cexplorer: CardanoExplorer = {
            mintPolicy: (policyId: string) => `https://${explorerUrl}/policy/${policyId}/mint`,
            tx: (txIn: string) => `https://${explorerUrl}/tx/${txIn}`,
            block: (blockHash: string) => `https://${explorerUrl}/block/${blockHash}`,
            address: (addr: string) => `https://${explorerUrl}/address/${addr}`
        }

        return (
            <CardanoExplorerContext.Provider value={cexplorer}>
                {children}
            </CardanoExplorerContext.Provider>
        )
    }
