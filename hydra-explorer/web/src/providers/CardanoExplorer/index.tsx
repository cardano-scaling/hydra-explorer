"use client" // This is a client component 👈🏽

import { mainnetNetworkMagic, useNetworkContext } from "@/providers/NetworkProvider"
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
}

export const CardanoExplorerProvider: React.FC<PropsWithChildren<CardanoExplorerProps>> =
    ({ children }) => {

        const { currentNetworkMagic } = useNetworkContext()

        let explorerUrl: string
        // XXX: This seems to be an in-progress change across cexplorer, so
        // will likely need to be updated soon.
        let mintUrl: string
        switch (currentNetworkMagic) {
            case mainnetNetworkMagic:
                explorerUrl = "cexplorer.io"
                mintUrl = "/mint"
                break
            case 1:
                explorerUrl = "preprod.cexplorer.io"
                mintUrl = "?tab=mint"
                break
            case 2:
                explorerUrl = "preview.cexplorer.io"
                mintUrl = "?tab=mint"
                break
            default:
                throw new Error("Unsupported network magic: " + currentNetworkMagic)
        }

        const cexplorer: CardanoExplorer = {
            mintPolicy: (policyId: string) => `https://${explorerUrl}/policy/${policyId}${mintUrl}`,
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
