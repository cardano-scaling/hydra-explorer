"use client" // This is a client component 👈🏽

import { useStore, mainnetNetworkMagic } from "@/store/useStore"
import React, { PropsWithChildren, useContext } from "react"

export interface CardanoExplorer {
  mintPolicy: (policyId: string, networkMagic?: number) => string
  tx: (txIn: string, networkMagic?: number) => string
  block: (blockHash: string, networkMagic?: number) => string
  address: (addr: string, networkMagic?: number) => string
}

const CardanoExplorerContext = React.createContext<CardanoExplorer | null>(null)

export const useCardanoExplorer = () => {
  const context = useContext(CardanoExplorerContext)
  if (!context) {
    throw new Error("useCardanoExplorer must be used within a CardanoExplorerProvider")
  }
  return context
}

export type CardanoExplorerProps = {}

export const CardanoExplorerProvider: React.FC<PropsWithChildren<CardanoExplorerProps>> = ({
  children,
}) => {
  const currentNetworkMagic = useStore((state) => state.currentNetworkMagic)

  const getExplorerUrlAndMintUrl = React.useCallback(
    (magic: number = currentNetworkMagic) => {
      let explorerUrl: string
      let mintUrl: string
      switch (magic) {
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
          explorerUrl = "cexplorer.io"
          mintUrl = "/mint"
      }
      return { explorerUrl, mintUrl }
    },
    [currentNetworkMagic],
  )

  const cexplorer = React.useMemo<CardanoExplorer>(
    () => ({
      mintPolicy: (policyId: string, magic?: number) => {
        const { explorerUrl, mintUrl } = getExplorerUrlAndMintUrl(magic)
        return `https://${explorerUrl}/policy/${policyId}${mintUrl}`
      },
      tx: (txIn: string, magic?: number) => {
        const { explorerUrl } = getExplorerUrlAndMintUrl(magic)
        return `https://${explorerUrl}/tx/${txIn}`
      },
      block: (blockHash: string, magic?: number) => {
        const { explorerUrl } = getExplorerUrlAndMintUrl(magic)
        return `https://${explorerUrl}/block/${blockHash}`
      },
      address: (addr: string, magic?: number) => {
        const { explorerUrl } = getExplorerUrlAndMintUrl(magic)
        return `https://${explorerUrl}/address/${addr}`
      },
    }),
    [getExplorerUrlAndMintUrl],
  )

  return (
    <CardanoExplorerContext.Provider value={cexplorer}>{children}</CardanoExplorerContext.Provider>
  )
}
