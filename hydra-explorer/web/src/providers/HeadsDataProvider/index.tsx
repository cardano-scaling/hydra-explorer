"use client" // This is a client component 👈🏽

import { HeadState } from '@/app/model'
import useDataFetcher from '@/hooks/DataFetcher'
import React, { useContext, useMemo, useState } from 'react'
import { useVersionContext } from "@/providers/VersionProvider"
import { useNetworkContext } from "@/providers/NetworkProvider"

export interface HeadsDataService {
  heads: HeadState[],
  error: string | null
}

const HeadsDataContext: React.Context<HeadsDataService> =
  React.createContext({} as HeadsDataService)

export const useHeadsDataContext = () => {
  const context = useContext(HeadsDataContext)
  if (!context) {
    throw new Error("useHeadsDataContext must be used within a HeadsDataProvider")
  }
  return context
}

export const HeadsDataProvider: React.FC<any> = ({
  children
}) => {
  const [heads, setHeads] = useState<HeadState[]>([])
  const [error, setError] = useState<string | null>(null)

  const { currentVersion } = useVersionContext()
  const { currentNetwork, currentNetworkMagic } = useNetworkContext()

  useDataFetcher<HeadState[]>({
    url: `/heads`,
    setFetchedData: setHeads,
    setError,
  })

  const getHeads = useMemo(() => {
    return heads.filter(
      (head) => {
        const networkSelected = currentNetwork === "mainnet" ? "Mainnet" : "Testnet"
        return !(head.version === currentVersion &&
          head.network === networkSelected &&
          head.networkMagic === currentNetworkMagic)
      }
    )
  }, [heads, currentVersion, currentNetwork, currentNetworkMagic])

  return (
    <HeadsDataContext.Provider value={{ heads: getHeads, error: error }}>
      {children}
    </HeadsDataContext.Provider>
  )
}

export const useHeadsData = () => useContext(HeadsDataContext)
