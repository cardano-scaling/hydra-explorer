"use client" // This is a client component 👈🏽

import { HeadState } from '@/app/model'
import useDataFetcher from '@/hooks/DataFetcher'
import React, { useContext, useMemo, useState } from 'react'
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

  const { currentNetworkMagic } = useNetworkContext()

  const url = process.env.NEXT_PUBLIC_EXPLORER_URL
    ? `${process.env.NEXT_PUBLIC_EXPLORER_URL}/heads`
    : `/heads`

  useDataFetcher<HeadState[]>({
    url,
    setFetchedData: setHeads,
    setError,
  })

  const getHeads = useMemo(() => {
    return heads.filter(
      (head) => {
        return head.networkMagic === currentNetworkMagic
      }
    )
  }, [heads, currentNetworkMagic])

  return (
    <HeadsDataContext.Provider value={{ heads: getHeads, error: error }}>
      {children}
    </HeadsDataContext.Provider>
  )
}

export const useHeadsData = () => useContext(HeadsDataContext)
