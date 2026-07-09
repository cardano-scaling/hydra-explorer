"use client"

import { HeadState } from '@/app/model'
import useDataFetcher from '@/hooks/DataFetcher'
import React, { useContext, useMemo, useState } from 'react'
import { useNetworkContext } from "@/providers/NetworkProvider"

export interface HeadsDataService {
  heads: HeadState[]
  error: string | null
  isLoading: boolean
}

const HeadsDataContext: React.Context<HeadsDataService> =
  React.createContext({} as HeadsDataService)

export const useHeadsData = () => {
  const context = useContext(HeadsDataContext)
  if (!context) {
    throw new Error("useHeadsData must be used within a HeadsDataProvider")
  }
  return context
}

export const HeadsDataProvider: React.FC<{ children: React.ReactNode }> = ({
  children
}) => {
  const [heads, setHeads] = useState<HeadState[]>([])
  const [error, setError] = useState<string | null>(null)
  const [isLoading, setIsLoading] = useState(true)

  const { currentNetworkMagic } = useNetworkContext()

  const url = process.env.NEXT_PUBLIC_EXPLORER_URL
    ? `${process.env.NEXT_PUBLIC_EXPLORER_URL}/heads`
    : `/heads`

  useDataFetcher<HeadState[]>({
    url,
    setFetchedData: (data) => {
      setHeads(data)
      setIsLoading(false)
    },
    setError: (err) => {
      setError(err)
      setIsLoading(false)
    },
  })

  const filteredHeads = useMemo(() => {
    return heads.filter((head) => head.networkMagic === currentNetworkMagic)
  }, [heads, currentNetworkMagic])

  return (
    <HeadsDataContext.Provider value={{ heads: filteredHeads, error, isLoading }}>
      {children}
    </HeadsDataContext.Provider>
  )
}
