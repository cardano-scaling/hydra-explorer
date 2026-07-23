"use client"

import React from "react"
import { WifiOff } from "lucide-react"
import { useStore } from "@/store/useStore"

const OfflineBanner: React.FC = () => {
  const isOnline = useStore((state) => state.isOnline)

  if (isOnline) return null

  return (
    <div className="w-full bg-red-950/40 backdrop-blur-md border-b border-red-500/30 py-2.5 px-4 flex items-center justify-center gap-2.5 text-xs md:text-sm font-medium text-red-200 transition-all duration-300 z-50 sticky top-header-height">
      <div className="relative flex h-2 w-2">
        <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-red-400 opacity-75"></span>
        <span className="relative inline-flex rounded-full h-2 w-2 bg-red-500"></span>
      </div>
      <WifiOff size={15} className="text-red-400 shrink-0" />
      <span>Offline. Showing cached data and pausing automatic updates.</span>
    </div>
  )
}

export default OfflineBanner
