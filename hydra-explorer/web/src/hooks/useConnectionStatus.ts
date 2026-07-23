"use client"

import { useEffect } from "react"
import { useStore } from "@/store/useStore"

export function useConnectionStatus() {
  const setIsOnline = useStore((state) => state.setIsOnline)
  const isOnline = useStore((state) => state.isOnline)

  useEffect(() => {
    if (typeof window === "undefined") return

    const handleOnline = () => {
      setIsOnline(true)
    }
    const handleOffline = () => {
      setIsOnline(false)
    }

    window.addEventListener("online", handleOnline)
    window.addEventListener("offline", handleOffline)

    // Đồng bộ trạng thái mạng ban đầu khi mount
    setIsOnline(window.navigator.onLine ?? true)

    return () => {
      window.removeEventListener("online", handleOnline)
      window.removeEventListener("offline", handleOffline)
    }
  }, [setIsOnline])

  return isOnline
}
