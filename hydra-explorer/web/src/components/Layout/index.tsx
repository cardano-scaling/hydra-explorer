"use client"

import React from "react"
import Header from "@/components/Header"
import Footer from "@/components/Footer"
import OfflineBanner from "@/components/OfflineBanner"
import { useConnectionStatus } from "@/hooks/useConnectionStatus"

interface LayoutProps {
  headerRightSlot?: React.ReactNode
  children: React.ReactNode
}

const Layout: React.FC<LayoutProps> = ({ headerRightSlot, children }) => {
  // Kích hoạt theo dõi kết nối mạng
  useConnectionStatus()

  return (
    <div className="flex min-h-screen flex-col bg-background">
      <Header rightSlot={headerRightSlot} />
      <OfflineBanner />
      <main className="mx-auto w-full max-w-content-max-width flex-1 px-4 py-8 md:px-6">
        {children}
      </main>
      <Footer />
    </div>
  )
}

export default Layout
