"use client"

import IntervalSetter from "@/components/IntervalSetter"
import HeadsDashboard from "@/components/HeadsDashboard"
import { CardanoExplorerProvider } from "@/providers/CardanoExplorer"
import Layout from "@/components/Layout"
import { usePollingEngine } from "@/hooks/usePollingEngine"

import dynamic from "next/dynamic"
const HeadsTable = dynamic(() => import("@/components/HeadsTable"), { ssr: false })
const NetworkSetter = dynamic(() => import("@/components/NetworkSetter"), { ssr: false })

export default function Home() {
  // Chạy ngầm bộ polling để fetch dữ liệu từ API
  usePollingEngine()

  return (
    <CardanoExplorerProvider>
      <Layout
        headerRightSlot={
          <>
            <NetworkSetter />
            {/* <IntervalSetter /> */}
          </>
        }
      >
        <HeadsDashboard />
        <HeadsTable />
      </Layout>
    </CardanoExplorerProvider>
  )
}
