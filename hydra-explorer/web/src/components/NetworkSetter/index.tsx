"use client"

import React from "react"
import { useStore, mainnetNetworkMagic } from "@/store/useStore"
import { useRouter, useSearchParams, usePathname } from "next/navigation"
import { cn } from "@/lib/utils"
import { Button } from "@/components/ui/button"

const NetworkSetter: React.FC = () => {
  const currentNetworkMagic = useStore((state) => state.currentNetworkMagic)
  const updateNetwork = useStore((state) => state.updateNetwork)
  const router = useRouter()
  const searchParams = useSearchParams()
  const pathname = usePathname()

  const handleNetworkChange = (magic: number) => {
    updateNetwork(magic)

    const params = new URLSearchParams(searchParams.toString())
    params.set("network", magic.toString())
    params.delete("page") // Reset to page 1

    const newUrl = `${pathname}?${params.toString()}`
    router.replace(newUrl, { scroll: false })
  }

  const networks = [
    { name: "Mainnet", magic: mainnetNetworkMagic },
    { name: "Preprod", magic: 1 },
    { name: "Preview", magic: 2 },
  ]

  return (
    <div
      className="inline-flex rounded-md border border-border bg-input/30 p-0.5"
      role="group"
      aria-label="Network switcher"
    >
      {networks.map((net) => {
        const isActive = currentNetworkMagic === net.magic
        return (
          <Button
            key={net.magic}
            type="button"
            onClick={() => handleNetworkChange(net.magic)}
            aria-pressed={isActive}
            variant={isActive ? "secondary" : "ghost"}
            size="xs"
            className={cn(
              "px-3 py-1.5 text-xs font-medium rounded-md transition-all outline-none border-transparent",
              isActive
                ? "bg-primary-muted text-primary-vivid hover:bg-primary-muted/80"
                : "text-muted-foreground hover:bg-muted hover:text-foreground",
            )}
          >
            {net.name}
          </Button>
        )
      })}
    </div>
  )
}

export default NetworkSetter
