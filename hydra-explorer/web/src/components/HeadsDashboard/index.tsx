import React, { useMemo } from "react"
import {
  useStore,
  selectTotalHeads,
  selectActiveHeads,
  selectTVLInAda,
  selectActiveTick,
} from "@/store/useStore"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import { formatNumber } from "@/utils"

interface StatTileProps {
  label: string
  value: React.ReactNode
  subInfo?: React.ReactNode
  isLoading?: boolean
  hasSubInfo?: boolean
}

const StatTile: React.FC<StatTileProps> = ({ label, value, subInfo, isLoading, hasSubInfo }) => {
  if (isLoading) {
    return (
      <div className="bg-card border border-border rounded-lg py-5 px-6 flex flex-col justify-between min-h-[96px] h-full">
        <div>
          {/* Label Skeleton */}
          <div className="h-3 w-16 bg-muted rounded-sm animate-skeleton-pulse" />
          {/* Value Skeleton */}
          <div className="h-8 w-24 bg-muted rounded-sm animate-skeleton-pulse mt-2" />
        </div>
        {(subInfo || hasSubInfo) && (
          <div className="h-4 w-32 bg-muted rounded-sm animate-skeleton-pulse mt-2" />
        )}
      </div>
    )
  }

  return (
    <div className="bg-card border border-border rounded-lg py-5 px-6 flex flex-col justify-between min-h-[96px] h-full transition-colors">
      <div>
        <span className="text-[12px] font-medium tracking-wider uppercase text-muted-foreground">
          {label}
        </span>
        <div className="text-[20px] md:text-[30px] font-semibold leading-[1.2] md:leading-[1.1] text-foreground mt-1">
          {value}
        </div>
      </div>
      {subInfo && (
        <div className="mt-2 text-[12px] text-muted-foreground leading-normal">{subInfo}</div>
      )}
    </div>
  )
}

const formatTVL = (valueInAda: number) => {
  if (valueInAda >= 999_999_995) {
    return `${formatNumber(valueInAda / 1_000_000_000, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}B`
  }
  if (valueInAda >= 999_995) {
    return `${formatNumber(valueInAda / 1_000_000, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}M`
  }
  if (valueInAda >= 999.995) {
    return `${formatNumber(valueInAda / 1_000, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}K`
  }
  return formatNumber(valueInAda, { minimumFractionDigits: 0, maximumFractionDigits: 2 })
}

const HeadsDashboard = () => {
  const headsError = useStore((state) => state.headsError)
  const ticksError = useStore((state) => state.ticksError)

  const headsLoading = useStore((state) => state.headsLoading)
  const ticksLoading = useStore((state) => state.ticksLoading)

  const explorer = useCardanoExplorer()

  const totalHeads = useStore(selectTotalHeads)
  const activeHeads = useStore(selectActiveHeads)
  const tvlInAda = useStore(selectTVLInAda)
  const activeTick = useStore(selectActiveTick)

  // Skeletons show on initial load (when array is empty and loading is true)
  const showHeadsLoading = headsLoading && totalHeads === 0
  const showTicksLoading = ticksLoading && !activeTick

  // Format display values
  const displayTotalHeads = headsError ? "—" : formatNumber(totalHeads)
  const displayActiveHeads = headsError ? "—" : formatNumber(activeHeads)
  const displayTVL = headsError ? "—" : formatTVL(tvlInAda)

  const displayBlockNo = useMemo(() => {
    if (ticksError || !activeTick) return "—"
    const blockNo = activeTick.blockNo
    const formattedBlockNo = blockNo != null ? `#${formatNumber(blockNo)}` : "—"
    const blockHash = activeTick.point?.blockHash
    if (blockHash) {
      return (
        <a
          href={explorer.block(blockHash)}
          target="_blank"
          rel="noopener noreferrer"
          className="hover:text-primary-vivid transition-colors"
        >
          {formattedBlockNo}
        </a>
      )
    }
    return formattedBlockNo
  }, [activeTick, ticksError, explorer])

  const subInfoLatestBlock = useMemo(() => {
    if (ticksError || !activeTick) return null

    const blockHash = activeTick.point?.blockHash
    const slot = activeTick.point?.slot
    const truncatedHash = blockHash ? `${blockHash.slice(0, 6)}…${blockHash.slice(-4)}` : "—"
    const displaySlot = slot !== undefined && slot !== null && slot !== "" ? slot : "—"

    return (
      <div className="flex flex-wrap gap-x-2 gap-y-1">
        <span>
          Slot: <span className="font-mono">{displaySlot}</span>
        </span>
        {blockHash && (
          <span className="flex items-center gap-1">
            Hash:{" "}
            <a
              href={explorer.block(blockHash)}
              target="_blank"
              rel="noopener noreferrer"
              className="text-data font-mono hover:text-primary-vivid transition-colors"
              title={blockHash}
            >
              {truncatedHash}
            </a>
          </span>
        )}
      </div>
    )
  }, [activeTick, ticksError, explorer])

  return (
    <div className="grid grid-cols-2 lg:grid-cols-4 gap-4 md:gap-6 w-full mb-6">
      <StatTile label="Total Heads" value={displayTotalHeads} isLoading={showHeadsLoading} />
      <StatTile label="Active Heads" value={displayActiveHeads} isLoading={showHeadsLoading} />
      <StatTile label="TVL (₳)" value={displayTVL} isLoading={showHeadsLoading} />
      <StatTile
        label="Latest Block"
        value={displayBlockNo}
        subInfo={
          subInfoLatestBlock ||
          (showTicksLoading ? undefined : (
            <span className="text-[12px] text-muted-foreground">—</span>
          ))
        }
        isLoading={showTicksLoading}
        hasSubInfo={true}
      />
    </div>
  )
}

export default HeadsDashboard
