"use client"

import React, { useState, useEffect } from "react"
import { useStore } from "@/store/useStore"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import { formatNumber, formatRelativeDeadline } from "@/utils"
import HashDisplay from "@/components/HashDisplay"
import StatusBadge from "@/components/StatusBadge"
import MemberTable from "@/components/MemberTable"
import { Sheet, SheetContent, SheetHeader, SheetTitle, SheetBody } from "@/components/ui/sheet"

// ─── MetadataField ──────────────────────────────────────────────────────────
interface MetadataFieldProps {
  label: string
  children: React.ReactNode
  colSpan?: "full" | "half"
}

function MetadataField({ label, children, colSpan = "half" }: MetadataFieldProps) {
  return (
    <div
      className={[
        "border border-border p-4 rounded-[var(--radius-lg)] bg-background/40",
        colSpan === "full" ? "md:col-span-2" : "",
      ].join(" ")}
    >
      <dt className="text-[11px] font-medium uppercase tracking-wider text-muted-foreground mb-1.5">
        {label}
      </dt>
      <dd className="text-sm leading-relaxed">{children}</dd>
    </div>
  )
}

// ─── ContestationDeadlineCell ────────────────────────────────────────────────
interface ContestationDeadlineCellProps {
  timestampSeconds: number | null | undefined
}

function ContestationDeadlineCell({ timestampSeconds }: ContestationDeadlineCellProps) {
  const [tick, setTick] = useState(0)

  useEffect(() => {
    if (!timestampSeconds) return
    const id = setInterval(() => {
      setTick((t) => t + 1)
    }, 1000)
    return () => clearInterval(id)
  }, [timestampSeconds])

  const display = formatRelativeDeadline(timestampSeconds)

  if (!display) {
    return <span className="text-muted-foreground">—</span>
  }

  const isPast = display.relative.startsWith("expired")

  return (
    <span
      title={display.absoluteUtc}
      className={[
        "font-mono cursor-help",
        isPast ? "text-[var(--status-closed)]" : "text-foreground",
      ].join(" ")}
    >
      {display.relative}
    </span>
  )
}

// ─── HeadDetails ────────────────────────────────────────────────────────────
interface HeadDetailsProps {
  headId: string | null
  open: boolean
  onClose: () => void
}

const HeadDetails: React.FC<HeadDetailsProps> = ({ headId, open, onClose }) => {
  const heads = useStore((state) => state.heads)
  const isLoading = useStore((state) => state.headsLoading)
  const head = headId ? heads.find((h) => h.headId === headId) : null

  const [activeTab, setActiveTab] = useState<"metadata" | "members">("metadata")

  const [prevHeadId, setPrevHeadId] = useState(headId)
  const [prevOpen, setPrevOpen] = useState(open)

  if (headId !== prevHeadId || open !== prevOpen) {
    setPrevHeadId(headId)
    setPrevOpen(open)
    if (open) {
      setActiveTab("metadata")
    }
  }

  const explorer = useCardanoExplorer()

  // Format contestation period (seconds) → human-readable
  const formatContestationPeriod = (secs: number | null | undefined): string => {
    if (secs === null || secs === undefined) return "—"
    if (secs < 60) return `${formatNumber(secs)}s`
    if (secs < 3600) {
      const m = Math.floor(secs / 60)
      const s = secs % 60
      return s > 0 ? `${m}m ${s}s` : `${m}m`
    }
    const h = Math.floor(secs / 3600)
    const rem = secs % 3600
    const m = Math.floor(rem / 60)
    return m > 0 ? `${h}h ${m}m` : `${h}h`
  }

  // Truncated Head ID for panel title
  const truncatedHeadId = headId ? `${headId.slice(0, 8)}…${headId.slice(-6)}` : ""

  return (
    <Sheet
      open={open}
      onOpenChange={(isOpen) => {
        if (!isOpen) onClose()
      }}
    >
      <SheetContent aria-label={`Head detail: ${headId ?? ""}`}>
        {/* ── Panel Header ──────────────────────────────── */}
        <SheetHeader>
          <div className="flex flex-col gap-1 min-w-0 flex-1">
            <SheetTitle className="text-[11px] font-medium tracking-wider uppercase text-muted-foreground">
              Head Detail
            </SheetTitle>
            <p className="text-data truncate" title={headId ?? ""}>
              {truncatedHeadId}
            </p>
            {isLoading ? (
              <div className="flex items-center gap-2 mt-1">
                <div className="h-5 w-16 bg-muted/60 rounded animate-skeleton-pulse" />
                <div className="h-4 w-10 bg-muted/60 rounded animate-skeleton-pulse" />
              </div>
            ) : head ? (
              <div className="flex items-center gap-2 mt-1">
                {head.status ? (
                  <StatusBadge status={head.status} />
                ) : (
                  <span className="text-muted-foreground">—</span>
                )}
                <span className="text-[11px] text-muted-foreground font-mono">
                  v{head.version ?? "—"}
                </span>
              </div>
            ) : null}
          </div>
        </SheetHeader>

        {/* ── Panel Body ────────────────────────────────── */}
        <SheetBody>
          {isLoading ? (
            <div className="space-y-6">
              {/* Section 1: Core Details Skeleton */}
              <div>
                <div className="h-3.5 bg-muted/40 rounded w-24 mb-3 animate-skeleton-pulse" />
                <dl className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {/* 1. Head ID */}
                  <MetadataField label="Head ID">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-24"></div>
                  </MetadataField>

                  {/* 2. Status */}
                  <MetadataField label="Status">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-20"></div>
                  </MetadataField>

                  {/* 3. Seed TxIn */}
                  <MetadataField label="Seed TxIn">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-24"></div>
                  </MetadataField>

                  {/* 4. Version */}
                  <MetadataField label="Version">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-12"></div>
                  </MetadataField>
                </dl>
              </div>

              {/* Section 2: Contestation & Protocol Skeleton */}
              <div>
                <div className="h-3.5 bg-muted/40 rounded w-40 mb-3 animate-skeleton-pulse" />
                <dl className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {/* 5. Contestation Period */}
                  <MetadataField label="Contestation Period">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-16"></div>
                  </MetadataField>

                  {/* 6. Contestation Deadline */}
                  <MetadataField label="Contestation Deadline">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-28"></div>
                  </MetadataField>

                  {/* 7. Contestation Count */}
                  <MetadataField label="Contestation Count">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-10"></div>
                  </MetadataField>
                </dl>
              </div>

              {/* Section 3: Blockchain Point Skeleton */}
              <div>
                <div className="h-3.5 bg-muted/40 rounded w-32 mb-3 animate-skeleton-pulse" />
                <dl className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {/* 8. Snapshot Number */}
                  <MetadataField label="Snapshot Number">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-10"></div>
                  </MetadataField>

                  {/* 9. Slot */}
                  <MetadataField label="Slot">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-20"></div>
                  </MetadataField>

                  {/* 10. Block Hash */}
                  <MetadataField label="Block Hash">
                    <div className="h-5 bg-muted/60 rounded animate-skeleton-pulse w-24"></div>
                  </MetadataField>
                </dl>
              </div>
            </div>
          ) : !head ? (
            <p className="text-sm text-muted-foreground">Head not found.</p>
          ) : (
            <div className="flex flex-col h-full">
              {/* Tab Navigation */}
              <div className="flex border-b border-border mb-5 shrink-0">
                <button
                  onClick={() => setActiveTab("metadata")}
                  className={[
                    "px-4 py-2.5 -mb-px border-b-2 text-xs font-semibold uppercase tracking-wider transition-colors duration-150 cursor-pointer focus-visible:outline-none",
                    activeTab === "metadata"
                      ? "border-[#4C82F0] text-[#4C82F0]"
                      : "border-transparent text-muted-foreground hover:text-foreground",
                  ].join(" ")}
                >
                  Metadata
                </button>
                <button
                  onClick={() => setActiveTab("members")}
                  className={[
                    "px-4 py-2.5 -mb-px border-b-2 text-xs font-semibold uppercase tracking-wider transition-colors duration-150 cursor-pointer focus-visible:outline-none",
                    activeTab === "members"
                      ? "border-[#4C82F0] text-[#4C82F0]"
                      : "border-transparent text-muted-foreground hover:text-foreground",
                  ].join(" ")}
                >
                  Members ({head.members?.length ?? 0})
                </button>
              </div>

              {/* Tab Content Panels */}
              <div className="flex-1 min-h-0">
                {activeTab === "metadata" && (
                  <div className="space-y-6">
                    {/* Section 1: Core Details */}
                    <div>
                      <h3 className="text-[11px] font-semibold uppercase tracking-wider text-muted-foreground mb-3 px-1">
                        Core Details
                      </h3>
                      <dl className="grid grid-cols-1 md:grid-cols-2 gap-3">
                        {/* 1. Head ID */}
                        <MetadataField label="Head ID">
                          {head.headId ? (
                            <HashDisplay
                              value={head.headId}
                              href={explorer.mintPolicy(head.headId, head.networkMagic)}
                            />
                          ) : (
                            <span className="text-muted-foreground">—</span>
                          )}
                        </MetadataField>

                        {/* 2. Status */}
                        <MetadataField label="Status">
                          {head.status ? (
                            <StatusBadge status={head.status} />
                          ) : (
                            <span className="text-muted-foreground">—</span>
                          )}
                        </MetadataField>

                        {/* 3. Seed TxIn */}
                        <MetadataField label="Seed TxIn">
                          {head.seedTxIn ? (
                            <HashDisplay
                              value={head.seedTxIn}
                              href={explorer.tx(head.seedTxIn, head.networkMagic)}
                            />
                          ) : (
                            <span className="text-muted-foreground">—</span>
                          )}
                        </MetadataField>

                        {/* 4. Version */}
                        <MetadataField label="Version">
                          <span className="font-mono">{head.version ?? "—"}</span>
                        </MetadataField>
                      </dl>
                    </div>

                    {/* Section 2: Contestation & Protocol */}
                    <div>
                      <h3 className="text-[11px] font-semibold uppercase tracking-wider text-muted-foreground mb-3 px-1">
                        Contestation & Protocol
                      </h3>
                      <dl className="grid grid-cols-1 md:grid-cols-2 gap-3">
                        {/* 5. Contestation Period */}
                        <MetadataField label="Contestation Period">
                          <span className="font-mono">
                            {formatContestationPeriod(head.contestationPeriod)}
                          </span>
                        </MetadataField>

                        {/* 6. Contestation Deadline */}
                        <MetadataField label="Contestation Deadline">
                          <ContestationDeadlineCell timestampSeconds={head.contestationDeadline} />
                        </MetadataField>

                        {/* 7. Contestation Count */}
                        <MetadataField label="Contestation Count">
                          <span className="font-mono">{formatNumber(head.contestations)}</span>
                        </MetadataField>
                      </dl>
                    </div>

                    {/* Section 3: Blockchain Point */}
                    <div>
                      <h3 className="text-[11px] font-semibold uppercase tracking-wider text-muted-foreground mb-3 px-1">
                        Blockchain Point
                      </h3>
                      <dl className="grid grid-cols-1 md:grid-cols-2 gap-3">
                        {/* 8. Snapshot Number */}
                        <MetadataField label="Snapshot Number">
                          <span className="font-mono">{formatNumber(head.snapshotNumber)}</span>
                        </MetadataField>

                        {/* 9. Slot */}
                        <MetadataField label="Slot">
                          <span className="font-mono text-data">
                            {formatNumber(head.point.slot)}
                          </span>
                        </MetadataField>

                        {/* 10. Block Hash */}
                        <MetadataField label="Block Hash">
                          {head.point.blockHash ? (
                            <HashDisplay
                              value={head.point.blockHash}
                              href={explorer.block(head.point.blockHash, head.networkMagic)}
                            />
                          ) : (
                            <span className="text-muted-foreground">—</span>
                          )}
                        </MetadataField>
                      </dl>
                    </div>
                  </div>
                )}

                {activeTab === "members" && (
                  <div className="space-y-4">
                    <MemberTable members={head.members} networkMagic={head.networkMagic} />
                  </div>
                )}
              </div>
            </div>
          )}
        </SheetBody>
      </SheetContent>
    </Sheet>
  )
}

export default HeadDetails
