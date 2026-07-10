"use client"

import React, { useState } from "react"
import { HeadMember, UTxOEntry } from "@/app/model"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import { formatNumber } from "@/utils"
import HashDisplay from "@/components/HashDisplay"
import { ChevronDown, ChevronRight } from "lucide-react"
import { cn } from "@/lib/utils"
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table"

// ─── Helpers ────────────────────────────────────────────────────────────────

/**
 * Task 2: Sum lovelace value across all commits for a member.
 * Returns 0 if commits is null or empty.
 */
export function sumMemberLovelace(member: HeadMember): number {
  if (!member.commits) return 0
  return Object.values(member.commits).reduce(
    (total, commit) => total + (commit.value?.lovelace ?? 0),
    0,
  )
}

/**
 * Task 3: Parse TxIn "hash#index" into parts.
 * Returns { txHash, txIndex } — txHash is linked; txIndex rendered as text.
 */
function parseTxIn(txIn: string): { txHash: string; txIndex: string } {
  const hashIdx = txIn.lastIndexOf("#")
  if (hashIdx === -1) return { txHash: txIn, txIndex: "" }
  return {
    txHash: txIn.slice(0, hashIdx),
    txIndex: txIn.slice(hashIdx), // includes '#', e.g. "#0"
  }
}

// ─── CommitsSubTable ─────────────────────────────────────────────────────────
// Inline sub-table for a single member's commits (expanded row).
interface CommitsSubTableProps {
  commits: Record<string, UTxOEntry>
  networkMagic?: number
}

function CommitsSubTable({ commits, networkMagic }: CommitsSubTableProps) {
  const explorer = useCardanoExplorer()
  const entries = Object.entries(commits)

  if (entries.length === 0) {
    return <p className="text-sm text-muted-foreground px-4 py-3">No commits found.</p>
  }

  return (
    <Table className="w-full text-sm">
      <TableHeader>
        <TableRow className="bg-background text-muted-foreground uppercase text-[10px] tracking-wider border-b border-border hover:bg-transparent">
          <TableHead className="text-left px-4 py-2">TxIn</TableHead>
          <TableHead className="text-left px-4 py-2">Address</TableHead>
          <TableHead className="text-right px-4 py-2">Value (₳)</TableHead>
        </TableRow>
      </TableHeader>
      <TableBody>
        {entries.map(([txIn, commit]) => {
          const { txHash, txIndex } = parseTxIn(txIn)
          return (
            <TableRow
              key={txIn}
              className="border-b border-border last:border-b-0 hover:bg-accent/30 transition-colors"
            >
              {/* TxIn: link hash → Cexplorer tx, render #index as plain text */}
              <TableCell className="px-4 py-2 max-w-[200px]">
                <span className="inline-flex items-center gap-0.5 flex-wrap">
                  <HashDisplay value={txHash} href={explorer.tx(txHash, networkMagic)} />
                  {txIndex && (
                    <span className="font-mono text-[11px] text-muted-foreground">{txIndex}</span>
                  )}
                </span>
              </TableCell>

              {/* Address: truncated + copy */}
              <TableCell className="px-4 py-2 max-w-[180px]">
                {commit.address ? (
                  <HashDisplay
                    value={commit.address}
                    href={explorer.address(commit.address, networkMagic)}
                  />
                ) : (
                  <span className="text-muted-foreground">—</span>
                )}
              </TableCell>

              {/* Value */}
              <TableCell className="px-4 py-2 text-right font-mono tabular-nums">
                {formatNumber((commit.value?.lovelace ?? 0) / 1_000_000, {
                  minimumFractionDigits: 2,
                  maximumFractionDigits: 6,
                })}
              </TableCell>
            </TableRow>
          )
        })}
      </TableBody>
    </Table>
  )
}

// ─── MemberTable ─────────────────────────────────────────────────────────────
interface MemberTableProps {
  members: HeadMember[] | null | undefined
  networkMagic?: number
}

const MemberTable: React.FC<MemberTableProps> = ({ members, networkMagic }) => {
  // Track which row indices are expanded
  const [expandedRows, setExpandedRows] = useState<Set<number>>(new Set())

  const toggleRow = (index: number) => {
    setExpandedRows((prev) => {
      const next = new Set(prev)
      if (next.has(index)) {
        next.delete(index)
      } else {
        next.add(index)
      }
      return next
    })
  }

  const rows = members ?? []

  if (rows.length === 0) {
    return (
      <div className="border border-border rounded-[var(--radius-lg)] overflow-hidden">
        <p className="text-center text-sm text-muted-foreground py-6">—</p>
      </div>
    )
  }

  return (
    <div className="border border-border rounded-[var(--radius-lg)] overflow-hidden">
      <Table className="w-full">
        {/* ── Header ──────────────────────────────────── */}
        <TableHeader className="bg-muted text-muted-foreground uppercase text-[10px] tracking-wider border-b border-border">
          <TableRow className="hover:bg-transparent">
            {/* expand toggle column */}
            <TableHead className="w-8 px-3 py-2" aria-label="Expand" />
            <TableHead className="text-left px-4 py-2">On-Chain ID</TableHead>
            <TableHead className="text-left px-4 py-2">VKey</TableHead>
            <TableHead className="text-right px-4 py-2 font-medium">Total Committed (₳)</TableHead>
          </TableRow>
        </TableHeader>

        <TableBody>
          {rows.map((member, index) => {
            const isExpanded = expandedRows.has(index)
            const totalAda = sumMemberLovelace(member) / 1_000_000
            const hasCommits = member.commits && Object.keys(member.commits).length > 0

            const rowKey = member.party?.vkey || member.onChainId || index.toString()

            return (
              <React.Fragment key={rowKey}>
                {/* ── Member Row ────────────────────────── */}
                <TableRow
                  className={cn(
                    "border-b border-border",
                    hasCommits
                      ? "cursor-pointer hover:bg-accent/50 transition-colors duration-150"
                      : "cursor-default",
                    isExpanded ? "bg-accent/30" : "",
                  )}
                  onClick={() => hasCommits && toggleRow(index)}
                  aria-expanded={isExpanded}
                >
                  {/* Expand chevron */}
                  <TableCell className="w-8 px-3 py-2 text-muted-foreground">
                    {hasCommits ? (
                      isExpanded ? (
                        <ChevronDown size={14} aria-hidden />
                      ) : (
                        <ChevronRight size={14} aria-hidden />
                      )
                    ) : null}
                  </TableCell>

                  {/* On-Chain ID */}
                  <TableCell
                    className="px-4 py-2 max-w-[140px]"
                    onClick={(e) => e.stopPropagation()}
                  >
                    {member.onChainId ? (
                      <HashDisplay value={member.onChainId} />
                    ) : (
                      <span className="text-muted-foreground text-sm">—</span>
                    )}
                  </TableCell>

                  {/* VKey (truncated + copy icon via HashDisplay) */}
                  <TableCell
                    className="px-4 py-2 max-w-[140px]"
                    onClick={(e) => e.stopPropagation()}
                  >
                    {member.party?.vkey ? (
                      <HashDisplay value={member.party.vkey} />
                    ) : (
                      <span className="text-muted-foreground text-sm">—</span>
                    )}
                  </TableCell>

                  {/* Total Committed — Task 2 sum */}
                  <TableCell className="px-4 py-2 text-right font-mono text-sm tabular-nums">
                    {formatNumber(totalAda, {
                      minimumFractionDigits: 0,
                      maximumFractionDigits: 2,
                    })}
                  </TableCell>
                </TableRow>

                {/* ── Commits Sub-Table (expanded) ──────── */}
                {isExpanded && hasCommits && (
                  <TableRow className="bg-background/60 border-b border-border">
                    <TableCell colSpan={4} className="p-0">
                      <div className="border-t border-border/60">
                        <CommitsSubTable commits={member.commits!} networkMagic={networkMagic} />
                      </div>
                    </TableCell>
                  </TableRow>
                )}

                {/* Empty commits state */}
                {isExpanded && !hasCommits && (
                  <TableRow className="bg-background/60 border-b border-border">
                    <TableCell colSpan={4} className="px-8 py-3">
                      <p className="text-sm text-muted-foreground">No commits.</p>
                    </TableCell>
                  </TableRow>
                )}
              </React.Fragment>
            )
          })}
        </TableBody>
      </Table>
    </div>
  )
}

export default MemberTable
