"use client"

import React, { useEffect, useState, useRef, useMemo } from "react"
import { useRouter } from "next/navigation"
import { HeadState } from "@/app/model"
import { ReadonlyURLSearchParams } from "next/navigation"
import { Button } from "@/components/ui/button"
import { useStore, mainnetNetworkMagic } from "@/store/useStore"
import { truncateVersion } from "@/utils"
import { cn } from "@/lib/utils"
import { ChevronDown } from "lucide-react"
import StatusBadge from "@/components/StatusBadge"
import { Input } from "@/components/ui/input"
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover"

export interface FilterState {
  headId: string | null
  status: string | null
  version: string | null
  slot: string | null
  blockNo: string | null
  blockHash: string | null
}

export const filterStateFromUrl = (searchParams: ReadonlyURLSearchParams): FilterState => {
  const filters: FilterState = { ...emptyFilterState }
  searchParams.forEach((value, key) => {
    if (Object.prototype.hasOwnProperty.call(emptyFilterState, key)) {
      filters[key as keyof FilterState] = value
    }
  })
  return filters
}

export const emptyFilterState: FilterState = {
  headId: null,
  status: null,
  version: null,
  slot: null,
  blockNo: null,
  blockHash: null,
}

interface HeadsSelectProps {
  filters: FilterState
  setFilterState: React.Dispatch<React.SetStateAction<FilterState>>
  heads: HeadState[]
}

const getNetworkLabel = (magic: number): string => {
  if (magic === mainnetNetworkMagic) return "Mainnet"
  if (magic === 1) return "Preprod"
  if (magic === 2) return "Preview"
  return `Magic: ${magic}`
}

export const HeadsSelectTable: React.FC<HeadsSelectProps> = ({
  filters,
  setFilterState,
  heads,
}) => {
  const setPageIndex = useStore((state) => state.setPageIndex)
  const searchTerm = useStore((state) => state.searchTerm)
  const setSearchTerm = useStore((state) => state.setSearchTerm)
  const allHeads = useStore((state) => state.heads)
  const currentNetworkMagic = useStore((state) => state.currentNetworkMagic)
  const updateNetwork = useStore((state) => state.updateNetwork)

  const [activePopover, setActivePopover] = useState<"status" | "version" | null>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const [localValue, setLocalValue] = useState(searchTerm)
  const [inputFocused, setInputFocused] = useState(false)
  const [activeIndex, setActiveIndex] = useState(-1)
  const inputRef = useRef<HTMLInputElement>(null)
  const router = useRouter()

  const [prevSearchTerm, setPrevSearchTerm] = useState(searchTerm)
  if (searchTerm !== prevSearchTerm) {
    setPrevSearchTerm(searchTerm)
    setLocalValue(searchTerm)
  }

  // Debounce updating the store
  useEffect(() => {
    if (localValue === searchTerm) return
    const timer = setTimeout(() => {
      setSearchTerm(localValue)
    }, 300)

    return () => clearTimeout(timer)
  }, [localValue, searchTerm, setSearchTerm])

  // Match suggestion matching tiers: exact > prefix > contains
  const getMatchTier = (head: HeadState, query: string): number => {
    const q = query.toLowerCase().trim()
    if (!q) return 4

    const headId = (head?.headId || "").toLowerCase()
    const seed = head?.seedTxIn ? head.seedTxIn.toLowerCase() : ""
    const blockHash = head?.point?.blockHash ? head.point.blockHash.toLowerCase() : ""

    if (headId === q) {
      return 1 // Exact
    }
    if (headId.startsWith(q)) {
      return 2 // Prefix
    }
    if (headId.includes(q) || seed.includes(q) || blockHash.includes(q)) {
      return 3 // Contains
    }
    return 4 // No match
  }

  const matches = useMemo(() => {
    const query = localValue.trim()
    if (query.length < 3) return []

    return (allHeads || [])
      .map((head) => ({ head, tier: getMatchTier(head, query) }))
      .filter((item) => item.tier <= 3)
      .sort((a, b) => {
        if (a.tier !== b.tier) {
          return a.tier - b.tier
        }
        return (b.head.blockNo || 0) - (a.head.blockNo || 0)
      })
      .slice(0, 5)
      .map((item) => item.head)
  }, [allHeads, localValue])

  const [prevMatches, setPrevMatches] = useState(matches)
  if (matches !== prevMatches) {
    setPrevMatches(matches)
    setActiveIndex(-1)
  }

  const selectSuggestion = (head: HeadState) => {
    // Switch network if different
    if (head.networkMagic !== currentNetworkMagic) {
      updateNetwork(head.networkMagic)
    }

    // Navigate using URL parameters to sync layout
    const params = new URLSearchParams(window.location.search)
    params.set("network", head.networkMagic.toString())
    params.set("detail", head.headId)
    params.set("search", localValue.trim())
    params.delete("page") // Reset to page 1

    const newUrl = `${window.location.pathname}?${params.toString()}`
    router.replace(newUrl, { scroll: false })

    // Update search term in store
    setSearchTerm(localValue.trim())
    setInputFocused(false)
    inputRef.current?.blur()
  }

  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Escape") {
      e.preventDefault()
      setInputFocused(false)
      e.currentTarget.blur()
      return
    }

    if (e.key === "Enter") {
      e.preventDefault()
      if (showDropdown && activeIndex >= 0 && activeIndex < matches.length) {
        selectSuggestion(matches[activeIndex])
      } else {
        setSearchTerm(localValue)
        setInputFocused(false)
        e.currentTarget.blur()
      }
      return
    }

    if (matches.length === 0) return

    if (e.key === "ArrowDown") {
      e.preventDefault()
      setActiveIndex((prev) => (prev === matches.length - 1 ? 0 : prev + 1))
    } else if (e.key === "ArrowUp") {
      e.preventDefault()
      setActiveIndex((prev) => (prev <= 0 ? matches.length - 1 : prev - 1))
    }
  }

  const showDropdown = inputFocused && localValue.trim().length >= 3 && matches.length > 0

  const getVersions = () => {
    const seen = new Set<string>()
    const uniqueValues: string[] = []
    for (const head of heads) {
      const version = head.version
      if (version && !seen.has(version)) {
        seen.add(version)
        uniqueValues.push(version)
      }
    }
    return uniqueValues.sort()
  }

  const handleSelectStatus = (status: string | null) => {
    setFilterState((prev) => ({ ...prev, status }))
    setPageIndex(1)
    setActivePopover(null)
  }

  const handleSelectVersion = (version: string | null) => {
    setFilterState((prev) => ({ ...prev, version }))
    setPageIndex(1)
    setActivePopover(null)
  }

  const clearFilterState = () => {
    setFilterState(emptyFilterState)
    setSearchTerm("")
    setLocalValue("")
    setPageIndex(1)
    setActivePopover(null)
  }

  const statuses = ["Open", "Initializing", "Closed", "Finalized", "Aborted"]
  const versions = getVersions()
  const isAnyFilterActive = !!(filters.status || filters.version || searchTerm)

  return (
    <div ref={containerRef} className="mb-6 flex flex-col gap-3 w-full">
      {/* Search + Popover triggers */}
      <div className="flex flex-wrap items-center justify-end gap-3">
        {/* Search Input */}
        <div className="relative w-full sm:w-auto sm:max-w-[320px] order-first sm:order-0 sm:mr-auto">
          <Input
            ref={inputRef}
            id="header-search-input"
            type="text"
            placeholder="Search"
            value={localValue}
            onChange={(e) => setLocalValue(e.target.value)}
            onFocus={() => setInputFocused(true)}
            onBlur={() => setInputFocused(false)}
            onKeyDown={handleKeyDown}
            role="combobox"
            aria-expanded={showDropdown}
            aria-autocomplete="list"
            aria-controls={showDropdown ? "search-suggestions" : undefined}
            aria-activedescendant={activeIndex >= 0 ? `suggestion-item-${activeIndex}` : undefined}
            className="w-full bg-card focus-visible:ring-2 focus-visible:ring-primary-vivid focus-visible:border-transparent z-10 h-9"
          />

          {showDropdown && (
            <div
              id="search-suggestions"
              onMouseDown={(e) => e.preventDefault()}
              className="absolute top-full left-0 w-full min-w-[320px] bg-card border border-border rounded-md shadow-lg z-50 mt-1 overflow-hidden"
              role="listbox"
              aria-label="Search suggestions"
            >
              {matches.map((head, index) => {
                const isActive = index === activeIndex
                const truncatedId = `${head.headId.slice(0, 6)}…${head.headId.slice(-4)}`
                return (
                  <div
                    key={head.headId}
                    id={`suggestion-item-${index}`}
                    role="option"
                    aria-selected={isActive}
                    onMouseDown={(e) => {
                      // onMouseDown runs before onBlur and prevents focus loss
                      e.preventDefault()
                      selectSuggestion(head)
                    }}
                    className={cn(
                      "flex items-center justify-between px-4 py-2.5 cursor-pointer transition-all border-l-2 border-l-transparent text-foreground hover:bg-muted/50",
                      isActive && "bg-primary-muted border-l-primary-vivid hover:bg-primary-muted",
                    )}
                  >
                    <div className="flex items-center gap-2">
                      <span className="font-mono text-[13px] text-data leading-none">
                        {truncatedId}
                      </span>
                    </div>
                    <div className="flex items-center gap-2">
                      <span className="text-[11px] text-muted-foreground uppercase font-medium bg-muted px-1.5 py-0.5 rounded-sm">
                        {getNetworkLabel(head.networkMagic)}
                      </span>
                      <StatusBadge status={head.status} />
                    </div>
                  </div>
                )
              })}
            </div>
          )}
        </div>

        {/* Status Trigger */}
        <Popover
          open={activePopover === "status"}
          onOpenChange={(open) => setActivePopover(open ? "status" : null)}
        >
          <PopoverTrigger
            render={
              <Button
                variant="outline"
                aria-haspopup="listbox"
                aria-expanded={activePopover === "status"}
                className="flex items-center gap-2 border-border bg-background/50 hover:bg-accent text-foreground text-sm font-medium h-9 px-4 rounded-md animate-none"
              />
            }
          >
            <span>Status: {filters.status || "All"}</span>
            <ChevronDown className="w-4 h-4 text-muted-foreground" />
          </PopoverTrigger>
          <PopoverContent
            align="end"
            className="p-2 min-w-[180px] w-auto bg-popover border border-border shadow-lg"
          >
            <div>
              <label className="flex items-center gap-2 px-2 py-1.5 hover:bg-accent rounded-sm cursor-pointer text-sm font-medium text-foreground">
                <input
                  type="radio"
                  name="status-filter"
                  checked={filters.status === null}
                  onChange={() => handleSelectStatus(null)}
                  className="accent-[#4C82F0]"
                />
                <span>All</span>
              </label>
              {statuses.map((s) => (
                <label
                  key={s}
                  className="flex items-center gap-2 px-2 py-1.5 hover:bg-accent rounded-sm cursor-pointer text-sm font-medium text-foreground"
                >
                  <input
                    type="radio"
                    name="status-filter"
                    checked={filters.status === s}
                    onChange={() => handleSelectStatus(s)}
                    className="accent-[#4C82F0]"
                  />
                  <span>{s}</span>
                </label>
              ))}
            </div>
          </PopoverContent>
        </Popover>

        {/* Version Trigger */}
        <Popover
          open={activePopover === "version"}
          onOpenChange={(open) => setActivePopover(open ? "version" : null)}
        >
          <PopoverTrigger
            render={
              <Button
                variant="outline"
                aria-haspopup="listbox"
                aria-expanded={activePopover === "version"}
                className="flex items-center gap-2 border-border bg-background/50 hover:bg-accent text-foreground text-sm font-medium h-9 px-4 rounded-md animate-none"
              />
            }
          >
            <span title={filters.version || undefined}>
              Version: {filters.version ? truncateVersion(filters.version) : "All"}
            </span>
            <ChevronDown className="w-4 h-4 text-muted-foreground" />
          </PopoverTrigger>
          <PopoverContent
            align="end"
            className="p-2 min-w-[180px] w-auto max-h-[250px] overflow-y-auto bg-popover border border-border shadow-lg"
          >
            <div>
              <label className="flex items-center gap-2 px-2 py-1.5 hover:bg-accent rounded-sm cursor-pointer text-sm font-medium text-foreground">
                <input
                  type="radio"
                  name="version-filter"
                  checked={filters.version === null}
                  onChange={() => handleSelectVersion(null)}
                  className="accent-[#4C82F0]"
                />
                <span>All</span>
              </label>
              {versions.map((v) => (
                <label
                  key={v}
                  title={v}
                  className="flex items-center gap-2 px-2 py-1.5 hover:bg-accent rounded-sm cursor-pointer text-sm font-medium text-foreground"
                >
                  <input
                    type="radio"
                    name="version-filter"
                    checked={filters.version === v}
                    onChange={() => handleSelectVersion(v)}
                    className="accent-[#4C82F0]"
                  />
                  <span>{truncateVersion(v)}</span>
                </label>
              ))}
            </div>
          </PopoverContent>
        </Popover>

        {/* Clear All */}
        {isAnyFilterActive && (
          <button
            onClick={clearFilterState}
            className="text-muted-foreground hover:text-[#4C82F0] text-xs font-semibold transition-colors cursor-pointer px-1 h-9"
          >
            Clear All
          </button>
        )}
      </div>
    </div>
  )
}
