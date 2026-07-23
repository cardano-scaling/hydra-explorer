"use client"

import React, { useState, useMemo, useEffect, useCallback, useRef } from "react"
import { useSearchParams, useRouter, usePathname, ReadonlyURLSearchParams } from "next/navigation"
import { useStore, mainnetNetworkMagic } from "@/store/useStore"
import { totalLovelaceValueLocked, formatNumber, truncateVersion } from "@/utils"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import HeadDetails from "../HeadDetails"
import { HeadsSelectTable, FilterState, filterStateFromUrl } from "./select"
import { Button } from "@/components/ui/button"
import HashDisplay from "@/components/HashDisplay"
import StatusBadge from "@/components/StatusBadge"
import DoomBadge from "@/components/DoomBadge"
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table"
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select"
import {
  Pagination,
  PaginationContent,
  PaginationItem,
  PaginationNext,
  PaginationPrevious,
} from "@/components/ui/pagination"
import { cn } from "@/lib/utils"

const DOOM_HEAD_ID = "e1393f73096f03a2e127cdace1aad0d3332c158346d0b46efb5a9339"

const limitFromUrl = (searchParams: ReadonlyURLSearchParams): number => {
  const limit = searchParams.get("limit")
  const num = limit ? Number(limit) : 25
  return [25, 50, 100].includes(num) ? num : 25
}

const pageFromUrl = (searchParams: ReadonlyURLSearchParams): number => {
  const page = searchParams.get("page")
  const num = page ? Number(page) : 1
  return isNaN(num) ? 1 : Math.max(1, num)
}

const HeadsTable: React.FC = () => {
  const heads = useStore((state) => state.heads)
  const error = useStore((state) => state.headsError)
  const isLoading = useStore((state) => state.headsLoading)
  const currentNetworkMagic = useStore((state) => state.currentNetworkMagic)
  const updateNetwork = useStore((state) => state.updateNetwork)

  const currentPage = useStore((state) => state.pageIndex)
  const setCurrentPage = useStore((state) => state.setPageIndex)
  const searchTerm = useStore((state) => state.searchTerm)
  const setSearchTerm = useStore((state) => state.setSearchTerm)

  const searchParams = useSearchParams()
  const router = useRouter()
  const pathname = usePathname()

  const detailId = searchParams.get("detail")
  const [cachedDetailId, setCachedDetailId] = useState<string | null>(null)

  // Sync cache during render when detailId is present:
  if (detailId && detailId !== cachedDetailId) {
    setCachedDetailId(detailId)
  }

  const selectedHeadId = detailId || cachedDetailId
  const panelOpen = !!detailId

  useEffect(() => {
    if (!detailId) {
      const timer = setTimeout(() => {
        setCachedDetailId(null)
      }, 350)
      return () => clearTimeout(timer)
    }
  }, [detailId])

  const lastOpenedHeadIdRef = useRef<string | null>(null)
  const explorer = useCardanoExplorer()

  const [filters, setFilters] = useState<FilterState>(filterStateFromUrl(searchParams))
  const [pageSize, setPageSize] = useState<number>(limitFromUrl(searchParams))

  // Refs to read latest state in the sync effect without re-triggering it
  const currentNetworkMagicRef = useRef(currentNetworkMagic)
  const currentPageRef = useRef(currentPage)
  const pageSizeRef = useRef(pageSize)
  const searchTermRef = useRef(searchTerm)
  const filtersRef = useRef(filters)

  useEffect(() => {
    currentNetworkMagicRef.current = currentNetworkMagic
    currentPageRef.current = currentPage
    pageSizeRef.current = pageSize
    searchTermRef.current = searchTerm
    filtersRef.current = filters
  })

  const updateUrlParams = useCallback(
    (
      page: number,
      network: number,
      newFilters: FilterState,
      currentSearch: string,
      limit: number,
      detailId?: string | null,
    ) => {
      const currentSearchStr = typeof window !== "undefined" ? window.location.search : ""
      const params = new URLSearchParams(currentSearchStr)

      if (page > 1) {
        params.set("page", page.toString())
      } else {
        params.delete("page")
      }

      params.set("network", network.toString())

      if (limit !== 25) {
        params.set("limit", limit.toString())
      } else {
        params.delete("limit")
      }

      if (currentSearch) {
        params.set("search", currentSearch)
      } else {
        params.delete("search")
      }

      Object.entries(newFilters).forEach(([key, value]) => {
        if (key !== "headId") {
          if (value) params.set(key, value)
          else params.delete(key)
        }
      })

      if (detailId !== undefined) {
        if (detailId) {
          params.set("detail", detailId)
        } else {
          params.delete("detail")
        }
      }

      const newUrl = `${pathname}?${params.toString()}`
      const currentUrl = pathname + (searchParams.toString() ? "?" + searchParams.toString() : "")
      if (newUrl !== currentUrl) {
        router.replace(newUrl, { scroll: false })
      }
    },
    [pathname, router, searchParams],
  )

  // Sync URL parameters to React state (single effect for robust back/forward and deep link sync)
  useEffect(() => {
    const rawNetwork = searchParams.get("network")
    const urlNetwork = rawNetwork ? Number(rawNetwork) : mainnetNetworkMagic
    const validNetworks = [mainnetNetworkMagic, 1, 2]
    if (validNetworks.includes(urlNetwork) && urlNetwork !== currentNetworkMagicRef.current) {
      updateNetwork(urlNetwork)
    }
    const page = pageFromUrl(searchParams)
    if (page !== currentPageRef.current) {
      setCurrentPage(page)
    }
    const limit = limitFromUrl(searchParams)
    if (limit !== pageSizeRef.current) {
      setPageSize(limit)
    }
    const initialFilters = filterStateFromUrl(searchParams)
    const initialSearch = searchParams.get("search") || initialFilters.headId || ""
    if (initialSearch !== searchTermRef.current) {
      setSearchTerm(initialSearch)
    }
    const targetFilters = { ...initialFilters, headId: null }
    if (JSON.stringify(filtersRef.current) !== JSON.stringify(targetFilters)) {
      setFilters(targetFilters)
    }
  }, [searchParams, updateNetwork, setCurrentPage, setSearchTerm])

  // Return focus to triggering row on close
  useEffect(() => {
    if (!panelOpen && lastOpenedHeadIdRef.current) {
      const row = document.getElementById(`head-row-${lastOpenedHeadIdRef.current}`)
      if (row) {
        row.focus()
      } else {
        // Fallback to search input to prevent focus loss if row is gone
        const searchInput = document.getElementById("header-search-input") as HTMLInputElement
        if (searchInput) {
          searchInput.focus()
        }
      }
      lastOpenedHeadIdRef.current = null
    }
  }, [panelOpen])

  const filteredNetworkHeads = useMemo(() => {
    return heads?.filter((head) => head.networkMagic === currentNetworkMagic) || []
  }, [heads, currentNetworkMagic])

  const filteredHeads = useMemo(() => {
    return filteredNetworkHeads.filter((head) => {
      const matchesSearch =
        !searchTerm ||
        head.headId.toLowerCase().includes(searchTerm.toLowerCase()) ||
        (head.seedTxIn && head.seedTxIn.toLowerCase().includes(searchTerm.toLowerCase())) ||
        head.point.blockHash.toLowerCase().includes(searchTerm.toLowerCase())

      return (
        matchesSearch &&
        (!filters.status || head.status === filters.status) &&
        (!filters.version || head.version === filters.version) &&
        (!filters.slot || head.point.slot.toString() === filters.slot) &&
        (!filters.blockNo || head.blockNo.toString() === filters.blockNo) &&
        (!filters.blockHash || head.point.blockHash === filters.blockHash)
      )
    })
  }, [filteredNetworkHeads, filters, searchTerm])

  const paginatedHeads = useMemo(() => {
    if (!filteredHeads) return []
    const start = (currentPage - 1) * pageSize
    const end = currentPage * pageSize
    return filteredHeads.slice(start, end)
  }, [filteredHeads, currentPage, pageSize])

  const totalPages = useMemo(() => {
    return filteredHeads?.length ? Math.ceil(filteredHeads.length / pageSize) : 1
  }, [filteredHeads, pageSize])

  const previousPage = () => setCurrentPage(Math.max(currentPage - 1, 1))
  const nextPage = () => setCurrentPage(Math.min(currentPage + 1, totalPages))

  // Write parameters to URL when filters or page state changes (isLoading check is preserved for initial load safety)
  useEffect(() => {
    if (!isLoading) {
      if (currentPage > totalPages) {
        setCurrentPage(1)
      }
      updateUrlParams(currentPage, currentNetworkMagic, filters, searchTerm, pageSize)
    }
  }, [
    currentPage,
    currentNetworkMagic,
    filters,
    searchTerm,
    isLoading,
    totalPages,
    pageSize,
    updateUrlParams,
    setCurrentPage,
  ])

  const totalHeads = filteredHeads?.length || 0
  const startIndex = totalHeads === 0 ? 0 : (currentPage - 1) * pageSize + 1
  const endIndex = Math.min(currentPage * pageSize, totalHeads)

  return (
    <div className="container mx-auto mt-12 text-foreground">
      {error ? (
        <p className="text-red-500">{error}</p>
      ) : (
        <>
          <div className="sticky top-14 bg-background z-20">
            <HeadsSelectTable
              filters={filters}
              setFilterState={setFilters}
              heads={filteredNetworkHeads}
            />
          </div>
          <div className="relative bg-card border border-border rounded-lg overflow-hidden">
            <Table
              className="w-full border-collapse table-fixed"
              containerClassName="max-h-[500px] overflow-y-auto overflow-x-auto w-full"
            >
              <TableHeader className="sticky top-0 bg-muted text-muted-foreground uppercase text-[12px] tracking-wider border-b border-border z-10">
                <TableRow className="hover:bg-transparent">
                  <TableHead className="w-[5%] px-4 py-3 text-center">#</TableHead>
                  <TableHead className="w-[17%] px-4 py-3 text-center">Head ID</TableHead>
                  <TableHead className="w-[8%] px-4 py-3 text-center">Version</TableHead>
                  <TableHead className="w-[10%] px-4 py-3 text-center">Status</TableHead>
                  <TableHead className="hidden md:table-cell w-[12%] px-4 py-3 text-center">
                    Slot
                  </TableHead>
                  <TableHead className="hidden md:table-cell w-[12%] px-4 py-3 text-center">
                    Block No
                  </TableHead>
                  <TableHead className="hidden md:table-cell w-[16%] px-4 py-3 text-center">
                    Block Hash
                  </TableHead>
                  <TableHead className="hidden md:table-cell w-[10%] px-4 py-3 text-center">
                    TVL (₳)
                  </TableHead>
                  <TableHead className="w-[10%] px-4 py-3 text-center">Action</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {isLoading ? (
                  Array.from({ length: 8 }).map((_, index) => (
                    <TableRow key={`skeleton-${index}`} className="border-b border-border">
                      <TableCell className="truncate text-center border px-4 py-2">
                        <div className="h-4 bg-muted/60 rounded animate-skeleton-pulse mx-auto w-6"></div>
                      </TableCell>
                      <TableCell className="truncate text-center border px-4 py-2">
                        <div className="h-4 bg-[#8DA3C4]/20 rounded animate-skeleton-pulse mx-auto w-28"></div>
                      </TableCell>
                      <TableCell className="truncate text-center border px-4 py-2">
                        <div className="h-4 bg-muted/60 rounded animate-skeleton-pulse mx-auto w-10"></div>
                      </TableCell>
                      <TableCell className="truncate text-center border px-4 py-2">
                        <div className="h-6 bg-muted/60 rounded-full animate-skeleton-pulse mx-auto w-16"></div>
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2">
                        <div className="h-4 bg-muted/60 rounded animate-skeleton-pulse mx-auto w-16"></div>
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2">
                        <div className="h-4 bg-muted/60 rounded animate-skeleton-pulse mx-auto w-16"></div>
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2">
                        <div className="h-4 bg-[#8DA3C4]/20 rounded animate-skeleton-pulse mx-auto w-24"></div>
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2">
                        <div className="h-4 bg-muted/60 rounded animate-skeleton-pulse mx-auto w-16"></div>
                      </TableCell>
                      <TableCell className="text-center border px-4 py-2">
                        <div className="h-8 bg-muted/60 rounded animate-skeleton-pulse mx-auto w-14"></div>
                      </TableCell>
                    </TableRow>
                  ))
                ) : paginatedHeads.length === 0 ? (
                  <TableRow>
                    <TableCell
                      colSpan={9}
                      className="text-center py-8 text-muted-foreground text-sm font-medium"
                    >
                      No heads found matching the active filters.
                    </TableCell>
                  </TableRow>
                ) : (
                  paginatedHeads.map((head, rowIndex) => (
                    <TableRow
                      key={head.headId}
                      id={`head-row-${head.headId}`}
                      tabIndex={0}
                      className="hover:bg-accent/50 cursor-pointer transition-colors duration-150 border-b border-border focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-[#4C82F0] focus-visible:z-10"
                      onClick={() => {
                        lastOpenedHeadIdRef.current = head.headId
                        updateUrlParams(
                          currentPage,
                          currentNetworkMagic,
                          filters,
                          searchTerm,
                          pageSize,
                          head.headId,
                        )
                      }}
                      onKeyDown={(e) => {
                        if (e.key === "Enter" || e.key === " ") {
                          e.preventDefault()
                          lastOpenedHeadIdRef.current = head.headId
                          updateUrlParams(
                            currentPage,
                            currentNetworkMagic,
                            filters,
                            searchTerm,
                            pageSize,
                            head.headId,
                          )
                        }
                      }}
                    >
                      <TableCell className="truncate text-center border px-4 py-2 text-sm text-muted-foreground">
                        {startIndex + rowIndex}
                      </TableCell>
                      <TableCell className="truncate text-center border px-4 py-2">
                        <div className="flex flex-col items-center justify-center">
                          <div className="flex items-center justify-center">
                            {head.headId === DOOM_HEAD_ID && (
                              <div className="mr-2">
                                <DoomBadge />
                              </div>
                            )}
                            <HashDisplay
                              value={head.headId}
                              href={explorer.mintPolicy(head.headId, head.networkMagic)}
                            />
                          </div>
                          <div className="block md:hidden mt-1 text-xs text-muted-foreground whitespace-nowrap">
                            TVL:{" "}
                            {formatNumber(totalLovelaceValueLocked(head) / 1000000, {
                              minimumFractionDigits: 2,
                              maximumFractionDigits: 2,
                            })}{" "}
                            ₳
                          </div>
                        </div>
                      </TableCell>
                      <TableCell
                        className="truncate text-center border px-4 py-2 text-sm"
                        title={head.version}
                      >
                        {truncateVersion(head.version)}
                      </TableCell>
                      <TableCell className="truncate text-center border px-4 py-2 text-sm">
                        <StatusBadge status={head.status} />
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2 text-sm">
                        {formatNumber(head.point.slot)}
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2 text-sm">
                        {formatNumber(head.blockNo)}
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2">
                        <HashDisplay
                          value={head.point.blockHash}
                          href={explorer.block(head.point.blockHash, head.networkMagic)}
                        />
                      </TableCell>
                      <TableCell className="hidden md:table-cell truncate text-center border px-4 py-2 text-sm">
                        {formatNumber(totalLovelaceValueLocked(head) / 1000000, {
                          minimumFractionDigits: 2,
                          maximumFractionDigits: 2,
                        })}{" "}
                        ₳
                      </TableCell>
                      <TableCell className="text-center border px-4 py-2">
                        <Button
                          variant="outline"
                          size="sm"
                          onClick={(e) => {
                            e.stopPropagation()
                            lastOpenedHeadIdRef.current = head.headId
                            updateUrlParams(
                              currentPage,
                              currentNetworkMagic,
                              filters,
                              searchTerm,
                              pageSize,
                              head.headId,
                            )
                          }}
                        >
                          View
                        </Button>
                      </TableCell>
                    </TableRow>
                  ))
                )}
              </TableBody>
            </Table>
          </div>

          <div className="mt-6 flex flex-col sm:flex-row justify-between items-center gap-4 text-foreground bg-card/30 border border-border/50 p-4 rounded-lg">
            <div className="text-sm text-muted-foreground font-medium">
              Showing {startIndex}–{endIndex} of {totalHeads} heads
            </div>
            <div className="flex flex-wrap items-center gap-4">
              <div className="flex items-center gap-2">
                <span className="text-xs text-muted-foreground font-medium">Show</span>
                <Select
                  value={String(pageSize)}
                  onValueChange={(val) => {
                    const newSize = Number(val)
                    setPageSize(newSize)
                    setCurrentPage(1)
                  }}
                >
                  <SelectTrigger className="w-[70px] h-8 text-xs border-border bg-background">
                    <SelectValue placeholder="25" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="25">25</SelectItem>
                    <SelectItem value="50">50</SelectItem>
                    <SelectItem value="100">100</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div className="flex items-center gap-2">
                <Pagination>
                  <PaginationContent>
                    <PaginationItem>
                      <PaginationPrevious
                        onClick={previousPage}
                        className={cn(
                          "cursor-pointer text-xs h-8 px-3 border border-border bg-background",
                          currentPage === 1 || isLoading ? "pointer-events-none opacity-50" : "",
                        )}
                      />
                    </PaginationItem>
                    <PaginationItem>
                      <span className="text-xs font-semibold text-muted-foreground select-none px-2">
                        Page {currentPage} of {totalPages}
                      </span>
                    </PaginationItem>
                    <PaginationItem>
                      <PaginationNext
                        onClick={nextPage}
                        className={cn(
                          "cursor-pointer text-xs h-8 px-3 border border-border bg-background",
                          currentPage === totalPages || isLoading
                            ? "pointer-events-none opacity-50"
                            : "",
                        )}
                      />
                    </PaginationItem>
                  </PaginationContent>
                </Pagination>
              </div>
            </div>
          </div>
        </>
      )}
      {selectedHeadId && (
        <HeadDetails
          headId={selectedHeadId}
          open={panelOpen}
          onClose={() => {
            updateUrlParams(currentPage, currentNetworkMagic, filters, searchTerm, pageSize, null)
          }}
        />
      )}
    </div>
  )
}

export default HeadsTable
