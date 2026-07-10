"use client"

import React, { useState, useMemo, useEffect, useCallback } from "react"
import { useSearchParams, useRouter, usePathname, ReadonlyURLSearchParams } from "next/navigation"
import Image from "next/image"
import { HeadState } from "@/app/model"
import { useStore, mainnetNetworkMagic } from "@/store/useStore"
import { totalLovelaceValueLocked, formatNumber } from "@/utils"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import HeadDetails from "../HeadDetails"
import { HeadsSelectTable, FilterState, filterStateFromUrl } from "./select"
import { Button } from "@/components/ui/button"

const DOOM_HEAD_ID = "e1393f73096f03a2e127cdace1aad0d3332c158346d0b46efb5a9339"

const ITEMS_PER_PAGE = Number(process.env.NEXT_PUBLIC_ITEMS_PER_PAGE) || 100

const pageFromUrl = (searchParams: ReadonlyURLSearchParams): number => {
    const page = searchParams.get("page")
    return page ? Math.max(1, Number(page)) : 1
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

    const [selectedHead, setSelectedHead] = useState<HeadState | null>(null)
    const explorer = useCardanoExplorer()
    const searchParams = useSearchParams()
    const router = useRouter()
    const pathname = usePathname()

    const [filters, setFilters] = useState<FilterState>(filterStateFromUrl(searchParams))

    useEffect(() => {
        const rawNetwork = searchParams.get("network")
        const urlNetwork = rawNetwork ? Number(rawNetwork) : mainnetNetworkMagic
        const validNetworks = [mainnetNetworkMagic, 1, 2]
        if (validNetworks.includes(urlNetwork) && urlNetwork !== currentNetworkMagic) {
            updateNetwork(urlNetwork)
        }
        const page = pageFromUrl(searchParams)
        if (page !== currentPage) {
            setCurrentPage(page)
        }
        const initialFilters = filterStateFromUrl(searchParams)
        const initialSearch = searchParams.get("search") || initialFilters.headId || ""
        if (initialSearch !== searchTerm) {
            setSearchTerm(initialSearch)
        }
        const targetFilters = { ...initialFilters, headId: null }
        if (JSON.stringify(filters) !== JSON.stringify(targetFilters)) {
            setFilters(targetFilters)
        }
    }, [searchParams, currentNetworkMagic, currentPage, searchTerm, filters, updateNetwork, setCurrentPage, setSearchTerm])

    const filteredNetworkHeads = useMemo(() => {
        return heads?.filter((head) => head.networkMagic === currentNetworkMagic) || []
    }, [heads, currentNetworkMagic])

    const filteredHeads = useMemo(() => {
        return filteredNetworkHeads.filter((head) => {
            const matchesSearch = !searchTerm ||
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
        const totalItems = filteredHeads.length
        const start = totalItems - currentPage * ITEMS_PER_PAGE
        const end = totalItems - (currentPage - 1) * ITEMS_PER_PAGE
        return filteredHeads.slice(Math.max(0, start), Math.max(0, end))
    }, [filteredHeads, currentPage])

    const totalPages = useMemo(() => {
        return filteredHeads?.length ? Math.ceil(filteredHeads.length / ITEMS_PER_PAGE) : 1
    }, [filteredHeads])

    const previousPage = () => setCurrentPage(Math.max(currentPage - 1, 1))
    const nextPage = () => setCurrentPage(Math.min(currentPage + 1, totalPages))

    const updateUrlParams = useCallback((page: number, network: number, newFilters: FilterState, currentSearch: string) => {
        const params = new URLSearchParams()

        if (page > 1) {
            params.set("page", page.toString())
        }

        params.set("network", network.toString())

        if (currentSearch) {
            params.set("search", currentSearch)
        }

        Object.entries(newFilters).forEach(([key, value]) => {
            if (value && key !== "headId") params.set(key, value)
        })

        const newUrl = `${pathname}?${params.toString()}`
        if (newUrl !== window.location.href) {
            router.replace(newUrl, { scroll: false })
        }
    }, [pathname, router])

    useEffect(() => {
        if (!isLoading) {
            if (currentPage > totalPages) {
                setCurrentPage(1)
            }
            updateUrlParams(currentPage, currentNetworkMagic, filters, searchTerm)
        }
    }, [currentPage, currentNetworkMagic, filters, searchTerm, isLoading, totalPages, updateUrlParams])

    return (
        <div className="container mx-auto mt-12 text-foreground">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <>
                    <div className="sticky top-14 bg-background z-10">
                        <HeadsSelectTable
                            filters={filters}
                            setFilterState={setFilters}
                            heads={filteredNetworkHeads}
                        />
                    </div>
                    <div className="relative bg-card border border-border rounded-lg overflow-hidden">
                        <table className="table-fixed w-full border-collapse">
                            <thead className="sticky top-0 bg-muted text-muted-foreground uppercase text-[12px] tracking-wider border-b border-border">
                                <tr>
                                    <th className="px-4 py-3 text-center">Head ID</th>
                                    <th className="px-4 py-3 text-center">Head Version</th>
                                    <th className="px-4 py-3 text-center">Status</th>
                                    <th className="px-4 py-3 text-center">Slot Number</th>
                                    <th className="px-4 py-3 text-center">Block Number</th>
                                    <th className="px-4 py-3 text-center">Block Hash</th>
                                    <th className="px-4 py-3 text-center">Value Locked</th>
                                    <th className="px-4 py-3 text-center">Details</th>
                                </tr>
                            </thead>
                        </table>
                        <div className="h-[500px] overflow-y-auto">
                            <table className="table-fixed w-full">
                                <tbody>
                                    {paginatedHeads?.sort((a, b) => b.blockNo - a.blockNo).map((head) => (
                                        <tr key={head.headId} className="hover:bg-accent/50 transition-colors border-b border-border">
                                            <td className="truncate text-center border px-4 py-2">
                                                {head.headId === DOOM_HEAD_ID && (
                                                    <div className="flex items-center justify-center p-1 rounded-sm bg-doom-badge-bg text-doom-badge-fg float-left mr-2" title="Hydra Doom Final">
                                                        <Image src="/hydra.svg" alt="Hydra Head" className="invert" width={16} height={16} />
                                                    </div>
                                                )}
                                                <a href={explorer.mintPolicy(head.headId)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={head.headId}>
                                                    {head.headId.slice(0, 6)}…{head.headId.slice(-4)}
                                                </a>
                                            </td>
                                            <td className="truncate text-center border px-4 py-2 text-sm">{head.version}</td>
                                            <td className="truncate text-center border px-4 py-2 text-sm">{head.status}</td>
                                            <td className="truncate text-center border px-4 py-2 text-sm">{formatNumber(head.point.slot)}</td>
                                            <td className="truncate text-center border px-4 py-2 text-sm">{formatNumber(head.blockNo)}</td>
                                            <td className="truncate text-center border px-4 py-2">
                                                <a href={explorer.block(head.point.blockHash)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={head.point.blockHash}>
                                                    {head.point.blockHash.slice(0, 6)}…{head.point.blockHash.slice(-4)}
                                                </a>
                                            </td>
                                            <td className="truncate text-center border px-4 py-2 text-sm">{formatNumber(totalLovelaceValueLocked(head) / 1000000, { minimumFractionDigits: 0, maximumFractionDigits: 2 })} ₳</td>
                                            <td className="text-center border px-4 py-2">
                                                <Button variant="outline" size="sm" onClick={() => setSelectedHead(head)}>
                                                    View
                                                </Button>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>

                    <div className="mt-4 flex justify-between items-center text-foreground">
                        <Button
                            onClick={previousPage}
                            disabled={currentPage === 1}
                            variant="outline"
                            size="sm"
                        >
                            Previous
                        </Button>

                        {isLoading ? (
                            <span className="text-sm text-muted-foreground">Loading...</span>
                        ) : (
                            <span className="text-sm text-muted-foreground">Page {currentPage} of {totalPages}</span>
                        )}

                        <Button
                            onClick={nextPage}
                            disabled={currentPage === totalPages}
                            variant="outline"
                            size="sm"
                        >
                            Next
                        </Button>
                    </div>
                </>
            )}
            {selectedHead && <HeadDetails head={selectedHead} onClose={() => setSelectedHead(null)} />}
        </div>
    )
}

export default HeadsTable
