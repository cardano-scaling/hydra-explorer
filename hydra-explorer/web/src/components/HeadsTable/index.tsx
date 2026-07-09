"use client"

import React, { useState, useMemo, useEffect, useCallback } from "react"
import { useSearchParams, useRouter, usePathname, ReadonlyURLSearchParams } from "next/navigation"
import Image from "next/image"
import { HeadState } from "@/app/model"
import { useHeadsData } from "@/providers/HeadsDataProvider"
import { totalLovelaceValueLocked } from "@/utils"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import HeadDetails from "../HeadDetails"
import { HeadsSelectTable, FilterState, filterStateFromUrl } from "./select"
import { mainnetNetworkMagic, useNetworkContext } from "@/providers/NetworkProvider"

const DOOM_HEAD_ID = "e1393f73096f03a2e127cdace1aad0d3332c158346d0b46efb5a9339"

const ITEMS_PER_PAGE = Number(process.env.NEXT_PUBLIC_ITEMS_PER_PAGE) || 100

const pageFromUrl = (searchParams: ReadonlyURLSearchParams): number => {
    const page = searchParams.get("page")
    return page ? Math.max(1, Number(page)) : 1
}

const HeadsTable: React.FC = () => {
    const { heads, error, isLoading } = useHeadsData()
    const [selectedHead, setSelectedHead] = useState<HeadState | null>(null)
    const explorer = useCardanoExplorer()
    const searchParams = useSearchParams()
    const router = useRouter()
    const pathname = usePathname()

    const [filters, setFilters] = useState<FilterState>(filterStateFromUrl(searchParams))
    const [currentPage, setCurrentPage] = useState<number>(pageFromUrl(searchParams))

    const { currentNetworkMagic } = useNetworkContext()

    const filteredHeads = useMemo(() => {
        return heads?.filter((head) => {
            return (
                (!filters.headId || head.headId === filters.headId) &&
                (!filters.status || head.status === filters.status) &&
                (!filters.version || head.version === filters.version) &&
                (!filters.slot || head.point.slot.toString() === filters.slot) &&
                (!filters.blockNo || head.blockNo.toString() === filters.blockNo) &&
                (!filters.blockHash || head.point.blockHash === filters.blockHash)
            )
        })
    }, [heads, filters])

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

    const previousPage = () => setCurrentPage((p) => Math.max(p - 1, 1))
    const nextPage = () => setCurrentPage((p) => Math.min(p + 1, totalPages))

    const updateUrlParams = useCallback((page: number, network: number, newFilters: FilterState) => {
        const params = new URLSearchParams()

        if (page > 1) {
            params.set("page", page.toString())
        }

        if (network !== mainnetNetworkMagic) {
            params.set("network", network.toString())
        }

        Object.entries(newFilters).forEach(([key, value]) => {
            if (value) params.set(key, value)
        })

        const newUrl = `${pathname}?${params.toString()}`
        if (newUrl !== window.location.href) {
            router.replace(newUrl)
        }
    }, [pathname, router])

    useEffect(() => {
        if (!isLoading) {
            if (currentPage > totalPages) {
                setCurrentPage(1)
            }
            updateUrlParams(currentPage, currentNetworkMagic, filters)
        }
    }, [currentPage, currentNetworkMagic, filters, isLoading, totalPages, updateUrlParams])

    return (
        <div className="container mx-auto mt-12">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <>
                    <div className="sticky top-0 bg-black z-10">
                        <HeadsSelectTable
                            filters={filters}
                            setFilterState={setFilters}
                            heads={heads}
                        />
                    </div>
                    <div className="relative bg-black border border-gray-700 rounded-lg">
                        <table className="table-fixed w-full rounded-lg border-collapse">
                            <thead className="sticky top-0 bg-gray-800">
                                <tr>
                                    <th>Head ID</th>
                                    <th>Head Version</th>
                                    <th>Status</th>
                                    <th>Slot Number</th>
                                    <th>Block Number</th>
                                    <th>Block Hash</th>
                                    <th>Value Locked</th>
                                    <th>Details</th>
                                </tr>
                            </thead>
                        </table>
                        <div className="h-[500px] overflow-y-auto">
                            <table className="table-fixed w-full">
                                <tbody>
                                    {paginatedHeads?.sort((a, b) => b.blockNo - a.blockNo).map((head, index) => (
                                        <tr key={head.headId} className={`${index % 2 === 0 ? "bg-gray-700" : "bg-gray-600"}`}>
                                            <td className="truncate text-center border px-4 py-2">
                                                {head.headId === DOOM_HEAD_ID && (
                                                    <div className="flex items-center justify-center p-1 rounded-full bg-yellow-400 float-left mr-2" title="Hydra Doom Final">
                                                        <Image src="/hydra.svg" alt="Hydra Head" width={16} height={16} />
                                                    </div>
                                                )}
                                                <a href={explorer.mintPolicy(head.headId)} target="_blank" rel="noreferrer" className="text-blue-300 hover:text-blue-500">
                                                    {head.headId}
                                                </a>
                                            </td>
                                            <td className="truncate text-center border px-4 py-2">{head.version}</td>
                                            <td className="truncate text-center border px-4 py-2">{head.status}</td>
                                            <td className="truncate text-center border px-4 py-2">{head.point.slot}</td>
                                            <td className="truncate text-center border px-4 py-2">{head.blockNo}</td>
                                            <td className="truncate text-center border px-4 py-2">
                                                <a href={explorer.block(head.point.blockHash)} target="_blank" rel="noreferrer" className="text-blue-300 hover:text-blue-500">
                                                    {head.point.blockHash}
                                                </a>
                                            </td>
                                            <td className="truncate text-center border px-4 py-2">{totalLovelaceValueLocked(head) / 1000000} ₳</td>
                                            <td className="text-center border px-4 py-2">
                                                <button className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" onClick={() => setSelectedHead(head)}>
                                                    View
                                                </button>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>

                    <div className="mt-4 flex justify-between items-center">
                        <button
                            onClick={previousPage}
                            disabled={currentPage === 1}
                            className={`px-4 py-2 rounded ${currentPage === 1 ? "bg-gray-400 cursor-not-allowed" : "bg-blue-500 hover:bg-blue-600"} text-white`}
                        >
                            Previous
                        </button>

                        {isLoading ? (
                            <span>Loading...</span>
                        ) : (
                            <span>Page {currentPage} of {totalPages}</span>
                        )}

                        <button
                            onClick={nextPage}
                            disabled={currentPage === totalPages}
                            className={`px-4 py-2 rounded ${currentPage === totalPages ? "bg-gray-400 cursor-not-allowed" : "bg-blue-500 hover:bg-blue-600"} text-white`}
                        >
                            Next
                        </button>
                    </div>
                </>
            )}
            {selectedHead && <HeadDetails head={selectedHead} onClose={() => setSelectedHead(null)} />}
        </div>
    )
}

export default HeadsTable
