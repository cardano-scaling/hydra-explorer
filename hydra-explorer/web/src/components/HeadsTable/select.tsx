"use client" // This is a client component 👈🏽

import React, { useEffect, useState } from "react"
import Select from "react-select"
import { HeadState } from "@/app/model"
import { ReadonlyURLSearchParams } from "next/navigation"
import { Button } from "@/components/ui/button"

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
        if (key in filters) {
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

import { useStore } from "@/store/useStore"

interface HeadsSelectProps {
    filters: FilterState
    setFilterState: React.Dispatch<React.SetStateAction<FilterState>>
    heads: HeadState[]
}

export const HeadsSelectTable: React.FC<HeadsSelectProps> = ({ filters, setFilterState, heads }) => {
    const searchTerm = useStore((state) => state.searchTerm)
    const setSearchTerm = useStore((state) => state.setSearchTerm)

    // Must be deleted once
    // https://github.com/JedWatson/react-select/issues/5459 is fixed.
    const [isMounted, setIsMounted] = useState(false)
    useEffect(() => setIsMounted(true), [])

    // Generate global select options dynamically based on current heads
    const getSelectOptions = (key: keyof FilterState) => {
        const createOptions = (values: string[]) => {
            return values.map((value) => ({ value, label: value }))
        }

        if (key === "status") {
            // XXX: Don't forget to change if a new top-level status is defined.
            const allStatuses = ["Open", "Aborted", "Initializing", "Closed", "Finalized"]
            return createOptions(allStatuses)
        }
        else if (key === "version") {
            const seen = new Set<string>()
            const uniqueValues: string[] = []
            for (const head of heads) {
                let version = head.version
                if (version && !seen.has(version)) {
                    seen.add(version)
                    uniqueValues.push(version)
                }
            }
            return createOptions(uniqueValues)
        } else {
            const allValues: string[] = heads.map(head => {
                switch (key) {
                    case "slot":
                        return head.point.slot.toString()
                    case "blockHash":
                        return head.point.blockHash
                    case "blockNo":
                        return head.blockNo.toString()
                    default:
                        return head[key] as string
                }
            })
            return createOptions(allValues)
        }
    }

    const clearFilterState = () => {
        setFilterState(emptyFilterState)
        setSearchTerm("")
    }

    return isMounted ? (
        <div className="mb-4 flex flex-wrap gap-4 items-center">
            {(["headId", "status", "version", "slot", "blockNo", "blockHash"] as (keyof FilterState)[]).map((key) => {
                const isHeadId = key === "headId"
                const value = isHeadId
                    ? (searchTerm ? { value: searchTerm, label: searchTerm } : null)
                    : (filters[key] ? { value: filters[key]!, label: filters[key]! } : null)

                return (
                    <Select
                        key={key}
                        value={value}
                        options={getSelectOptions(key)}
                        onChange={(selected) => {
                            const val = selected ? selected.value : null
                            if (isHeadId) {
                                setSearchTerm(val || "")
                            } else {
                                setFilterState((prev) => ({ ...prev, [key]: val }))
                            }
                        }}
                        placeholder={`Filter by ${key}`}
                        isClearable
                    className="w-64"
                    styles={{
                        control: (provided) => ({
                            ...provided,
                            backgroundColor: "var(--card)",
                            color: "var(--foreground)",
                            borderColor: "var(--border)",
                            boxShadow: "none",
                        }),
                        singleValue: (provided) => ({
                            ...provided,
                            color: "var(--foreground)",
                        }),
                        input: (provided) => ({
                            ...provided,
                            color: "var(--foreground)",
                        }),
                        placeholder: (provided) => ({
                            ...provided,
                            color: "var(--muted-foreground)",
                        }),
                        menu: (provided) => ({
                            ...provided,
                            backgroundColor: "var(--popover)",
                            border: "1px solid var(--border)",
                        }),
                        option: (provided, state) => ({
                            ...provided,
                            backgroundColor: state.isSelected ? "var(--accent)" : "transparent",
                            color: "var(--foreground)",
                            "&:hover": {
                                backgroundColor: "var(--accent)",
                                color: "var(--accent-foreground)",
                            },
                        }),
                    }}
                />
                )
            })}
            <Button
                type="button"
                onClick={clearFilterState}
                size="sm"
                className="bg-primary-vivid text-white hover:bg-primary-vivid/90 border-transparent"
            >
                Clear All Filters
            </Button>
        </div>
    ) : null
}
