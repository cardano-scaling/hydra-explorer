"use client" // This is a client component 👈🏽

import React from "react"
import Select from "react-select"
import { HeadState } from "@/app/model"

export interface FilterState {
    headId: string | null
    status: string | null
    version: string | null
    slot: string | null
    blockNo: string | null
    blockHash: string | null
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
    setFilters: React.Dispatch<React.SetStateAction<FilterState>>
    clearAllFilters: () => void
    heads: HeadState[]
}

export const HeadsSelectTable: React.FC<HeadsSelectProps> = ({ filters, setFilters, clearAllFilters, heads }) => {
    // Filter the available heads based on selected filters
    const filteredHeads = heads.filter((head) => {
        return Object.entries(filters).every(([key, value]) => {
            if (!value) return true // If no filter is applied, keep all options
            if (key === "slot") return head.point.slot.toString() === value
            if (key === "blockHash") return head.point.blockHash === value
            if (key === "blockNo") return head.blockNo.toString() === value
            return head[key as keyof HeadState] === value
        })
    })

    // Generate options dynamically based on current filtered heads
    const getOptions = (key: keyof FilterState) => {
        const uniqueValues = Array.from(new Set(
            filteredHeads.map((head) => {
                if (key === "slot") return head.point.slot.toString()
                if (key === "blockHash") return head.point.blockHash
                if (key === "blockNo") return head.blockNo.toString()
                return head[key] as string
            })
        )).filter(Boolean)

        return uniqueValues.map((value) => ({ value, label: value }))
    }

    const grayColor = "rgb(31 41 55)"

    return (
        <div className="mb-4 flex flex-wrap gap-4 items-center">
            {(["headId", "status", "version", "slot", "blockNo", "blockHash"] as (keyof FilterState)[]).map((key) => (
                <Select
                    key={key}
                    value={filters[key] ? { value: filters[key]!, label: filters[key]! } : null}
                    options={getOptions(key)}
                    onChange={(selected) => setFilters((prev) => ({ ...prev, [key]: selected ? selected.value : null }))}
                    placeholder={`Filter by ${key}`}
                    isClearable
                    className="w-64"
                    styles={{
                        control: (provided) => ({
                            ...provided,
                            backgroundColor: grayColor,
                            color: "white",
                            borderColor: grayColor,
                        }),
                        singleValue: (provided) => ({
                            ...provided,
                            color: "white",
                        }),
                        input: (provided) => ({
                            ...provided,
                            color: "white",
                        }),
                        option: (provided, state) => ({
                            ...provided,
                            backgroundColor: state.isSelected ? grayColor : "transparent",
                            color: state.isSelected ? "white" : "black",
                            "&:hover": {
                                backgroundColor: grayColor,
                                color: "white",
                            },
                        }),
                    }}
                />
            ))}
            <button
                type="button"
                onClick={clearAllFilters}
                className="px-4 py-2 rounded bg-blue-500 hover:bg-blue-600 text-white"
            >
                Clear All Filters
            </button>
        </div>
    )
}
