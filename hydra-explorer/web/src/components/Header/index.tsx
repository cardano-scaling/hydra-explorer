"use client"

import React, { useState } from "react"
import Link from "next/link"
import Image from "next/image"
import { Search, X } from "lucide-react"
import { cn } from "@/lib/utils"
import { useStore } from "@/store/useStore"

interface HeaderProps {
    rightSlot?: React.ReactNode
}

const Header: React.FC<HeaderProps> = ({ rightSlot }) => {
    const [mobileSearchOpen, setMobileSearchOpen] = useState(false)
    const searchTerm = useStore((state) => state.searchTerm)
    const setSearchTerm = useStore((state) => state.setSearchTerm)

    return (
        <header className="sticky top-0 z-10 flex flex-nowrap items-center gap-3 border-b border-border bg-background px-4 py-2 md:px-6">
            <Link href="/" className="flex shrink-0 items-center gap-2">
                <Image src="/hydra.svg" alt="Hydra Logo" width={28} height={28} className="dark:invert" priority />
                <div className="flex flex-col leading-none">
                    <span className="text-lg font-bold text-foreground">Hydrascan</span>
                    <span className="hidden text-[10px] tracking-wide text-muted-foreground sm:block">Hydra L2 Explorer</span>
                </div>
            </Link>

            <div className={cn("min-w-0 flex-1", mobileSearchOpen ? "flex" : "hidden md:flex")}>
                <input
                    type="text"
                    placeholder="Search by Head ID, Seed TxIn, or Block Hash"
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                    className="w-full max-w-[480px] rounded-md border border-input bg-card px-3 py-2 text-sm text-foreground placeholder:text-muted-foreground focus:outline-none focus:ring-1 focus:ring-ring"
                />
            </div>

            <div className="ml-auto flex items-center gap-3">
                <button
                    type="button"
                    onClick={() => setMobileSearchOpen((open) => !open)}
                    className="flex h-9 w-9 shrink-0 items-center justify-center rounded-md border border-input bg-card text-foreground hover:bg-muted md:hidden"
                    aria-label={mobileSearchOpen ? "Close search" : "Open search"}
                >
                    {mobileSearchOpen ? <X size={16} /> : <Search size={16} />}
                </button>

                <div className={cn("flex flex-wrap items-center gap-3", mobileSearchOpen && "hidden md:flex")}>
                    {rightSlot}
                </div>
            </div>
        </header>
    )
}

export default Header
