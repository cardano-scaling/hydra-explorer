"use client"

import React from "react"
import Link from "next/link"
import Image from "next/image"

interface HeaderProps {
  rightSlot?: React.ReactNode
}

const Header: React.FC<HeaderProps> = ({ rightSlot }) => {
  return (
    <header className="sticky top-0 z-30 flex flex-nowrap items-center gap-3 border-b border-border bg-background px-4 py-4 md:px-6">
      <Link href="/" className="flex shrink-0 items-center gap-2">
        <Image
          src="/hydra.svg"
          alt="Hydra Logo"
          width={36}
          height={36}
          className="dark:invert"
          priority
        />
        <span className="text-2xl font-bold text-foreground">Hydrascan</span>
      </Link>

      <div className="ml-auto flex items-center gap-3">
        <div className="flex flex-wrap items-center gap-3">{rightSlot}</div>
      </div>
    </header>
  )
}

export default Header
