import React from "react"
import Image from "next/image"

export const DoomBadge: React.FC = () => {
  return (
    <div
      className="inline-flex items-center justify-center gap-1.5 px-1.5 py-0.5 rounded-sm bg-[#92400E] text-[#FDE68A] text-[11px] font-bold select-none cursor-help shrink-0"
      title="Hydra Doom Final"
    >
      <Image src="/hydra.svg" alt="Hydra Head" className="invert" width={12} height={12} />
      <span>DOOM</span>
    </div>
  )
}

export default DoomBadge
