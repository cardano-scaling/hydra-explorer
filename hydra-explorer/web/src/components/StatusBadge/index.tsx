import React from "react"
import { HeadStatus } from "@/app/model"

interface StatusBadgeProps {
  status?: HeadStatus | string | null
}

const statusStyles: Record<string, { bg: string; text: string }> = {
  Open: { bg: "bg-[#052E16]", text: "text-[#22C55E]" },
  Initializing: { bg: "bg-[#172554]", text: "text-[#60A5FA]" },
  Closed: { bg: "bg-[#431407]", text: "text-[#F59E0B]" },
  Finalized: { bg: "bg-[#0F172A]", text: "text-[#94A3B8]" },
  Aborted: { bg: "bg-[#450A0A]", text: "text-[#F87171]" },
}

export const StatusBadge: React.FC<StatusBadgeProps> = ({ status }) => {
  if (!status) {
    return <span className="text-muted-foreground">—</span>
  }
  const style = statusStyles[status] || { bg: "bg-gray-800", text: "text-gray-400" }
  return (
    <span
      className={`inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium ${style.bg} ${style.text}`}
    >
      {status}
    </span>
  )
}

export default StatusBadge
