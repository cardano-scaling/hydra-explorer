"use client"

import React, { useState, useEffect } from "react"
import { Clipboard, Check, AlertCircle, ExternalLink } from "lucide-react"

interface HashDisplayProps {
  value?: string | null
  href?: string
  /** If true, renders the full hash value instead of truncated {first6}…{last4} */
  showFull?: boolean
}

const HashDisplay: React.FC<HashDisplayProps> = ({ value, href, showFull = false }) => {
  const [copyStatus, setCopyStatus] = useState<"idle" | "success" | "error">("idle")

  const handleCopy = async (e: React.MouseEvent) => {
    e.preventDefault()
    e.stopPropagation()
    if (!value) return
    try {
      if (navigator?.clipboard) {
        await navigator.clipboard.writeText(value)
        setCopyStatus("success")
      } else {
        // Fallback for non-secure contexts
        const textarea = document.createElement("textarea")
        try {
          textarea.value = value
          textarea.style.position = "fixed"
          textarea.style.opacity = "0"
          document.body.appendChild(textarea)
          textarea.select()
          const success = document.execCommand("copy")
          if (!success) throw new Error("Copy command failed")
          setCopyStatus("success")
        } finally {
          if (document.body.contains(textarea)) {
            document.body.removeChild(textarea)
          }
        }
      }
    } catch (err) {
      console.error("Failed to copy text: ", err)
      setCopyStatus("error")
    }
  }

  useEffect(() => {
    if (copyStatus === "idle") return
    const timer = setTimeout(() => {
      setCopyStatus("idle")
    }, 2000)
    return () => clearTimeout(timer)
  }, [copyStatus])

  if (!value) return null

  // Format: {first6}…{last4} in table; full value in detail panel
  const truncated = showFull
    ? value
    : value.length > 12
      ? `${value.slice(0, 6)}\u2026${value.slice(-4)}`
      : value

  const content = (
    <span
      className="font-mono text-[13px] text-[#8DA3C4] inline-flex items-center gap-1"
      title={value}
    >
      {truncated}
      {href && (
        <ExternalLink className="w-3 h-3 opacity-65 hover:text-[#4C82F0] transition-colors" />
      )}
    </span>
  )

  return (
    <div className="inline-flex items-center gap-1.5 justify-center">
      {href ? (
        <a
          href={href}
          target="_blank"
          rel="noopener noreferrer"
          className="hover:text-[#4C82F0] transition-colors"
        >
          {content}
        </a>
      ) : (
        content
      )}
      <button
        onClick={handleCopy}
        className="p-1 rounded text-muted-foreground hover:text-[#4C82F0] active:text-[#22C55E] transition-colors cursor-pointer"
        title={
          copyStatus === "success"
            ? "Copied!"
            : copyStatus === "error"
              ? "Copy failed"
              : "Copy to clipboard"
        }
        aria-label={
          copyStatus === "success"
            ? "Copied!"
            : copyStatus === "error"
              ? "Copy failed"
              : "Copy to clipboard"
        }
      >
        {copyStatus === "success" ? (
          <Check className="w-3.5 h-3.5 text-[#22C55E]" />
        ) : copyStatus === "error" ? (
          <AlertCircle className="w-3.5 h-3.5 text-[#EF4444]" />
        ) : (
          <Clipboard className="w-3.5 h-3.5" />
        )}
      </button>
    </div>
  )
}

export { HashDisplay }
export default HashDisplay
