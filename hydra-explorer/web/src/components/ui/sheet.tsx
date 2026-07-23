"use client"

import * as React from "react"
import { Dialog } from "@base-ui/react/dialog"
import { cva, type VariantProps } from "class-variance-authority"
import { X } from "lucide-react"

import { cn } from "@/lib/utils"

// Sheet component — slide-in panel from the right.
// Built on Base UI Dialog for full accessibility (focus trap, ESC, aria-modal).
//
// Visual spec from DESIGN.md slide-panel:
//   - bg: var(--card)
//   - border-left: 1px solid var(--border)
//   - box-shadow: -4px 0 24px oklch(0 0 0 / 30%) (left edge only)
//   - backdrop: oklch(0 0 0 / 50%)
//   - width: 100% mobile / 70% tablet / 50% desktop
//   - animation: translateX(100%) → translateX(0) in 300ms ease-out
//   - prefers-reduced-motion: instant (transition-none)

// ─── Root ──────────────────────────────────────────────────────────────────
const Sheet = Dialog.Root

// ─── Trigger ───────────────────────────────────────────────────────────────
const SheetTrigger = Dialog.Trigger

// ─── Close ─────────────────────────────────────────────────────────────────
const SheetClose = Dialog.Close

// ─── Portal ────────────────────────────────────────────────────────────────
const SheetPortal = Dialog.Portal

// ─── Overlay (backdrop) ────────────────────────────────────────────────────
function SheetOverlay({
  className,
  ...props
}: React.ComponentPropsWithoutRef<typeof Dialog.Backdrop>) {
  return (
    <Dialog.Backdrop
      className={cn(
        // Full-screen dim layer
        "fixed inset-0 z-50 bg-[oklch(0_0_0_/_50%)]",
        // Fade in / out via Base UI data attributes
        "transition-opacity duration-300 ease-out motion-reduce:transition-none",
        "opacity-0",
        "[&[data-open]]:opacity-100",
        "[&[data-starting-style]]:opacity-0",
        "[&[data-ending-style]]:opacity-0",
        className,
      )}
      {...props}
    />
  )
}

// ─── Panel variants ─────────────────────────────────────────────────────────
const sheetVariants = cva(
  cn(
    // Fixed right-side panel, full height
    "fixed top-0 right-0 z-50 h-full flex flex-col",
    // Surface: card bg + left border + left-only shadow
    "bg-card border-l border-border",
    "shadow-[-4px_0_24px_oklch(0_0_0_/_30%)]",
    // Rounded left corners: xl = 10px per DESIGN.md
    "rounded-l-[10px]",
    // Clip overflow for inner scroll
    "overflow-hidden",
    // Slide animation: right → in-place in 300ms ease-out
    "transition-[translate,opacity] duration-300 ease-out motion-reduce:transition-none",
    "translate-x-full",
    "[&[data-open]]:translate-x-0",
    "[&[data-starting-style]]:translate-x-full",
    "[&[data-ending-style]]:translate-x-full",
  ),
  {
    variants: {
      side: {
        right: "w-full md:w-[70%] lg:w-[50%]",
      },
    },
    defaultVariants: {
      side: "right",
    },
  },
)

// ─── Content ────────────────────────────────────────────────────────────────
interface SheetContentProps
  extends React.ComponentPropsWithoutRef<typeof Dialog.Popup>, VariantProps<typeof sheetVariants> {}

function SheetContent({ className, children, side, ...props }: SheetContentProps) {
  return (
    <SheetPortal>
      <SheetOverlay />
      <Dialog.Popup className={cn(sheetVariants({ side }), className)} {...props}>
        {children}
      </Dialog.Popup>
    </SheetPortal>
  )
}

// ─── Header ─────────────────────────────────────────────────────────────────
function SheetHeader({ className, ...props }: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      className={cn(
        "flex items-start justify-between gap-4 px-6 py-4 border-b border-border shrink-0",
        className,
      )}
      {...props}
    />
  )
}

// ─── Title ──────────────────────────────────────────────────────────────────
function SheetTitle({ className, ...props }: React.ComponentPropsWithoutRef<typeof Dialog.Title>) {
  return (
    <Dialog.Title className={cn("text-sm font-semibold text-foreground", className)} {...props} />
  )
}

// ─── Description ────────────────────────────────────────────────────────────
function SheetDescription({
  className,
  ...props
}: React.ComponentPropsWithoutRef<typeof Dialog.Description>) {
  return (
    <Dialog.Description className={cn("text-sm text-muted-foreground", className)} {...props} />
  )
}

// ─── Close Button (×) ───────────────────────────────────────────────────────
// Pre-built accessible close button per spec: top-right "X" icon.
function SheetCloseButton({ className, ...props }: React.HTMLAttributes<HTMLButtonElement>) {
  return (
    <Dialog.Close
      className={cn(
        "ml-auto inline-flex shrink-0 items-center justify-center",
        "h-8 w-8 rounded-[var(--radius-md)] text-muted-foreground",
        "hover:bg-muted hover:text-foreground transition-colors duration-150",
        "focus-visible:outline-none focus-visible:ring-2",
        "focus-visible:ring-[var(--primary-vivid)] focus-visible:ring-offset-2 focus-visible:ring-offset-background",
        className,
      )}
      aria-label="Close panel"
      {...props}
    >
      <X size={16} aria-hidden="true" />
    </Dialog.Close>
  )
}

// ─── Body ───────────────────────────────────────────────────────────────────
function SheetBody({ className, ...props }: React.HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("flex-1 overflow-y-auto px-6 py-4", className)} {...props} />
}

export {
  Sheet,
  SheetPortal,
  SheetOverlay,
  SheetTrigger,
  SheetClose,
  SheetContent,
  SheetHeader,
  SheetTitle,
  SheetDescription,
  SheetCloseButton,
  SheetBody,
}
