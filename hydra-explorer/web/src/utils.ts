import { HeadState } from "./app/model"

export const VERSION_DISPLAY_LENGTH = 8

export const truncateVersion = (version: string): string =>
  version.length > VERSION_DISPLAY_LENGTH
    ? `${version.slice(0, VERSION_DISPLAY_LENGTH)}...`
    : version

export const totalLovelaceValueLocked = (head: HeadState): number => {
  if (head.status === "Finalized" || head.status === "Aborted") return 0
  return (head.members || []).reduce((total, member) => {
    if (member.commits && Object.keys(member.commits).length > 0) {
      const memberTotal = Object.values(member.commits).reduce(
        (acc, commit) => acc + commit.value.lovelace,
        0,
      )
      return total + memberTotal
    }
    return total
  }, 0)
}

export interface FormatNumberOptions extends Intl.NumberFormatOptions {
  locale?: string
  fallback?: string
}

export const formatNumber = (
  value: number | bigint | string | null | undefined,
  options?: FormatNumberOptions,
): string => {
  if (value === null || value === undefined) {
    return options?.fallback ?? "—"
  }

  let num: number | bigint
  if (typeof value === "string") {
    const parsed = Number(value)
    if (isNaN(parsed) || value.trim() === "") {
      return value
    }
    num = parsed
  } else {
    num = value
  }

  const { locale = "en-US", fallback = "—", ...restOptions } = options || {}
  try {
    return new Intl.NumberFormat(locale, restOptions).format(num)
  } catch (e) {
    console.error("Error formatting number:", e)
    return num.toString()
  }
}

/**
 * Formats a contestation deadline (POSIX timestamp in seconds) as:
 *   - relative: "in 2h 30m", "expired 5m ago", "in 45s"
 *   - absolute: full UTC ISO string for tooltip
 *
 * Returns null if value is null/undefined.
 */
export interface DeadlineDisplay {
  /** Short relative label, e.g. "in 2h 30m" or "expired 3m ago" */
  relative: string
  /** Full UTC string for tooltip, e.g. "2026-07-10 14:30:00 UTC" */
  absoluteUtc: string
}

export const formatRelativeDeadline = (
  timestampSeconds: number | null | undefined,
): DeadlineDisplay | null => {
  if (timestampSeconds === null || timestampSeconds === undefined) return null

  const deadlineMs = timestampSeconds * 1000
  const deadlineDate = new Date(deadlineMs)
  if (isNaN(deadlineDate.getTime())) return null

  const nowMs = Date.now()
  const diffMs = deadlineMs - nowMs
  const absMs = Math.abs(diffMs)
  const isPast = diffMs < 0

  const totalSec = Math.floor(absMs / 1000)
  const hours = Math.floor(totalSec / 3600)
  const minutes = Math.floor((totalSec % 3600) / 60)
  const seconds = totalSec % 60

  let relStr: string
  if (absMs < 60_000) {
    // < 1 minute — show seconds
    relStr = `${seconds}s`
  } else if (absMs < 3_600_000) {
    // < 1 hour — show min + sec
    relStr = seconds > 0 ? `${minutes}m ${seconds}s` : `${minutes}m`
  } else if (absMs < 86_400_000) {
    // < 1 day — show hr + min
    relStr = minutes > 0 ? `${hours}h ${minutes}m` : `${hours}h`
  } else {
    // >= 1 day — show days + hours
    const days = Math.floor(totalSec / 86400)
    const remHours = Math.floor((totalSec % 86400) / 3600)
    relStr = remHours > 0 ? `${days}d ${remHours}h` : `${days}d`
  }

  const relative = isPast ? `expired ${relStr} ago` : `in ${relStr}`
  const absoluteUtc = deadlineDate.toISOString().replace("T", " ").replace(".000Z", " UTC")

  return { relative, absoluteUtc }
}
