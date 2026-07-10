import { HeadState } from "./app/model"

export const totalLovelaceValueLocked = (head: HeadState): number => {
    if (head.status === "Finalized" || head.status === "Aborted") return 0
    return (head.members || []).reduce((total, member) => {
        if (member.commits && Object.keys(member.commits).length > 0) {
            const memberTotal = Object.values(member.commits).reduce(
                (acc, commit) => acc + commit.value.lovelace,
                0
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
    options?: FormatNumberOptions
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

