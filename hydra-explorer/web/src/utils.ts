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
