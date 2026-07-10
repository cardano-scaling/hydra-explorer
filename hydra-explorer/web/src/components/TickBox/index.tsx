import { TickState } from "@/app/model"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import { useMemo } from "react"
import { useStore } from "@/store/useStore"

const TickBox = () => {
    const ticks = useStore((state) => state.ticks)
    const error = useStore((state) => state.ticksError)
    const currentNetworkMagic = useStore((state) => state.currentNetworkMagic)

    const tick: TickState | undefined = useMemo(() => {
        return ticks.find(
            (tick) => {
                return tick.networkMagic === currentNetworkMagic
            }
        )
    }, [ticks, currentNetworkMagic])


    const explorer = useCardanoExplorer()

    return (
        <div className="container mx-auto mt-8 text-foreground">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full border border-border bg-card rounded-lg overflow-hidden">
                    <table className="table-fixed w-full border-collapse">
                        <thead className="bg-muted text-muted-foreground uppercase text-[12px] tracking-wider border-b border-border">
                            <tr>
                                <th className="text-center px-4 py-3">Block Number</th>
                                <th className="text-center px-4 py-3">Block Hash</th>
                                <th className="text-center px-4 py-3">Slot Number</th>
                            </tr>
                        </thead>
                        <tbody>
                            {tick ? (
                                <tr className="hover:bg-accent/50 transition-colors">
                                    <td className="truncate text-center border px-4 py-2 text-sm">{tick?.blockNo}</td>
                                    <td className="truncate text-center border px-4 py-2">
                                        <a href={explorer.block(tick?.point.blockHash)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={tick?.point.blockHash}>
                                            {tick?.point.blockHash.slice(0, 6)}…{tick?.point.blockHash.slice(-4)}
                                        </a>
                                    </td>
                                    <td className="truncate text-center border px-4 py-2 text-sm">{tick?.point.slot}</td>
                                </tr>
                            ) : null}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    )
}

export default TickBox
