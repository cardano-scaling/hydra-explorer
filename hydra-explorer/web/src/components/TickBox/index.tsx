"use client" // This is a client component 👈🏽

import { TickState } from "@/app/model"
import useDataFetcher from "@/hooks/DataFetcher"
import { useCardanoExplorer } from "@/providers/CardanoExplorer"
import { useMemo, useState } from "react"
import { useNetworkContext } from "@/providers/NetworkProvider"

const TickBox = () => {
    const [ticks, setTicks] = useState<TickState[]>([])
    const [error, setError] = useState<string | null>(null)

    const { currentNetworkMagic } = useNetworkContext()

    const url = process.env.NEXT_PUBLIC_EXPLORER_URL
        ? `${process.env.NEXT_PUBLIC_EXPLORER_URL}/ticks`
        : `/ticks`

    useDataFetcher<TickState[]>({
        url,
        setFetchedData: setTicks,
        setError,
    })

    const tick: TickState | undefined = useMemo(() => {
        return ticks.find(
            (tick) => {
                return tick.networkMagic === currentNetworkMagic
            }
        )
    }, [ticks, currentNetworkMagic])


    const explorer = useCardanoExplorer()

    return (
        <div className="container mx-auto mt-8">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full border border-gray-700 rounded-lg">
                    <table className="table-fixed bg-gray-800 text-white rounded-lg border-collapse">
                        <thead>
                            <tr>
                                <th className="text-center px-4 py-2">Block Number</th>
                                <th className="text-center px-4 py-2">Block Hash</th>
                                <th className="text-center px-4 py-2">Slot Number</th>
                            </tr>
                        </thead>
                        <tbody>
                            {tick ? (
                                <tr>
                                    <td className="truncate text-center border px-4 py-2">{tick?.blockNo}</td>
                                    <td className="truncate text-center border px-4 py-2">
                                        <a href={explorer.block(tick?.point.blockHash)} target="_blank" className="text-blue-300 hover:text-blue-500">
                                            {tick?.point.blockHash}
                                        </a>
                                    </td>
                                    <td className="truncate text-center border px-4 py-2">{tick?.point.slot}</td>
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
