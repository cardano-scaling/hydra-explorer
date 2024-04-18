"use client" // This is a client component 👈🏽

import React, { useState } from 'react'
import { HeadState } from "@/app/model"
import { useHeadsData } from "@/providers/HeadsDataProvider"
import HeadDetails from "../HeadDetails"
import { totalLovelaceValueLocked } from '@/utils'
import { useCardanoExplorer } from '@/providers/CardanoExplorer'

const HeadsTable: React.FC = () => {
    const { heads, error } = useHeadsData()
    const [selectedHead, setSelectedHead] = useState<HeadState | null>(null)

    const handleRowClick = (head: HeadState) => {
        setSelectedHead(head)
    }

    const explorer = useCardanoExplorer()

    return (
        <div className="container mx-auto mt-12 overflow-y-auto">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full">
                    <table className="table-fixed w-full rounded-lg">
                        <thead className="sticky top-0 text-center px-4 py-2 bg-gray-800">
                            <tr>
                                <th>Head ID</th>
                                <th>Status</th>
                                <th>Slot Number</th>
                                <th>Block Number</th>
                                <th>Block Hash</th>
                                <th>Value Locked</th>
                                <th>Details</th>
                            </tr>
                        </thead>
                        <tbody>
                            {heads?.sort((a, b) => b.blockNo - a.blockNo).map((head, index) => (
                                <tr key={index} className={`${index % 2 === 0 ? 'bg-gray-700' : 'bg-gray-600'}`}>
                                    <td className="truncate text-center border px-4 py-2">
                                        <a href={explorer.mintPolicy(head.headId)} target="_blank" className="text-blue-300 hover:text-blue-500">
                                            {head.headId}
                                        </a>
                                    </td>
                                    <td className="truncate text-center border px-4 py-2">{head.status}</td>
                                    <td className="truncate text-center border px-4 py-2">{head.point.slot}</td>
                                    <td className="truncate text-center border px-4 py-2">{head.blockNo}</td>
                                    <td className="truncate text-center border px-4 py-2">
                                        <a href={explorer.block(head.point.blockHash)} target="_blank" className="text-blue-300 hover:text-blue-500">
                                            {head.point.blockHash}
                                        </a>
                                    </td>
                                    <td className="truncate text-center border px-4 py-2">{totalLovelaceValueLocked(head) / 1000000} ₳</td>
                                    <td className="text-center border px-4 py-2">
                                        <button
                                            className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                                            onClick={() => handleRowClick(head)}
                                        >
                                            View
                                        </button>
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            {selectedHead && <HeadDetails head={selectedHead} onClose={() => setSelectedHead(null)} />}
        </div>
    )
}

export default HeadsTable