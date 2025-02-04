"use client" // This is a client component 👈🏽

import React, { ChangeEvent } from 'react'
import { useNetworkContext } from "@/providers/NetworkProvider"

const NetworkSetter = () => {
  const { currentNetwork, currentNetworkMagic, updateNetwork } = useNetworkContext()

  return (
    <div className="flex">

      <div className="mt-9">
        <label className="px-4 text-sm font-medium text-gray-200">Select Network:</label>
        <div className="ml-4 py-2">
          <select
            value={currentNetworkMagic}
            onChange={(e: ChangeEvent<HTMLSelectElement>) => {
              const networkSelected = e.target.innerText
              const networkMagicSelected = Number(e.target.value)
              updateNetwork(networkSelected, networkMagicSelected)
            }
            }
            className="py-2 px-3 bg-gray-800 text-gray-200 rounded-md"
          >
            <option value={0}>mainnet</option>
            <option value={2}>preview</option>
          </select>
        </div>
      </div>

    </div>
  )
}

export default NetworkSetter
