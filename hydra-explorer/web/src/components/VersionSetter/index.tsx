"use client" // This is a client component 👈🏽

import React, { ChangeEvent } from 'react'
import { useVersionContext } from "@/providers/VersionProvider"

const VersionSetter = () => {
  const {currentVersion, updateVersion} = useVersionContext()

  return (
    <div className="flex">

      <div className="mt-9">
        <label className="px-4 text-sm font-medium text-gray-200">Select Version:</label>
        <div className="ml-4 py-2">
          <select
            value={currentVersion}
            onChange={(e: ChangeEvent<HTMLSelectElement>) => {
              const versionSelected = e.target.value
              updateVersion(versionSelected)
            }
            }
            className="py-2 px-3 bg-gray-800 text-gray-200 rounded-md"
          >
            <option value={"latest"}>latest</option>
            <option value={"0/20/0"}>0.20.0</option>
            <option value={"0/21/0"}>0.21.0</option>
          </select>
        </div>
      </div>

    </div>
  )
}

export default VersionSetter