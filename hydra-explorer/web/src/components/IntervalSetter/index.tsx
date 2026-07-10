"use client"

import { Button } from "@/components/ui/button"
import { useStore } from "@/store/useStore"
import { ChangeEvent } from "react"
import { Pause, Play } from "lucide-react"
import { cn } from "@/lib/utils"

const IntervalSetter = () => {
  const isAutoUpdate = useStore((state) => state.isAutoUpdate)
  const intervalTime = useStore((state) => state.intervalTime)
  const toggleAutoUpdate = useStore((state) => state.toggleAutoUpdate)
  const updateIntervalTime = useStore((state) => state.updateIntervalTime)

  return (
    <div className="flex items-center gap-3">
      {/* Polling controls container */}
      <div className="flex items-center gap-2">
        {/* Pulsing status dot */}
        <div className="relative flex h-2.5 w-2.5 items-center justify-center">
          {isAutoUpdate && (
            <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-status-open opacity-75"></span>
          )}
          <span className="relative inline-flex rounded-full h-2.5 w-2.5 bg-status-open"></span>
        </div>

        {/* Pause/Resume button */}
        <Button
          type="button"
          onClick={toggleAutoUpdate}
          variant="ghost"
          size="sm"
          className={cn(
            "flex items-center gap-1.5 h-8 px-2.5 rounded-md transition-all outline-none font-medium",
            isAutoUpdate
              ? "bg-primary-muted text-primary-vivid hover:bg-primary-muted/80 border-transparent"
              : "hover:bg-muted text-foreground"
          )}
          aria-label={isAutoUpdate ? "Pause updates" : "Resume updates"}
        >
          {isAutoUpdate ? <Pause size={14} className="shrink-0" /> : <Play size={14} className="shrink-0" />}
          <span>{isAutoUpdate ? "Live" : "Paused"}</span>
        </Button>
      </div>

      {/* Interval Selector */}
      <div className="flex items-center gap-1.5">
        <span className="text-sm font-medium text-muted-foreground">Interval:</span>
        <select
          value={intervalTime}
          onChange={(e: ChangeEvent<HTMLSelectElement>) => {
            const intervalValue = parseInt(e.target.value, 10)
            updateIntervalTime(intervalValue)
          }}
          className="py-1.5 px-3 bg-background text-foreground border border-input rounded-md focus:outline-none focus:ring-1 focus:ring-ring text-sm"
        >
          <option value={1000}>1s</option>
          <option value={5000}>5s</option>
          <option value={10000}>10s</option>
        </select>
      </div>
    </div>
  )
}

export default IntervalSetter