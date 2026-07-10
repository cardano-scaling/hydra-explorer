"use client"

import { Button } from "@/components/ui/button"
import { useStore } from "@/store/useStore"
import { Pause, Play } from "lucide-react"
import { cn } from "@/lib/utils"
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select"

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
              : "hover:bg-muted text-foreground",
          )}
          aria-label={isAutoUpdate ? "Pause updates" : "Resume updates"}
        >
          {isAutoUpdate ? (
            <Pause size={14} className="shrink-0" />
          ) : (
            <Play size={14} className="shrink-0" />
          )}
          <span>{isAutoUpdate ? "Live" : "Paused"}</span>
        </Button>
      </div>

      {/* Interval Selector */}
      <div className="flex items-center gap-1.5">
        <span className="text-sm font-medium text-muted-foreground">Interval:</span>
        <Select
          value={String(intervalTime)}
          onValueChange={(val) => {
            if (val) {
              const intervalValue = parseInt(val, 10)
              updateIntervalTime(intervalValue)
            }
          }}
        >
          <SelectTrigger className="w-[80px] h-8 text-xs border-border bg-background">
            <SelectValue placeholder="1s" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="1000">1s</SelectItem>
            <SelectItem value="5000">5s</SelectItem>
            <SelectItem value="10000">10s</SelectItem>
          </SelectContent>
        </Select>
      </div>
    </div>
  )
}

export default IntervalSetter
