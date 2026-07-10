import { create } from "zustand"
import { HeadState, TickState } from "@/app/model"
import { totalLovelaceValueLocked } from "@/utils"

export const mainnetNetworkMagic = 764824073

export interface StoreState {
  // Network & Polling Config
  currentNetworkMagic: number
  intervalTime: number
  isAutoUpdate: boolean
  isOnline: boolean

  // UI Navigation State
  searchTerm: string
  pageIndex: number

  // Data Cache & Status
  heads: HeadState[]
  ticks: TickState[]
  headsError: string | null
  ticksError: string | null
  headsLoading: boolean
  ticksLoading: boolean

  // Core Actions
  updateNetwork: (magic: number) => void
  updateIntervalTime: (ms: number) => void
  toggleAutoUpdate: () => void
  setIsOnline: (online: boolean) => void
  setSearchTerm: (term: string) => void
  setPageIndex: (index: number) => void

  // Cache Update Actions
  setHeads: (heads: HeadState[]) => void
  setTicks: (ticks: TickState[]) => void
  setHeadsError: (error: string | null) => void
  setTicksError: (error: string | null) => void
  setHeadsLoading: (loading: boolean) => void
  setTicksLoading: (loading: boolean) => void
}

const getDefaultInterval = () => {
  if (typeof process !== "undefined" && process.env.NEXT_PUBLIC_PULL_INTERVAL) {
    const val = Number(process.env.NEXT_PUBLIC_PULL_INTERVAL)
    return isNaN(val) ? 5000 : val
  }
  return 5000
}

export const useStore = create<StoreState>((set) => ({
  // Initial State
  currentNetworkMagic: mainnetNetworkMagic,
  intervalTime: getDefaultInterval(),
  isAutoUpdate: true,
  isOnline: true,
  searchTerm: "",
  pageIndex: 1,
  heads: [],
  ticks: [],
  headsError: null,
  ticksError: null,
  headsLoading: true,
  ticksLoading: true,

  // Actions
  updateNetwork: (magic: number) =>
    set({
      currentNetworkMagic: magic,
      headsLoading: true,
      ticksLoading: true,
      heads: [],
      ticks: [],
      pageIndex: 1,
    }),
  updateIntervalTime: (ms: number) => set({ intervalTime: ms }),
  toggleAutoUpdate: () => set((state) => ({ isAutoUpdate: !state.isAutoUpdate })),
  setIsOnline: (online: boolean) => set({ isOnline: online }),
  setSearchTerm: (term: string) => set({ searchTerm: term }),
  setPageIndex: (index: number) => set({ pageIndex: index }),

  setHeads: (heads: HeadState[]) =>
    set({
      heads: [...heads].sort((a, b) => b.blockNo - a.blockNo),
      headsLoading: false,
      headsError: null,
    }),
  setTicks: (ticks: TickState[]) => set({ ticks, ticksLoading: false, ticksError: null }),
  setHeadsError: (error: string | null) => set({ headsError: error, headsLoading: false }),
  setTicksError: (error: string | null) => set({ ticksError: error, ticksLoading: false }),
  setHeadsLoading: (loading: boolean) => set({ headsLoading: loading }),
  setTicksLoading: (loading: boolean) => set({ ticksLoading: loading }),
}))

export const selectTotalHeads = (state: StoreState) => {
  return (state.heads || []).filter(
    (head) => head && head.networkMagic === state.currentNetworkMagic,
  ).length
}

export const selectActiveHeads = (state: StoreState) => {
  return (state.heads || []).filter(
    (head) => head && head.networkMagic === state.currentNetworkMagic && head.status === "Open",
  ).length
}

export const selectTVLInAda = (state: StoreState) => {
  const filtered = (state.heads || []).filter(
    (head) => head && head.networkMagic === state.currentNetworkMagic,
  )
  const totalLovelace = filtered.reduce((total, head) => total + totalLovelaceValueLocked(head), 0)
  return totalLovelace / 1_000_000
}

export const selectActiveTick = (state: StoreState) => {
  return (state.ticks || []).find((tick) => tick?.networkMagic === state.currentNetworkMagic)
}
