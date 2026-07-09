export type HeadStatus = 'Initializing' | 'Open' | 'Closed' | 'Finalized' | 'Aborted'

export interface UTxOEntry {
    address: string
    datum: unknown
    datumhash: unknown
    inlineDatum: unknown
    referenceScript: unknown
    value: {
        lovelace: number
    }
}

export interface UTxO {
    [key: string]: UTxOEntry
}

export interface HeadMember {
    commits: UTxO | null
    onChainId: string | null
    party: {
        vkey: string
    }
}

export interface ChainPoint {
    blockHash: string
    slot: string
}

export interface HeadState {
    network: string
    networkMagic: number
    version: string
    headId: string
    seedTxIn: string | null
    status: HeadStatus
    contestationPeriod: number | null
    members: HeadMember[] | null
    contestations: number | null
    snapshotNumber: number | null
    contestationDeadline: number | null
    point: ChainPoint
    blockNo: number
}

export interface TickState {
    network: string
    networkMagic: number
    point: ChainPoint
    blockNo: number
}
