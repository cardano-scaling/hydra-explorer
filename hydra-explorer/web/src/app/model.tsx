export interface UTxO {
    [key: string]: {
        address: string
        datum: any
        datumhash: any
        inlineDatum: any
        referenceScript: any
        value: {
            lovelace: number
        }
    }
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
    status: string
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
