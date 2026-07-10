"use client" // This is a client component 👈🏽

import React, { useState, useEffect } from 'react'
import { HeadState, HeadMember } from '@/app/model'
import MemberCommitDetails from '../MemberCommitDetails'
import { useCardanoExplorer } from '@/providers/CardanoExplorer'
import { Button } from '@/components/ui/button'

interface HeadDetailsProps {
    head: HeadState
    onClose: () => void
}

const HeadDetails: React.FC<HeadDetailsProps> = ({ head, onClose }) => {
    const [selectedMember, setSelectedMember] = useState<HeadMember | null>(null)
    const [showMemberCommitDetails, setShowMemberCommitDetails] = useState(false)

    const handleMemberClick = (member: HeadMember) => {
        setSelectedMember(member)
        setShowMemberCommitDetails(true)
    }

    useEffect(() => {
        const handleKeyPress = (event: KeyboardEvent) => {
            if (event.key === 'Escape') {
                if (showMemberCommitDetails) {
                    setShowMemberCommitDetails(false)
                } else {
                    onClose()
                }
            }
        }

        window.addEventListener('keydown', handleKeyPress)

        return () => {
            window.removeEventListener('keydown', handleKeyPress)
        }
    }, [onClose, showMemberCommitDetails])

    const explorer = useCardanoExplorer()

    return (
        <div className="fixed inset-0 flex items-center justify-center bg-background/80 backdrop-blur-sm z-50 text-foreground">
            <div className="bg-card border border-border p-6 rounded-xl shadow-2xl relative overflow-auto max-h-screen max-w-[800px] w-full mx-4">
                <Button
                    variant="destructive"
                    size="sm"
                    className="absolute top-4 right-4"
                    onClick={onClose}
                >
                    Close
                </Button>
                <h2 className="text-xl font-bold mb-6 border-b border-border pb-2 text-foreground uppercase tracking-wide">Head Details</h2>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Head ID</h3>
                        <p className="truncate">
                            <a href={explorer.mintPolicy(head.headId)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={head.headId}>
                                {head.headId}
                            </a>
                        </p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Head Version</h3>
                        <p className="text-sm">{head.version}</p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40 md:col-span-2">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Seed Tx In</h3>
                        <p className="truncate">
                            {head.seedTxIn ? (
                                <a
                                    href={explorer.tx(head.seedTxIn)}
                                    target="_blank"
                                    rel="noreferrer"
                                    className="text-data hover:text-primary-vivid transition-colors"
                                    title={head.seedTxIn}
                                >
                                    {head.seedTxIn}
                                </a>
                            ) : (
                                <span className="text-sm text-muted-foreground">None</span>
                            )}
                        </p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Status</h3>
                        <p className="text-sm">{head.status}</p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Contestation Period</h3>
                        <p className="text-sm">{head.contestationPeriod}</p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Contestations</h3>
                        <p className="text-sm">{head.contestations}</p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Snapshot Number</h3>
                        <p className="text-sm">{head.snapshotNumber}</p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Contestation Deadline</h3>
                        <p className="text-sm">{head.contestationDeadline || "N/A"}</p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-2">Point</h3>
                        <p className="text-sm">
                            <span className="text-muted-foreground">Block Hash:</span>{" "}
                            <a href={explorer.block(head.point.blockHash)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={head.point.blockHash}>
                                {head.point.blockHash.slice(0, 8)}…{head.point.blockHash.slice(-8)}
                            </a> <br />
                            <span className="text-muted-foreground">Slot:</span> {head.point.slot}
                        </p>
                    </div>
                    <div className="border border-border p-4 rounded-lg bg-background/40 col-span-1 md:col-span-2">
                        <h3 className="text-[12px] font-medium uppercase tracking-wider text-muted-foreground mb-4">Members</h3>
                        <div className="border border-border rounded-lg overflow-hidden">
                            <table className="w-full border-collapse">
                                <thead className="bg-muted text-muted-foreground uppercase text-[11px] tracking-wider border-b border-border">
                                    <tr>
                                        <th className="text-center px-4 py-2">On Chain ID</th>
                                        <th className="text-center px-4 py-2">Party VKey</th>
                                        <th className="text-center px-4 py-2">Total Value Committed</th>
                                        <th className="text-center px-4 py-2">Details</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {head.members?.map((member, index) => (
                                        <tr key={index} className="hover:bg-accent/50 transition-colors border-b border-border last:border-b-0">
                                            <td className="truncate text-center border-r border-border px-4 py-2 text-data" title={member.onChainId || undefined}>
                                                {member.onChainId ? `${member.onChainId.slice(0, 6)}…${member.onChainId.slice(-4)}` : ""}
                                            </td>
                                            <td className="truncate text-center border-r border-border px-4 py-2 text-data" title={member.party?.vkey || undefined}>
                                                {member.party?.vkey ? `${member.party.vkey.slice(0, 6)}…${member.party.vkey.slice(-4)}` : ""}
                                            </td>
                                            <td className="truncate text-center border-r border-border px-4 py-2 text-sm">{getTotalCommittedLovelace(member) / 1000000} ₳</td>
                                            <td className="text-center px-4 py-2">
                                                <Button
                                                    variant="outline"
                                                    size="xs"
                                                    onClick={() => handleMemberClick(member)}
                                                >
                                                    View
                                                </Button>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>
                </div>
            </div>
            {selectedMember && showMemberCommitDetails && <MemberCommitDetails member={selectedMember} onClose={() => setShowMemberCommitDetails(false)} />}
        </div>
    )
}

function getTotalCommittedLovelace(member: HeadMember): number {
    if (!member.commits) return 0
    return Object.values(member.commits).reduce((total, commit) => total + commit.value.lovelace, 0)
}

export default HeadDetails
