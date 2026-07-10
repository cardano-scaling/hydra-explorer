"use client" // This is a client component 👈🏽

import React, { useEffect } from 'react'
import { HeadMember } from '@/app/model'
import { useCardanoExplorer } from '@/providers/CardanoExplorer'
import { Button } from '@/components/ui/button'
import { formatNumber } from '@/utils'

interface MemberCommitDetailsProps {
  member: HeadMember
  onClose: () => void
}

const MemberCommitDetails: React.FC<MemberCommitDetailsProps> = ({ member, onClose }) => {
  useEffect(() => {
    const handleKeyPress = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        onClose()
      }
    }

    window.addEventListener('keydown', handleKeyPress)

    return () => {
      window.removeEventListener('keydown', handleKeyPress)
    }
  }, [onClose])

  const explorer = useCardanoExplorer()

  return (
    <div className="fixed inset-0 flex items-center justify-center bg-background/80 backdrop-blur-sm z-50 text-foreground">
      <div className="bg-card border border-border p-6 rounded-xl shadow-2xl relative max-w-[800px] w-full mx-4">
        <Button
          variant="destructive"
          size="sm"
          className="absolute top-4 right-4"
          onClick={onClose}
        >
          Close
        </Button>
        <h2 className="text-xl font-bold mb-6 border-b border-border pb-2 text-foreground uppercase tracking-wide">Member Commits</h2>
        <div className="border border-border rounded-lg overflow-hidden">
          <table className="w-full border-collapse">
            <thead className="bg-muted text-muted-foreground uppercase text-[11px] tracking-wider border-b border-border">
              <tr>
                <th className="text-center px-4 py-2">Tx In</th>
                <th className="text-center px-4 py-2">Address</th>
                <th className="text-center px-4 py-2">Value</th>
              </tr>
            </thead>
            <tbody>
              {member.commits &&
                Object.entries(member.commits)
                  .map(([txIn, commit], index) => {
                    const [txId, txIx] = txIn.split("#")
                    return (
                      <tr key={index} className="hover:bg-accent/50 transition-colors border-b border-border last:border-b-0">
                        <td className="truncate text-center border-r border-border px-4 py-2">
                          <a href={explorer.tx(txId)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={txIn}>
                            {txId.slice(0, 6)}…{txId.slice(-4)}
                          </a>#{txIx}
                        </td>
                        <td className="truncate text-center border-r border-border px-4 py-2">
                          <a href={explorer.address(commit.address)} target="_blank" rel="noreferrer" className="text-data hover:text-primary-vivid transition-colors" title={commit.address}>
                            {commit.address.slice(0, 8)}…{commit.address.slice(-8)}
                          </a>
                        </td>
                        <td className="truncate text-center px-4 py-2 text-sm">{formatNumber(commit.value.lovelace / 1000000, { minimumFractionDigits: 0, maximumFractionDigits: 2 })} ₳</td>
                      </tr>
                    )
                  })}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

export default MemberCommitDetails
