import React from "react"

const Footer: React.FC = () => {
  return (
    <footer className="border-t border-border">
      <div className="mx-auto flex max-w-content-max-width flex-wrap items-center justify-center gap-4 px-6 py-3.5 text-[11px] text-muted-foreground">
        <a
          href="https://github.com/cardano-scaling/hydra-explorer"
          target="_blank"
          rel="noreferrer"
          className="hover:text-foreground"
        >
          GitHub
        </a>
        <span className="opacity-30">·</span>
        <a
          href="https://hydra.family/head-protocol/docs"
          target="_blank"
          rel="noreferrer"
          className="hover:text-foreground"
        >
          Hydra Docs
        </a>
        <span className="opacity-30">·</span>
        <span>v1.0.0</span>
        <span className="opacity-30">·</span>
        <span>© Hydra Protocol</span>
      </div>
    </footer>
  )
}

export default Footer
