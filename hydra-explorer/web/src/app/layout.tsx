import type { Metadata } from "next"
import "./globals.css"
import { GeistSans } from "geist/font/sans"
import { cn } from "@/lib/utils"
import { Toaster } from "sonner"

export const metadata: Metadata = {
  title: "Hydrascan",
  description: "Hydra Head Explorer",
  icons: {
    icon: [{ url: "/favicon.svg", type: "image/svg+xml" }],
  },
}

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode
}>) {
  return (
    <html lang="en" className={cn("dark font-sans", GeistSans.variable)}>
      <body>
        {children}
        <Toaster theme="dark" closeButton position="bottom-right" />
      </body>
    </html>
  )
}
