import Head from "next/head"

import Image from "next/image"
import IntervalSettingProvider from "@/providers/IntervalProvider"
import IntervalSetter from "@/components/IntervalSetter"
import TickBox from "@/components/TickBox"
import { HeadsDataProvider } from "@/providers/HeadsDataProvider"
import HeadsDashboard from "@/components/HeadsDashboard"
import { CardanoExplorerProvider } from "@/providers/CardanoExplorer"
import NetworkSetter from "@/components/NetworkSetter"

import dynamic from "next/dynamic"
const HeadsTable = dynamic(() => import("@/components/HeadsTable"), { ssr: false })
const NetworkSettingProvider = dynamic(() => import("@/providers/NetworkProvider"), { ssr: false })

export default function Home() {

  return (
    <main className="items-center">
      <div className="">
        <Head>
          <title>Hydrascan</title>
          <meta name="description" content="Hydra Head Explorer" />
        </Head>

        <IntervalSettingProvider>
          <NetworkSettingProvider>
            <div className="flex flex-col items-center justify-center h-screen">
              <HeadsDataProvider>

                <div className="flex ">
                  <h1 className="text-3xl font-bold flex items-center">
                    <div className="mr-4">
                      <Image
                        src="/hydra.svg"
                        alt="Hydra Logo"
                        className="dark:invert"
                        width={50}
                        height={50}
                        priority
                      />
                    </div>
                    Hydrascan
                  </h1>
                  <div className="ml-10">
                    <div className="flex items-start space-x-4">
                      <div>
                        <NetworkSetter />
                      </div>
                    </div>
                  </div>
                  <div className="ml-10">
                    <HeadsDashboard />
                  </div>
                </div>

                <CardanoExplorerProvider>
                  <div className="flex items-start space-x-4">
                    <div>
                      <TickBox />
                    </div>
                    <div>
                      <IntervalSetter />
                    </div>

                  </div>
                  <HeadsTable />
                </CardanoExplorerProvider>


              </HeadsDataProvider>
            </div>
          </NetworkSettingProvider>
        </IntervalSettingProvider>
      </div>
    </main>
  )
}
