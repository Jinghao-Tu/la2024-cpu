package Skeleton.config

import spinal.core._

case class CPUConfig() {
    // ISA related
    def wordLength = 32
    def arfSize = 32
    def arfIdxWidth = log2Up(arfSize)
    def instLength = 32
    def csrAddrLength = 14
    def counterWidth = 64

    // Timer related
    def timerWidth = wordLength

    // Pipeline related
    def fetchWidth = 2
    def decodeWidth = 2
    def issueWidth = 5
    def writebackWidth = issueWidth
    def retireWidth = 2

    def retireNumWidth = log2Up(retireWidth+1)

    // Instruction Queue related
    def instrQueueSize = 8

    // ROB related
    def robSize = 32
    def robIdxWidth = log2Up(robSize)
    
    // RegFile related
    def prfSize = 64
    def readPairNum = issueWidth
    def prfIdxWidth = log2Up(prfSize)
    def freeListSize = prfSize // No doubt here

    // MMU/TLB related
    def palen = 32
    def valen = 32
    def tlbSize = 4
    def tlbSizeWidth = log2Up(tlbSize)

    // Cache related
    def iCacheTagWidth = palen - iCacheIdxWidth - iCacheOffsetWidth
    def iCacheWaySize = 4
    def iCacheBlockSize = 64
    def iCacheSize = 8192 // KiB
    def iCacheLineSize = iCacheSize / iCacheWaySize / iCacheBlockSize // Line number per way
    def iCacheSizePerWay = iCacheLineSize * iCacheBlockSize * 8 / axiDataWidth
    def iCacheIdxWidth = log2Up(iCacheLineSize)
    def iCacheOffsetWidth = log2Up(iCacheBlockSize)
    def iCacheBlockOffsetWidth = log2Up(axiDataWidth/8)
    def dCacheTagWidth = palen - dCacheIdxWidth - dCacheOffsetWidth
    def dCacheWaySize = 4
    def dCacheBlockSize = 64
    def dCacheSize = 8192 // KiB
    def dCacheLineSize = dCacheSize / dCacheWaySize / dCacheBlockSize // Line number per way
    def dCacheSizePerWay = dCacheLineSize * dCacheBlockSize * 8 / axiDataWidth
    def dCacheIdxWidth = log2Up(dCacheSizePerWay)
    def dCacheOffsetWidth = log2Up(dCacheBlockSize)
    def dCacheBlockOffsetWidth = log2Up(axiDataWidth/8)

    // AXI related
    def axiAddressWidth = palen
    def axiDataWidth = wordLength
    def axiIdWidth = 4
    def axiBlockBurstLength = iCacheBlockSize * 8 / axiDataWidth

    // Misc
    def debug = true
}