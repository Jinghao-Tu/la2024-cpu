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
    def resetVector = 0x1C000000

    // Timer related
    def timerWidth = wordLength

    // Pipeline related
    def fetchWidth = 2
    def decodeWidth = 2
    def issueWidth = 5
    def writebackWidth = issueWidth
    def retireWidth = 2

    def retireNumWidth = log2Up(retireWidth+1)

    def aluWakeCount = 3 // 1 Issue Queue, 1 RO, 1 ALU
    def muluWakeCount = 3
    def divuWakeCount = 0
    def lsuWakeCount = 2
    
    def aluForwardCount = 2 // 1 ALU, 1 Commit
    def muluForwardCount = 2 // 1 MULU, 1 Commit
    def divuForwardCount = 0
    def lsuForwardCount = 1

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
    def iCacheSize = 8192
    def iCacheLineSize = iCacheSize / iCacheWaySize / iCacheBlockSize // Line number per way
    def iCacheSizePerWay = iCacheLineSize * iCacheBlockSize * 8 / axiDataWidth // How many words a way has
    def iCacheIdxWidth = log2Up(iCacheLineSize)
    def iCacheOffsetWidth = log2Up(iCacheBlockSize)
    def iCacheBlockOffsetWidth = log2Up(axiDataWidth/8)
    def dCacheTagWidth = palen - dCacheIdxWidth - dCacheOffsetWidth
    def dCacheWaySize = 4
    def dCacheBlockSize = 64
    def dCacheSize = 8192
    def dCacheLineSize = dCacheSize / dCacheWaySize / dCacheBlockSize // Line number per way
    def dCacheSizePerWay = dCacheLineSize * dCacheBlockSize * 8 / axiDataWidth // How many words a way has
    def dCacheIdxWidth = log2Up(dCacheLineSize)
    def dCacheOffsetWidth = log2Up(dCacheBlockSize)
    def dCacheBlockOffsetWidth = log2Up(axiDataWidth/8)

    def dCacheMissBufferSize = 1
    def dCacheWriteBufferSize = 8

    // AXI related
    def axiAddressWidth = palen
    def axiDataWidth = wordLength
    def axiIdWidth = 4
    def axiBlockBurstLength = iCacheBlockSize * 8 / axiDataWidth - 1

    // Misc
    def debug = true

    // Tage Predictor
    def predictorTagWidth = 8
    def btbValidWidth = 1
    def btbTagWidth = predictorTagWidth
    def btbtargetWidth = 32
    def btbWidth = btbValidWidth + btbTagWidth + btbtargetWidth
    def btbSize = 64
    def bhtWidth = 2
    def bhtSize = 1024
    def phtCounterWidth = 3
    def phtTagWidth = predictorTagWidth
    def phtUsefulWidth = 2
    def phtWidth = phtCounterWidth + phtTagWidth + phtUsefulWidth
    def phtNum = 4
    def phtSize = 1024
    def ghrWidth = (1 << (phtNum - 1)) * 10
    
    // Ras Predictor
    def rasTableWidth = 9
    def rasTableSize = 1024
    def rasStackDepth = 64
    def rasStackWidth = 32
    def rasTagWidth = predictorTagWidth
    def rasStackCounterWidth = 8
}