package Skeleton.config

import spinal.core._

case class CPUConfig() {
    // ISA related
    def wordLength = 32
    def arfSize = 32
    def arfIdxWidth = log2Up(arfSize)

    // Pipeline related
    def fetchWidth = 2
    def decodeWidth = 2
    def issueWidth = 5
    def writeBackWidth = issueWidth
    def retireWidth = 2

    def retireNumWidth = log2Up(retireWidth+1)
    
    // RegFile related
    def prfSize = 64
    def readPairNum = issueWidth
    def prfIdxWidth = log2Up(prfSize)
    def freeListSize = prfSize // No doubt here

    // MMU/TLB related
    def palen = 32
    def valen = 32
    def tlbSize = 4
}