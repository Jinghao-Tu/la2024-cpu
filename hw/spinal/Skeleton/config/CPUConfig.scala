package Skeleton.config

import spinal.core._

case class CPUConfig() {
    // ISA related
    def wordLength = 32

    // Pipeline related
    def fetchWidth = 2
    def issueWidth = 5
    def retireWidth = 2
    
    // RegFile related
    def prfSize = 64
    def readPairNum = issueWidth
    def wbNum = issueWidth
    def prfIdxWidth = log2Up(prfSize)

    // MMU/TLB related
    def palen = 32
    def valen = 32
    def tlbSize = 4
}