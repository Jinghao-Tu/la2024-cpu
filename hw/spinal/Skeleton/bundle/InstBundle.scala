package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class BranchResult(config: CPUConfig) extends Bundle {
    val targetPC = Bits(config.wordLength bits)
    val branchResult = Bool()
    val predictFail = Bool()
}

case class BranchInfo(config: CPUConfig) extends Bundle {
    val predictPC = Bits(config.wordLength bits)
    val predictResult = Bool()
}

case class ExceptionInfo() extends Bundle {
    val exception = Bool()
    val eCode = Bits(6 bits)
    val eSubCode = Bits(1 bits) // Optimized for LA32R
}