package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class BranchResult(config: CPUConfig) extends Bundle {
    val targetPC = Bits(config.wordLength bits)
    val branchResult = Bool()
    val predictFail = Bool()
    def resetVal: BranchResult = {
        val value = BranchResult(config)
        value.targetPC := B(0).resized
        value.branchResult := False
        value.predictFail := False
        return value
    }
}

case class BranchInfo(config: CPUConfig) extends Bundle {
    val predictPC = Bits(config.wordLength bits)
    val predictResult = Bool()
    def resetVal: BranchInfo = {
        val value = BranchInfo(config)
        value.predictPC := B(0).resized
        value.predictResult := False
        return value
    }
}

case class ExceptionInfo() extends Bundle {
    val exception = Bool()
    val eCode = Bits(6 bits)
    val eSubCode = Bits(1 bits) // Optimized for LA32R
    def resetVal: ExceptionInfo = {
        val value = ExceptionInfo()
        value.exception := False
        value.eCode := B(0).resized
        value.eSubCode := B(0).resized
        return value
    }
}