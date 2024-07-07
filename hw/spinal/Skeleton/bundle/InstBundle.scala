package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class BranchResult(config: CPUConfig) extends Bundle {
    val targetPC = UInt(config.wordLength bits)
    val branchResult = Bool()
    val predictFail = Bool()
    def resetVal: BranchResult = {
        val value = BranchResult(config)
        value.targetPC := U(0).resized
        value.branchResult := False
        value.predictFail := False
        return value
    }
}

case class BranchInfo(config: CPUConfig) extends Bundle {
    val predictTarget = UInt(config.wordLength bits)
    val predictTaken = Bool() // true: taken, false: not taken
    val predictJumpInst = Bool() // true: jump inst, false: not jump inst. Also include call and ret.
    val GHR = UInt(config.ghrWidth bits) // GHR value before this prediction
    val sp = UInt(config.wordLength bits) // sp value before this prediction
    val rasTop = UInt(config.wordLength bits) // ras top value before this prediction
    def resetVal: BranchInfo = {
        val value = BranchInfo(config)
        value.predictTarget := U(0).resized
        value.predictTaken := False
        value.predictJumpInst := False
        value.GHR := U(0).resized
        value.sp := U(0).resized
        value.rasTop := U(0).resized
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