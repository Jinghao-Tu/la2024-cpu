package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class BranchResult(config: CPUConfig) extends Bundle {
    val pc = if (config.debug) UInt(config.valen bits) else null
    val isJumpInst = Bool()
    // val isCallInst = Bool()
    // val isRetInst = Bool()
    val targetPC = UInt(config.valen bits)
    val taken = Bool()
    val predictFail = Bool()
    val GHR = UInt(config.ghrWidth bits)
    def resetVal: BranchResult = {
        val value = BranchResult(config)
        if (config.debug) value.pc := U(0).resized
        value.isJumpInst := False
        // value.isCallInst := False
        // value.isRetInst := False
        value.targetPC := U(0).resized
        value.taken := False
        value.predictFail := False
        value.GHR := U(0).resized
        return value
    }
}

case class BranchInfo(config: CPUConfig) extends Bundle {
    val pc = if (config.debug) UInt(config.valen bits) else null
    val predictTarget = UInt(config.valen bits)
    val predictTaken = Bool() // true: taken, false: not taken
    val predictJumpInst = Bool() // true: jump inst, false: not jump inst. Also include call and ret.
    val GHR = UInt(config.ghrWidth bits) // GHR value before this prediction
    // val rasSP = UInt(log2Up(config.rasStackDepth) bits) // sp value before this prediction
    // val rasTop = UInt(config.wordLength bits) // ras top value before this prediction
    // val rasCount = UInt(config.rasStackCounterWidth bits) // ras count value before this prediction
    def resetVal: BranchInfo = {
        val value = BranchInfo(config)
        if (config.debug) value.pc := U(0).resized
        value.predictTarget := U(0).resized
        value.predictTaken := False
        value.predictJumpInst := False
        value.GHR := U(0).resized
        // value.rasSP := U(0).resized
        // value.rasTop := U(0).resized
        // value.rasCount := U(0).resized
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