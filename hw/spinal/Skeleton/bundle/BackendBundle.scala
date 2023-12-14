package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class PRFIOBundle(isWrite: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: FUs
    // Slave: PRF
    val idx = Bits(config.prfIdxWidth bits)
    val data = UInt(config.wordLength bits)
    val wen = isWrite generate (Bool())

    def asMaster(): Unit = {
        if (isWrite) {
            out(data)
        } else {
            in(data)
        }
        out(idx, wen)
    }
}

case class RATIOBundle(isWrite: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Retire logic / Rename logic
    // Slave: RAT
    val ard = Bits(config.arfIdxWidth bits)
    val prd = Bits(config.prfIdxWidth bits)
    val wen = isWrite generate Bool()
    val valid = if (isWrite) null else Bool()

    def asMaster(): Unit = {
        if (isWrite) {
            out(prd)
        } else {
            in(prd)
        }
        in(valid)
        out(ard, wen)
    }
}

case class SRATEntry(config: CPUConfig) extends Bundle {
    val prfIdx = Bits(config.prfIdxWidth bits)
    val valid = Bool()
    def resetVal(): SRATEntry = {
        val value = SRATEntry(config)
        value.prfIdx := B"1'b0".resized
        value.valid := True
        return value
    }
}