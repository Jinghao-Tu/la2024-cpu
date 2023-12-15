package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class FreeListDispatchIOBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Dispatcher
    // Slave: Free List
    val allowMask = Bits(config.decodeWidth bits) // LSB has priority
    val availMask = Bits(config.decodeWidth bits) // LSB has priority
    val prfIdx = Vec.fill(config.decodeWidth)(Bits(config.prfIdxWidth bits))

    def asMaster(): Unit = {
        in(availMask, prfIdx)
        out(allowMask)
    }
}

case class FreeListRetireIOBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Retire logic
    // Slave: Free list
    val prfIdx = Vec.fill(config.retireWidth)(Bits(config.prfIdxWidth bits))
    val writeNum = UInt(config.retireNumWidth bits)
    val flush = Bool()

    def asMaster(): Unit = {
        out(prfIdx, writeNum, flush)
    }
}

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

case class RATIOBundle(isWrite: Boolean, isUpdate: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Retire logic / Rename logic
    // Slave: RAT
    val ard = if(isUpdate) null else Bits(config.arfIdxWidth bits)
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