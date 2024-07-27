package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class BPUUpdateBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Retire logic
    // Slave: Branch predictor
    // Include 2 parts: DecodeInfo and BranchInfo
    val pc = UInt(config.wordLength bits)
    val branchResult = BranchResult(config)
    val branchInfo = BranchInfo(config)

    def asMaster(): Unit = {
        out(pc, branchResult, branchInfo)
    }
}

case class InstrQueueInBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: IQueue
    // Slave: I-Cache
    val allowMask = Bits(config.fetchWidth bits)
    val availMask = Bits(config.fetchWidth bits)
    val info = Vec.fill(config.fetchWidth)(InstrQueueEntry(config))

    def asMaster(): Unit = {
        in(availMask, info)
        out(allowMask)
    }
}

case class InstrQueueOutBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Dispatcher
    // Slave: IQueue
    val allowMask = Bits(config.decodeWidth bits)
    val availMask = Bits(config.decodeWidth bits)
    val info = Vec.fill(config.fetchWidth)(InstrQueueEntry(config))
    val dispatchInfo = Vec.fill(config.decodeWidth)(DispatchInfo(config))

    def asMaster(): Unit = {
        in(availMask, info, dispatchInfo)
        out(allowMask)
    }
}

case class InstrQueueEntry(config: CPUConfig) extends Bundle {
    val inst = Bits(config.instLength bits)
    val branchInfo = BranchInfo(config)
    val exceptionInfo = ExceptionInfo()
    val pc = UInt(config.wordLength bits)
}

case class DispatchInfo(config: CPUConfig) extends Bundle{
    val fuType = FUType()
    val ard = Bits(config.arfIdxWidth bits)
    val asrc = Vec.fill(2)(Bits(config.arfIdxWidth bits))
}

object FUType extends SpinalEnum {
    val alu, csr, counter, lsu, mulu, divu = newElement()
}