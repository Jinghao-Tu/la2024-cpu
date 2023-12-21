package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class ForwardBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // 0-latency Flow!
    // Master: FUs
    // Slave: Operand reading logic
    val idx = Bits(config.prfIdxWidth bits)
    val payload = UInt(config.wordLength bits)

    def asMaster(): Unit = {
        out(idx, payload)
    }
}

case class FUWBBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // 1-latency Stream!
    // Master: FUs
    // Slave: Commit logic
    val robIdx = Bits(config.robIdxWidth bits)
    val data = UInt(config.wordLength bits)
    val prd = Bits(config.prfIdxWidth bits)
    val branchResult = BranchResult(config)
    val exceptionInfo = ExceptionInfo()

    def asMaster(): Unit = {
        out(robIdx, data, prd, branchResult, exceptionInfo)
    }
}

case class ROFUBundle(iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Bundle with IMasterSlave {
    // 1-latency Stream!
    // Master: Operand reading logic
    // Slave: FUs
    val src1 = UInt(config.wordLength bits)
    val src2 = UInt(config.wordLength bits)
    val src3 = if (iqType == FUType.counter || iqType == FUType.csr) UInt(config.wordLength bits) else null // LSU src3 is included in uop bundle
    val src4 = if (iqType == FUType.counter || iqType == FUType.csr) UInt(config.wordLength bits) else null
    val robIdx = Bits(config.robIdxWidth bits)
    val branchInfo = if (iqType == FUType.counter || iqType == FUType.csr) BranchInfo(config) else null
    val branchResult = if (iqType == FUType.counter || iqType == FUType.csr) null else BranchResult(config)
    val exceptionInfo = ExceptionInfo()
    val pc = UInt(config.wordLength bits)
    val prd = Bits(config.prfIdxWidth bits)
    val uop = uopBundle(iqType, config)

    def asMaster(): Unit = {
        out(src1, src2, src3, src4, branchInfo, exceptionInfo, pc, prd, uop)
    }
}

case class IssueQueueDispatchIOBundle(iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Bundle with IMasterSlave {
    // 0-latency Stream!
    // Master: Dispatcher
    // Slave: Issue Queue
    val robIdx = Bits(config.robIdxWidth bits)
    val branchInfo = if (iqType == FUType.counter || iqType == FUType.csr) BranchInfo(config) else null
    val branchResult = if (iqType == FUType.counter || iqType == FUType.csr) null else BranchResult(config)
    val exceptionInfo = ExceptionInfo()
    val pc = UInt(config.wordLength bits)
    val prd = Bits(config.prfIdxWidth bits)
    val psrc = Vec.fill(2)(Bits(config.prfIdxWidth bits))
    val imm = UInt(config.wordLength bits)
    val uop = uopBundle(iqType, config)
    val roop = if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) roopBundle(iqType) else null
    val srcReady = Vec.fill(2)(Bool())

    def asMaster(): Unit = {
        out(robIdx, branchInfo, exceptionInfo, pc, prd, psrc, imm, uop, roop, srcReady)
    }
}

case class IssueQueueROIOBundle(iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Bundle with IMasterSlave {
    // 1-latency Stream!
    // Master: Issue Queue
    // Slave: Operand reading logic
    val robIdx = Bits(config.robIdxWidth bits)
    val branchInfo = if (iqType == FUType.counter || iqType == FUType.csr) BranchInfo(config) else null
    val branchResult = if (iqType == FUType.counter || iqType == FUType.csr) null else BranchResult(config)
    val exceptionInfo = ExceptionInfo()
    val pc = UInt(config.wordLength bits)
    val prd = Bits(config.prfIdxWidth bits)
    val psrc = Vec.fill(2)(Bits(config.prfIdxWidth bits))
    val imm = UInt(config.wordLength bits)
    val uop = uopBundle(iqType, config)
    val roop = if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) roopBundle(iqType) else null

    def asMaster(): Unit = {
        out(robIdx, branchInfo, exceptionInfo, pc, prd, psrc, imm, uop, roop)
    }
}

case class IssueQueueEntry(iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Bundle {
    val valid = Bool()
    val robIdx = Bits(config.robIdxWidth bits)
    val branchInfo = if (iqType == FUType.counter || iqType == FUType.csr) BranchInfo(config) else null
    val branchResult = if (iqType == FUType.counter || iqType == FUType.csr) null else BranchResult(config)
    val exceptionInfo = ExceptionInfo()
    val pc = UInt(config.wordLength bits)
    val prd = Bits(config.prfIdxWidth bits)
    val psrc = Vec.fill(2)(Bits(config.prfIdxWidth bits))
    val imm = UInt(config.wordLength bits)
    val uop = uopBundle(iqType, config)
    val roop = if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) roopBundle(iqType) else null
    val srcReady = Vec.fill(2)(Bool())
    val srcWakeup = if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.mulu || iqType == FUType.lsu) Vec.fill(2)(Bool()) else null // DIVU does not have forward logic
    def resetVal: IssueQueueEntry = {
        val value = IssueQueueEntry(iqType, config)
        value.valid := False
        value.robIdx := B(0).resized
        if (iqType == FUType.counter || iqType == FUType.csr) value.branchInfo := BranchInfo(config).resetVal
        else value.branchResult := BranchResult(config).resetVal
        value.exceptionInfo := ExceptionInfo().resetVal
        value.pc := U(0).resized
        value.prd := B(0).resized
        value.psrc.foreach(entry => { entry := B(0).resized })
        value.imm := U(0).resized
        value.uop := uopBundle(iqType, config).resetVal
        if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) value.roop := roopBundle(iqType).resetVal
        value.srcReady.foreach(entry => { entry := False })
        if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.mulu || iqType == FUType.lsu) value.srcWakeup.foreach(entry => { entry := False })
        return value
    }
}

case class uopBundle(iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Bundle {
    val aluOp = if (iqType == FUType.counter || iqType == FUType.csr) ALUOp() else null
    val bruOp = if (iqType == FUType.counter || iqType == FUType.csr) BRUOp() else null
    val cruOp = if (iqType == FUType.csr) CRUOp() else null
    val muluOp = if (iqType == FUType.mulu) MULUOp() else null
    val divuOp = if (iqType == FUType.divu) DIVUOp() else null
    val lsuOp = if (iqType == FUType.lsu) LSUOp() else null
    val lsuCoOp = if (iqType == FUType.lsu) Bits(5 bits) else null // LSUSize, or inst hint
    def resetVal: uopBundle = {
        val value = uopBundle(iqType, config)
        iqType match {
            case FUType.counter => {
                value.aluOp := ALUOp.add
                value.bruOp := BRUOp.nop
            }
            case FUType.csr => {
                value.aluOp := ALUOp.add
                value.bruOp := BRUOp.nop
                value.cruOp := CRUOp.nop
            }
            case FUType.mulu => {
                value.muluOp := MULUOp.mullo
            }
            case FUType.divu => {
                value.divuOp := DIVUOp.div
            }
            case FUType.lsu => {
                value.lsuOp := LSUOp.dbar
                value.lsuCoOp := B(0).resized
            }
        }
        return value
    }
}

case class roopBundle(iqType: SpinalEnumElement[FUType.type]) extends Bundle {
    val aluROOp = if (iqType == FUType.counter || iqType == FUType.csr) ALUROOp() else null
    val lsuROOp = if (iqType == FUType.lsu) LSUROOp() else null
    val cruROOp = if (iqType == FUType.counter) CRUROOp() else null
    def resetVal: roopBundle = {
        val value = roopBundle(iqType)
        iqType match {
            case FUType.counter => {
                value.aluROOp := ALUROOp.reg
                value.cruROOp := CRUROOp.id
            }
            case FUType.csr => {
                value.aluROOp := ALUROOp.reg
            }
            case FUType.lsu => {
                value.lsuROOp := LSUROOp.regimm
            }
        }
        return value
    }
}

object ALUOp extends SpinalEnum {
    val add, sub, slt, sltu, eq, nor, and, or, xor, sll, srl, sra, passa, passb = newElement()
}

object BRUOp extends SpinalEnum {
    val nop, add, cadd, ncadd = newElement() // NOP means component not active
}

object CRUOp extends SpinalEnum {
    val nop, pass, mask = newElement() // NOP means component not active
}

object ALUROOp extends SpinalEnum {
    val reg, regimm, pcimm, csr, linkpc, linkreg = newElement()
}

object CRUROOp extends SpinalEnum {
    val id, lo, hi = newElement()
}

object MULUOp extends SpinalEnum {
    val mullo, mulhi, mulhiu = newElement()
}

object DIVUOp extends SpinalEnum {
    val div, divu, mod, modu = newElement()
}

object LSUOp extends SpinalEnum {
    val cacop, tlbsrch, tlbrd, tlbwr, tlbfill, invtlb, ll, sc, ld, ldu, st, preld, dbar, ibar = newElement()
}

object LSUSizeOp extends SpinalEnum { // Cast to bits when used to share data path
    val byte, halfword, word = newElement()
}

object LSUROOp extends SpinalEnum {
    val reg, regimm = newElement()
}

object ROBSpecialOp extends SpinalEnum {
    val nop, writeBufferWakeup, cacop, tlb, ll, sc, writeCSR, ertn, idle = newElement()
}

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
    // Master: Operand read logic / Commit logic
    // Slave: PRF
    val idx = Bits(config.prfIdxWidth bits)
    val data = UInt(config.wordLength bits)

    def asMaster(): Unit = {
        if (isWrite) {
            out(data)
        } else {
            in(data)
        }
        out(idx)
    }
}

case class RATIOBundle(isWrite: Boolean, isUpdate: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Retire logic / Rename logic
    // Slave: RAT
    if(isUpdate) require(isWrite)
    val ard = if(isUpdate) null else Bits(config.arfIdxWidth bits)
    val prd = Bits(config.prfIdxWidth bits)
    val wen = (isWrite || isUpdate) generate Bool()
    val valid = if (isWrite || isUpdate) null else Bool()

    def asMaster(): Unit = {
        if (isWrite || isUpdate) {
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
    def resetVal: SRATEntry = {
        val value = SRATEntry(config)
        value.prfIdx := B"1'b0".resized
        value.valid := True
        return value
    }
}