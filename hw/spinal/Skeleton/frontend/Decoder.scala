package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class Decoder(config: CPUConfig) extends Component {
    val io = new Bundle {
        val info = in(InstrQueueEntry(config))
        val plv = in(CSRBundle(config).crmd.plv)
        val branchInfo = out(BranchInfo(config))
        val branchResult = out(BranchResult(config))
        val exceptionInfo = out(ExceptionInfo())
        val pc = out(UInt(config.wordLength bits))
        val specialOp = out(ROBSpecialOp())
        val imm = out(UInt(config.wordLength bits))
        val uopALU0 = out(uopBundle(FUType.csr, config))
        val uopALU1 = out(uopBundle(FUType.counter, config))
        val uopMULU = out(uopBundle(FUType.mulu, config))
        val uopDIVU = out(uopBundle(FUType.divu, config))
        val uopLSU = out(uopBundle(FUType.lsu, config))
        val roopALU0 = out(roopBundle(FUType.csr))
        val roopALU1 = out(roopBundle(FUType.counter))
        val roopLSU = out(roopBundle(FUType.lsu))
    }
    val privileged = Bool()
    val illegal = Bool()
    val break = Bool()
    val syscall = Bool()
    // By default these should be false
    privileged := False
    illegal := False
    break := False
    syscall := False

    // Exception info is updated only when no exception has happened
    when (~io.info.exceptionInfo.exception) {
        when (privileged && io.plv =/= B(0).resized) {
            io.exceptionInfo.exception := True
            io.exceptionInfo.eCode := ECode.IPE.eCode
            io.exceptionInfo.eSubCode := ECode.IPE.eSubCode
        } elsewhen (illegal) {
            io.exceptionInfo.exception := True
            io.exceptionInfo.eCode := ECode.INE.eCode
            io.exceptionInfo.eSubCode := ECode.INE.eSubCode
        } elsewhen (break) {
            io.exceptionInfo.exception := True
            io.exceptionInfo.eCode := ECode.BRK.eCode
            io.exceptionInfo.eSubCode := ECode.BRK.eSubCode
        } elsewhen (syscall) {
            io.exceptionInfo.exception := True
            io.exceptionInfo.eCode := ECode.SYS.eCode
            io.exceptionInfo.eSubCode := ECode.SYS.eSubCode
        } otherwise {
            io.exceptionInfo := io.info.exceptionInfo
        }
    } otherwise {
        io.exceptionInfo := io.info.exceptionInfo
    }

    // BPU does not care about targetpc when actual direction is not taken/not a branch
    // branchResult here is for non-ALU insts, thus just make branchResult not taken and set predictFail as needed
    if (config.debug) io.branchResult.pc := io.info.branchInfo.pc
    io.branchResult.isJumpInst := False
    io.branchResult.targetPC := io.info.pc + config.instLength / 8
    io.branchResult.taken := False
    // io.branchResult.predictFail := io.info.branchInfo.predictTaken
    io.branchResult.predictFail := io.info.branchInfo.predictJumpInst
    io.branchResult.GHR := io.info.branchInfo.GHR
    // branchInfo here is for ALU insts, non-branch insts will be handled in BRU
    io.branchInfo := io.info.branchInfo

    io.pc := io.info.pc

    val imm8 = sext(17, 10)
    val imm12 = sext(21, 10)
    val imm14 = sext(23, 10) |<< 2 // For LL, SC
    val imm16 = sext(25, 10)
    val imm21 = sext(4, 0, 25, 10)
    val imm26 = sext(9, 0, 25, 10)
    val uimm5 = zext(14, 10)
    val uimm12 = zext(21, 10)
    val uimm14 = zext(23, 10) // For CSR
    val immu20 = sextu(24, 5)
    val lsuCoOp = io.info.inst(4 downto 0)

    // Set some default value for operands not using
    io.specialOp := ROBSpecialOp.nop
    io.imm := imm8
    io.uopALU0 := uopBundle(FUType.csr, config).resetVal
    io.uopALU1 := uopBundle(FUType.counter, config).resetVal
    io.uopMULU := uopBundle(FUType.mulu, config).resetVal
    io.uopDIVU := uopBundle(FUType.divu, config).resetVal
    io.uopLSU := uopBundle(FUType.lsu, config).resetVal
    io.roopALU0 := roopBundle(FUType.csr).resetVal
    io.roopALU1 := roopBundle(FUType.counter).resetVal
    io.roopLSU := roopBundle(FUType.lsu).resetVal

    // OK, do dirty works now
    switch(io.info.inst) {
        is(Insts.RDCNTID_W) {
            io.uopALU1.aluOp := ALUOp.passa
            io.roopALU1.aluROOp := ALUROOp.csr
            io.roopALU1.cruROOp := CRUROOp.id
        }
        is(Insts.RDCNTVL_W) {
            io.uopALU1.aluOp := ALUOp.passa
            io.roopALU1.aluROOp := ALUROOp.csr
            io.roopALU1.cruROOp := CRUROOp.lo
        }
        is(Insts.RDCNTVH_W) {
            io.uopALU1.aluOp := ALUOp.passa
            io.roopALU1.aluROOp := ALUROOp.csr
            io.roopALU1.cruROOp := CRUROOp.hi
        }
        is(Insts.ADD_W) {
            io.uopALU0.aluOp := ALUOp.add
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.add
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.SUB_W) {
            io.uopALU0.aluOp := ALUOp.sub
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.sub
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.SLT) {
            io.uopALU0.aluOp := ALUOp.slt
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.slt
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.SLTU) {
            io.uopALU0.aluOp := ALUOp.sltu
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.sltu
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.NOR) {
            io.uopALU0.aluOp := ALUOp.nor
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.nor
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.AND) {
            io.uopALU0.aluOp := ALUOp.and
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.and
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.OR) {
            io.uopALU0.aluOp := ALUOp.or
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.or
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.XOR) {
            io.uopALU0.aluOp := ALUOp.xor
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.xor
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.SLL_W) {
            io.uopALU0.aluOp := ALUOp.sll
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.sll
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.SRL_W) {
            io.uopALU0.aluOp := ALUOp.srl
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.srl
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.SRA_W) {
            io.uopALU0.aluOp := ALUOp.sra
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.sra
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.MUL_W) {
            io.uopMULU.muluOp := MULUOp.mullo
        }
        is(Insts.MULH_W) {
            io.uopMULU.muluOp := MULUOp.mulhi
        }
        is(Insts.MULH_WU) {
            io.uopMULU.muluOp := MULUOp.mulhiu
        }
        is(Insts.DIV_W) {
            io.uopDIVU.divuOp := DIVUOp.div
        }
        is(Insts.MOD_W) {
            io.uopDIVU.divuOp := DIVUOp.mod
        }
        is(Insts.DIV_WU) {
            io.uopDIVU.divuOp := DIVUOp.divu
        }
        is(Insts.MOD_WU) {
            io.uopDIVU.divuOp := DIVUOp.modu
        }
        is(Insts.BREAK) {
            break := True
        }
        is(Insts.SYSCALL) {
            syscall := True
        }
        is(Insts.SLLI_W) {
            io.imm := uimm5
            io.uopALU0.aluOp := ALUOp.sll
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.sll
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.SRLI_W) {
            io.imm := uimm5
            io.uopALU0.aluOp := ALUOp.srl
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.srl
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.SRAI_W) {
            io.imm := uimm5
            io.uopALU0.aluOp := ALUOp.sra
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.sra
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.SLTI) {
            io.imm := imm12
            io.uopALU0.aluOp := ALUOp.slt
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.slt
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.SLTUI) {
            io.imm := imm12
            io.uopALU0.aluOp := ALUOp.sltu
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.sltu
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.ADDI_W) {
            io.imm := imm12
            io.uopALU0.aluOp := ALUOp.add
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.add
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.ANDI) {
            io.imm := uimm12
            io.uopALU0.aluOp := ALUOp.and
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.and
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.ORI) {
            io.imm := uimm12
            io.uopALU0.aluOp := ALUOp.or
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.or
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.XORI) {
            io.imm := uimm12
            io.uopALU0.aluOp := ALUOp.xor
            io.roopALU0.aluROOp := ALUROOp.regimm
            io.uopALU1.aluOp := ALUOp.xor
            io.roopALU1.aluROOp := ALUROOp.regimm
        }
        is(Insts.CSR) {
            privileged := True
            io.imm := uimm14
            io.uopALU0.aluOp := ALUOp.passa
            when (io.info.inst(9 downto 5) === B(1).resized) { // CSRWR
                io.uopALU0.cruOp := CRUOp.pass
            } elsewhen (io.info.inst(9 downto 5) =/= B(0).resized) { // CSRXCHG
                io.uopALU0.cruOp := CRUOp.mask
            }
            io.roopALU0.aluROOp := ALUROOp.csr
            when (io.info.inst(9 downto 5) =/= B(0).resized) {
                io.specialOp := ROBSpecialOp.writeCSR
            }
        }
        is(Insts.CACOP) {
            when (lsuCoOp(4 downto 3) =/= B(2).resized) { // I think HIT CACOP means hit invalidate
                privileged := True
            }
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.cacop
            io.uopLSU.lsuCoOp := lsuCoOp
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.lsuAction
        }
        is(Insts.TLBSRCH) {
            privileged := True
            io.uopLSU.lsuOp := LSUOp.tlbsrch
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.lsuAction
        }
        is(Insts.TLBRD) {
            privileged := True
            io.uopLSU.lsuOp := LSUOp.tlbrd
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.lsuAction
        }
        is(Insts.TLBWR) {
            privileged := True
            io.uopLSU.lsuOp := LSUOp.tlbwr
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.lsuAction
        }
        is(Insts.TLBFILL) {
            privileged := True
            io.uopLSU.lsuOp := LSUOp.tlbfill
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.lsuAction
        }
        is(Insts.ERTN) {
            privileged := True
            io.specialOp := ROBSpecialOp.ertn
        }
        is(Insts.IDLE) {
            privileged := True
            io.specialOp := ROBSpecialOp.idle
        }
        is(Insts.INVTLB) {
            privileged := True
            io.uopLSU.lsuOp := LSUOp.invtlb
            io.uopLSU.lsuCoOp := lsuCoOp
            io.roopLSU.lsuROOp := LSUROOp.reg
            io.specialOp := ROBSpecialOp.lsuAction
            when (lsuCoOp.asUInt > U(6)) {
                illegal := True
            }
        }
        is(Insts.LU12I_W) {
            io.imm := immu20
            io.uopALU0.aluOp := ALUOp.passb
            io.roopALU0.aluROOp := ALUROOp.pcimm
            io.uopALU1.aluOp := ALUOp.passb
            io.roopALU1.aluROOp := ALUROOp.pcimm
        }
        is(Insts.PCADDU12I) {
            io.imm := immu20
            io.uopALU0.aluOp := ALUOp.add
            io.roopALU0.aluROOp := ALUROOp.pcimm
            io.uopALU1.aluOp := ALUOp.add
            io.roopALU1.aluROOp := ALUROOp.pcimm
        }
        is(Insts.LL_W) {
            io.imm := imm14
            io.uopLSU.lsuOp := LSUOp.ll
            io.uopLSU.lsuCoOp := LSUSizeOp.word.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.ll
        }
        is(Insts.SC_W) {
            io.imm := imm14
            io.uopLSU.lsuOp := LSUOp.sc
            io.uopLSU.lsuCoOp := LSUSizeOp.word.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.LD_B) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.ld
            io.uopLSU.lsuCoOp := LSUSizeOp.byte.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.LD_H) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.ld
            io.uopLSU.lsuCoOp := LSUSizeOp.halfword.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.LD_W) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.ld
            io.uopLSU.lsuCoOp := LSUSizeOp.word.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.ST_B) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.st
            io.uopLSU.lsuCoOp := LSUSizeOp.byte.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.ST_H) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.st
            io.uopLSU.lsuCoOp := LSUSizeOp.halfword.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.ST_W) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.st
            io.uopLSU.lsuCoOp := LSUSizeOp.word.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.LD_BU) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.ldu
            io.uopLSU.lsuCoOp := LSUSizeOp.byte.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.LD_HU) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.ldu
            io.uopLSU.lsuCoOp := LSUSizeOp.halfword.asBits.resized
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.PRELD) {
            io.imm := imm12
            io.uopLSU.lsuOp := LSUOp.preld
            io.uopLSU.lsuCoOp := lsuCoOp
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.DBAR) { // NOP
            io.uopLSU.lsuOp := LSUOp.dbar
            io.uopLSU.lsuCoOp := lsuCoOp // Not valid
            io.roopLSU.lsuROOp := LSUROOp.regimm
        }
        is(Insts.IBAR) {
            io.uopLSU.lsuOp := LSUOp.ibar
            io.uopLSU.lsuCoOp := lsuCoOp // Not valid
            io.roopLSU.lsuROOp := LSUROOp.regimm
            io.specialOp := ROBSpecialOp.lsuAction
        }
        is(Insts.JIRL) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.add
            io.uopALU0.bruOp := BRUOp.add
            io.roopALU0.aluROOp := ALUROOp.linkreg
            io.uopALU1.aluOp := ALUOp.add
            io.uopALU1.bruOp := BRUOp.add
            io.roopALU1.aluROOp := ALUROOp.linkreg
        }
        is(Insts.B) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm26
            io.uopALU0.bruOp := BRUOp.add
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.bruOp := BRUOp.add
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.BL) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm26
            io.uopALU0.aluOp := ALUOp.add
            io.uopALU0.bruOp := BRUOp.add
            io.roopALU0.aluROOp := ALUROOp.linkpc
            io.uopALU1.aluOp := ALUOp.add
            io.uopALU1.bruOp := BRUOp.add
            io.roopALU1.aluROOp := ALUROOp.linkpc
        }
        is(Insts.BEQ) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.eq
            io.uopALU0.bruOp := BRUOp.cadd
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.eq
            io.uopALU1.bruOp := BRUOp.cadd
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.BNE) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.eq
            io.uopALU0.bruOp := BRUOp.ncadd
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.eq
            io.uopALU1.bruOp := BRUOp.ncadd
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.BLT) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.slt
            io.uopALU0.bruOp := BRUOp.cadd
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.slt
            io.uopALU1.bruOp := BRUOp.cadd
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.BGE) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.slt
            io.uopALU0.bruOp := BRUOp.ncadd
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.slt
            io.uopALU1.bruOp := BRUOp.ncadd
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.BLTU) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.sltu
            io.uopALU0.bruOp := BRUOp.cadd
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.sltu
            io.uopALU1.bruOp := BRUOp.cadd
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        is(Insts.BGEU) {
            io.specialOp := ROBSpecialOp.bpuUpdate
            io.imm := imm16
            io.uopALU0.aluOp := ALUOp.sltu
            io.uopALU0.bruOp := BRUOp.ncadd
            io.roopALU0.aluROOp := ALUROOp.reg
            io.uopALU1.aluOp := ALUOp.sltu
            io.uopALU1.bruOp := BRUOp.ncadd
            io.roopALU1.aluROOp := ALUROOp.reg
        }
        default {
            illegal := True
        }
    }

    def sext(hi: Int, lo: Int): UInt = {
        return ((io.info.inst(hi) #* (config.instLength-hi+lo-1)) ## io.info.inst(hi downto lo)).asUInt
    }
    def sext(hhi: Int, hlo: Int, lhi: Int, llo: Int): UInt = {
        return ((io.info.inst(hhi) #* (config.instLength-hhi-lhi+hlo+llo-2)) ## io.info.inst(hhi downto hlo) ## io.info.inst(lhi downto llo)).asUInt
    }
    def zext(hi: Int, lo: Int): UInt = {
        return (io.info.inst(hi downto lo).resize(config.instLength)).asUInt
    }
    def sextu(hi: Int, lo: Int): UInt = {
        return (io.info.inst(hi downto lo) ## B(0).resize(config.instLength-hi+lo-1)).asUInt
    }
}