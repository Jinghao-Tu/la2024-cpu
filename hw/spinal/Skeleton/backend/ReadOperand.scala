package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class ReadOperandLogic(iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Component {
    val forwardPortNum = iqType match {
        case FUType.counter => config.aluForwardCount * 2 + config.lsuForwardCount // From ALU0, ALU1, LSU
        case FUType.csr => config.aluForwardCount * 2 + config.lsuForwardCount // From ALU0, ALU1, LSU
        case FUType.mulu => config.lsuForwardCount // From LSU
        case FUType.lsu => config.aluForwardCount * 2 + config.lsuForwardCount + config.muluForwardCount // From ALU0, ALU1, LSU, MULU
        case _ => 0
    }
    val io = new Bundle {
        val cmd = slave Stream(IssueQueueROIOBundle(iqType, config)) // 1-latency
        val toFU = master Stream(ROFUBundle(iqType, config)) // 1-latency
        val forward = if (forwardPortNum > 0) Vec.fill(forwardPortNum)(slave Flow(ForwardBundle(config))) else null // 0-latency
        val wakeOut = if (iqType == FUType.csr || iqType == FUType.counter) master(Flow(Bits(config.prfIdxWidth bits))) else null // 0-latency!
        val prf = Vec.fill(2)(master(PRFIOBundle(false, config)))
        val counter = if (iqType == FUType.counter) master(CounterReadBundle(config)) else null
        val csr = if (iqType == FUType.csr) master(CSRSwIOBundle(false, config)) else null
        val interrupt = in(Bool())
    }
    io.toFU.robIdx := io.cmd.robIdx
    io.toFU.branchInfo := io.cmd.branchInfo
    io.toFU.branchResult := io.cmd.branchResult
    if (iqType == FUType.csr || iqType == FUType.counter) {
        io.wakeOut.valid := io.cmd.valid
        io.wakeOut.payload := io.cmd.payload.prd
    }
    val interruptInfo = ExceptionInfo()
    interruptInfo.exception := True
    interruptInfo.eCode := ECode.INT.eCode
    interruptInfo.eSubCode := ECode.INT.eSubCode
    io.toFU.exceptionInfo := Mux(io.interrupt, interruptInfo, io.cmd.exceptionInfo)
    io.toFU.pc := io.cmd.pc
    io.toFU.prd := io.cmd.prd
    io.toFU.uop := io.cmd.uop
    io.toFU.valid := io.cmd.valid
    io.cmd.ready := io.toFU.ready // No latency here, so no special treatment needed for LSU
    val reg1 = UInt(config.wordLength bits)
    io.prf(0).idx := io.cmd.psrc(0)
    reg1 := io.prf(0).data
    if (forwardPortNum > 0) {
        (0 until forwardPortNum).map(i => {
            when (io.forward(i).idx === io.cmd.psrc(0) && io.forward(i).valid) {
                reg1 := io.forward(i).payload.payload
            }
        })
    }
    val reg2 = UInt(config.wordLength bits)
    io.prf(1).idx := io.cmd.psrc(1)
    reg2 := io.prf(1).data
    if (forwardPortNum > 0) {
        (0 until forwardPortNum).map(i => {
            when (io.forward(i).idx === io.cmd.psrc(1) && io.forward(i).valid) {
                reg2 := io.forward(i).payload.payload
            }
        })
    }
    val imm = io.cmd.imm
    val pc = io.cmd.pc
    val csr = UInt(config.wordLength bits)
    if (iqType == FUType.counter) {
        switch(io.cmd.roop.cruROOp) {
            is(CRUROOp.id) {
                csr := io.counter.id
            }
            is(CRUROOp.lo) {
                csr := io.counter.value(31 downto 0)
            }
            is(CRUROOp.hi) {
                csr := io.counter.value(63 downto 32)
            }
        }
    } else if (iqType == FUType.csr) {
        io.csr.address := imm(config.csrAddrLength-1 downto 0).asBits
        csr := io.csr.value.asUInt
    }
    
    if (iqType == FUType.counter || iqType == FUType.csr) {
        switch(io.cmd.roop.aluROOp) {
            is(ALUROOp.reg) {
                io.toFU.src1 := reg1
                io.toFU.src2 := reg2
                io.toFU.src3 := imm |<< 2
                io.toFU.src4 := pc
            }
            is(ALUROOp.regimm) {
                io.toFU.src1 := reg1
                io.toFU.src2 := imm
                io.toFU.src3 := imm
                io.toFU.src4 := pc
            }
            is(ALUROOp.pcimm) {
                io.toFU.src1 := pc
                io.toFU.src2 := imm
                io.toFU.src3 := imm
                io.toFU.src4 := pc
            }
            is(ALUROOp.csr) {
                io.toFU.src1 := csr
                io.toFU.src2 := imm
                io.toFU.src3 := reg2
                io.toFU.src4 := reg1
            }
            is(ALUROOp.linkpc) {
                io.toFU.src1 := pc
                io.toFU.src2 := U(4).resized
                io.toFU.src3 := imm |<< 2
                io.toFU.src4 := pc
            }
            is(ALUROOp.linkreg) {
                io.toFU.src1 := pc
                io.toFU.src2 := U(4).resized
                io.toFU.src3 := imm |<< 2
                io.toFU.src4 := reg1
            }
        }
    } else if (iqType == FUType.mulu || iqType == FUType.divu) {
        io.toFU.src1 := reg1
        io.toFU.src2 := reg2
    } else if (iqType == FUType.lsu) {
        io.toFU.src1 := reg1
        io.toFU.src3 := reg2
        switch(io.cmd.roop.lsuROOp) {
            is(LSUROOp.reg) {
                io.toFU.src2 := reg2
            }
            is(LSUROOp.regimm) {
                io.toFU.src2 := imm
            }
        }
    } else {
        assert(false)
    }
}