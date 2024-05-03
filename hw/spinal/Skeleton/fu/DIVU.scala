package Skeleton.fu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import Skeleton.bundle._
import Skeleton.config._
import Skeleton.backend.CommitLogic

case class DIVU(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(FUType.divu, config))
        val output = master Stream(FUWBBundle(config))
    }
    require(config.debug || config.divider == DividerType.restoring, "Unsupported divider type")
    if (config.debug) {
        val timeToCompute = 32
        val block = RegInit(False)
        val counter = Counter(0 to timeToCompute)
        block := block
        io.input.ready := ~block
        io.output.valid := False
        when (io.input.fire) {
            block := True
        }
        when (block) {
            counter.increment()
        }
        when (counter.willOverflowIfInc || (block && io.input.payload.exceptionInfo.exception)) { // Finished computing
            block := False
            io.output.valid := True
        }
        io.output.payload.robIdx := io.input.payload.robIdx
        io.output.payload.prd := io.input.payload.prd
        io.output.payload.branchResult := io.input.payload.branchResult
        io.output.payload.exceptionInfo := io.input.payload.exceptionInfo
        switch(io.input.payload.uop.divuOp) {
            is(DIVUOp.div ) { io.output.payload.data := (io.input.payload.src1.asSInt / io.input.payload.src2.asSInt).asUInt }
            is(DIVUOp.divu) { io.output.payload.data := io.input.payload.src1 / io.input.payload.src2 }
            is(DIVUOp.mod ) { io.output.payload.data := (io.input.payload.src1.asSInt % io.input.payload.src2.asSInt).asUInt.resized }
            is(DIVUOp.modu) { io.output.payload.data := (io.input.payload.src1 % io.input.payload.src2).resized }
        }
    } else if (config.divider == DividerType.restoring) {
        val size = Reg(UInt(log2Up(config.wordLength) bits))
        val quotient = Reg(UInt(config.wordLength bits))
        val remainder = Reg(UInt(config.wordLength*2 bits))
        val divisor = Reg(UInt(config.wordLength*2 bits))
        val quotientNegative = Reg(Bool())
        val remainderNegative = Reg(Bool())
        size.init(U(0).resized)
        quotient.init(U(0).resized)
        remainder.init(U(0).resized)
        quotientNegative.init(False)
        remainderNegative.init(False)

        val signed = io.input.payload.uop.divuOp === DIVUOp.div || io.input.payload.uop.divuOp === DIVUOp.mod
        val dividendAbs = (signed && io.input.payload.src1(config.wordLength-1)) ? (-io.input.payload.src1.asSInt).asUInt | io.input.payload.src1
        val divisorAbs = (signed && io.input.payload.src2(config.wordLength-1)) ? (-io.input.payload.src2.asSInt).asUInt | io.input.payload.src2

        val midRes = remainder - divisor
        val quotientNext = quotient(config.wordLength-2 downto 0) @@ (~midRes(config.wordLength*2-1))
        val remainderNext = midRes(config.wordLength*2-1) ? remainder | midRes

        val fsm = new StateMachine {
            val idle = new State with EntryPoint
            val busy = new State

            idle
                .whenIsActive {
                    when (io.input.valid) { // This is OK, a sample will be conducted on returning feom busy to idle, no duplicated calculation will be made
                        size := OHToUInt(OHMasking.last(dividendAbs))
                        quotient := U(0).resized
                        remainder := dividendAbs.resized
                        divisor := divisorAbs.resize(config.wordLength*2 bits) |<< OHToUInt(OHMasking.last(dividendAbs))
                        quotientNegative := signed && (io.input.payload.src1(config.wordLength-1) ^ io.input.payload.src2(config.wordLength-1))
                        remainderNegative := signed && io.input.payload.src1(config.wordLength-1)
                        goto(busy)
                    }
                }
            busy
                .whenIsActive {
                    when (size =/= 0) {
                        size := size - 1
                        quotient := quotientNext
                        remainder := remainderNext
                        divisor := divisor |>> 1
                    } otherwise {
                        when (io.output.fire) {
                            goto(idle)
                        }
                    }
                }
        }
        io.input.ready := io.output.fire
        io.output.valid := fsm.isActive(fsm.busy) && size === 0
        io.output.payload.robIdx := io.input.payload.robIdx
        io.output.payload.data := (io.input.payload.uop.divuOp === DIVUOp.div || io.input.payload.uop.divuOp === DIVUOp.divu) ? (quotientNegative ? (-quotientNext.asSInt).asUInt | quotientNext) | (remainderNegative ? (-remainderNext(config.wordLength-1 downto 0).asSInt).asUInt | remainderNext(config.wordLength-1 downto 0))
        io.output.payload.prd := io.input.payload.prd
        io.output.payload.branchResult := io.input.payload.branchResult
        io.output.payload.exceptionInfo := io.input.payload.exceptionInfo
    } else if (config.divider == DividerType.srt) {
        
    }
}