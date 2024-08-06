package Skeleton.fu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import Skeleton.bundle._
import Skeleton.config._

case class DIVU(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(FUType.divu, config))
        val output = master Stream(FUWBBundle(config))
    }
    require(config.debug || config.divider == DividerType.restoring, "Unsupported divider type")
    if (config.divider == DividerType.restoring) {
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
        val divisorAbs = (signed && io.input.payload.src2(config.wordLength-1)) ? (-io.input.payload.src2.asSInt).asUInt | io.input.payload.src2
        val dividendNeg = Reg(UInt(config.wordLength bits))
        val divisorAbsLatch = Reg(UInt(config.wordLength bits))
        dividendNeg.init(U(0).resized)
        divisorAbsLatch.init(U(0).resized)
        dividendNeg := (-io.input.payload.src1.asSInt).asUInt
        divisorAbsLatch := divisorAbs

        val midRes = remainder - divisor
        val quotientNext = quotient(config.wordLength-2 downto 0) @@ (~midRes(config.wordLength*2-1))
        val remainderNext = midRes(config.wordLength*2-1) ? remainder | midRes

        val fsm = new StateMachine {
            val idle = new State with EntryPoint
            val signCorrection = new State
            val busy = new State

            idle
                .whenIsActive {
                    when (io.input.valid) { // This is OK, a sample will be conducted on returning from busy to idle, no duplicated calculation will be made
                        size := OHToUInt(OHMasking.last(io.input.payload.src1))
                        quotient := U(0).resized
                        remainder := io.input.payload.src1.resized
                        divisor := divisorAbs.resize(config.wordLength*2 bits) |<< OHToUInt(OHMasking.last(io.input.payload.src1))
                        quotientNegative := signed && (io.input.payload.src1(config.wordLength-1) ^ io.input.payload.src2(config.wordLength-1))
                        remainderNegative := signed && io.input.payload.src1(config.wordLength-1)
                        when (signed && io.input.payload.src1(config.wordLength-1)) {
                            goto(signCorrection)
                        } otherwise {
                            goto(busy)
                        }
                    }
                }
            signCorrection // For timing relaxing, breaking up critical path on dividend width calculation
                .whenIsActive {
                    size := OHToUInt(OHMasking.last(dividendNeg))
                    remainder := dividendNeg.resized
                    divisor := divisorAbsLatch.resize(config.wordLength*2 bits) |<< OHToUInt(OHMasking.last(dividendNeg))
                    goto(busy)
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