package Skeleton.fu

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class MULU(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(FUType.mulu, config))
        val output = master Stream(FUWBBundle(config))
        val forward = master Flow(ForwardBundle(config)) // 0-latency!
        val wakeOut = master Flow(Bits(config.prfIdxWidth bits)) // 0-latency!
    }
    io.wakeOut.valid := io.input.valid
    io.wakeOut.payload := io.input.payload.prd
    // Stage 1
    val stage12 = Stream(ROFUBundle(FUType.mulu, config))
    stage12 <-< io.input
    // Stage 2
    val stage23 = Stream(ROFUBundle(FUType.mulu, config))
    stage23 <-< stage12
    // Stage 3
    val resu = stage23.payload.src1 * stage23.payload.src2
    val ress = stage23.payload.src1.asSInt * stage23.payload.src2.asSInt
    val res = UInt(config.wordLength bits)
    io.forward.valid := stage23.valid
    io.forward.payload.idx := stage23.payload.prd
    io.forward.payload.payload := res
    io.output.valid := stage23.valid
    stage23.ready := io.output.ready
    io.output.payload.robIdx := stage23.payload.robIdx
    io.output.payload.data := res
    io.output.payload.prd := stage23.payload.prd
    io.output.payload.branchResult := stage23.payload.branchResult
    io.output.payload.exceptionInfo := stage23.payload.exceptionInfo
    switch(stage23.payload.uop.muluOp) {
        is(MULUOp.mullo) { res := resu(31 downto 0) }
        is(MULUOp.mulhi) { res := ress(63 downto 32).asUInt }
        is(MULUOp.mulhiu) { res := resu(63 downto 32) }
    }
}