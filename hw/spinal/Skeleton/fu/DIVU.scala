package Skeleton.fu

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class DIVU(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(FUType.divu, config))
        val output = master Stream(FUWBBundle(config))
    }
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
}