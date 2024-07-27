package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class CommitLogic(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Stream(FUWBBundle(config))
        val srat = master(RATIOBundle(true, true, config))
        val rob = master(ROBCommitIOBundle(config))
        val prf = master(PRFIOBundle(true, config))
        val forward = master Flow(ForwardBundle(config)) // 0-latency!
    }
    io.input.ready := True
    io.srat.prd := io.input.payload.prd
    io.srat.wen := io.input.valid
    io.rob.robIdx := io.input.payload.robIdx
    io.rob.branchInfo := io.input.payload.branchInfo
    io.rob.branchResult := io.input.payload.branchResult
    io.rob.exceptionInfo := io.input.payload.exceptionInfo
    io.rob.valid := io.input.valid
    io.prf.idx := Mux(io.input.valid, io.input.payload.prd, B(0).resized)
    io.prf.data := io.input.payload.data
    io.forward.valid := io.input.valid && (io.input.payload.prd =/= B(0).resized)
    io.forward.payload.idx := io.input.payload.prd
    io.forward.payload.payload := io.input.payload.data
}