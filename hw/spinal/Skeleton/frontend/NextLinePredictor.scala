package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // 0-latency!
        val branchInfo = Vec.fill(config.fetchWidth)(master Flow(BranchInfo(config)))
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config)))
    }

    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    nextBase := io.pc(fetchMask.orR ? (CountOne(fetchMask)-1) | 0).payload + 4

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
        io.branchInfo(i).valid := True
        io.branchInfo(i).payload.predictPC := nextBase + i<<log2Up(config.instLength/8)
        io.branchInfo(i).payload.predictResult := False
    })
}