package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(master Flow(UInt(config.valen bits))) // 0-latency!
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config)))
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config))) // 0-latency!
    }
    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    nextBase := io.pc(lastPCIdx).payload + 4

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
        io.npc(i).valid := True
        io.npc(i).payload := nextBase + i|<<log2Up(config.instLength/8)
        io.branchInfo(i).predictPC := U(0).resized // Don't care on not taken
        io.branchInfo(i).predictResult := False
    })
}