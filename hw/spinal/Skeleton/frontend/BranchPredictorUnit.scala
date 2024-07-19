package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class BPU(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow (UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits))) // 0-latency!
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config))) // match to pc
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config))) 
        val flush = in(Bool())
        val fetch1_flush = out(Bool())
    }

    val GHR = Reg(Bits(config.ghrWidth bits)) // global history register

    val fetchMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    val lastPC = UInt(config.valen bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    lastPC := io.pc(lastPCIdx).payload


}
