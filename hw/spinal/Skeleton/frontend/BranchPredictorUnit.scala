package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class BPU(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow (UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits))) // 0-latency!
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config))) // match to npc
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config))) 
        val flush = in(Bool())
        val fetch1_flush = out(Bool())
    }

    val GHR = Reg(Bits(config.ghrWidth bits)) // global history register
    val rasStack = RasStack(config) // return address stack

    // // default: not jump, not taken
    // val fetchMask = Bits(config.fetchWidth bits)
    // val nextBase = UInt(config.valen bits)
    // val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    // (0 until config.fetchWidth).map(i => {
    //     fetchMask(i) := io.pc(i).valid
    // })
    // lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    // nextBase := io.pc(lastPCIdx).payload + 4

    // next line predictor
    val io_nlp = new Bundle {
    }
    val nlp = NextLinePredictor(config)
    nlp.io <> io_nlp
    
    // full predictor
    val io_fp = new Bundle {
    }
    val fp = FullPredictor(config)
    fp.io <> io_fp
    
    // ras predictor
    val io_ras = new Bundle {
    }
    val ras = RasPredictor(config)
    ras.io <> io_ras

}
