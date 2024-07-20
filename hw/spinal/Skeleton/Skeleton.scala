package Skeleton

import spinal.core._

import config._
import _root_.Skeleton.frontend.FullPredictor
import _root_.Skeleton.frontend.NextLinePredictor
import _root_.Skeleton.frontend.RasPredictor
import _root_.Skeleton.frontend.FTB
import _root_.Skeleton.frontend.BranchPredictUnit

// Hardware definition
case class Skeleton() extends Component {
  val io = new Bundle {
    val cond0 = in  Bool()
    val cond1 = in  Bool()
    val flag  = out Bool()
    val state = out UInt(8 bits)
  }

  val counter = Reg(UInt(8 bits)) init 0

  when(io.cond0) {
    counter := counter + 1
  }

  io.state := counter
  io.flag := (counter === 0) | io.cond1
}

object SkeletonVerilog extends App {
  // TODO: ROB, Decoder, ALU need to be fixed.
  // Config.spinal.generateVerilog(NextLinePredictor(CPUConfig()))
  // Config.spinal.generateVerilog(FullPredictor(CPUConfig()))
  // Config.spinal.generateVerilog(RasPredictor(CPUConfig()))
  // Config.spinal.generateVerilog(FTB(CPUConfig()))
  Config.spinal.generateVerilog(BranchPredictUnit(CPUConfig()))
}
