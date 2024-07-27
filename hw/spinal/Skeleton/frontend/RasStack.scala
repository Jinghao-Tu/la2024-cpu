package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._
import Skeleton.bundle.CRUOp.mask

case class RasStack(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pushen  = in(Bool())
        val wdata = in(UInt(32 bits)) // mem address width, 记得改成参数化
        val popen = in(Bool())
        val rtop = out(UInt(32 bits)) // mem address width, 记得改成参数化
        val rcount = out(UInt(config.rasStackCounterWidth bits))
        val rsp = out(UInt(log2Up(config.rasStackDepth) bits))
        
        val flush = in(Bool())
        val reTop = in(UInt(32 bits))
        val reCount = in(UInt(config.rasStackCounterWidth bits))
        val reSp = in(UInt(log2Up(config.rasStackDepth) bits))
    }

    val stack = Mem(RasStackBundle(config), config.rasStackDepth)
    val sp = Reg(UInt(log2Up(config.rasStackDepth) bits)) init(0)
    val rtop = stack.readSync(sp) // target and counter

    when (!io.flush) {
        when(io.pushen === True) {
            when (rtop.target === io.wdata) {
                stack.write(sp, RasStackBundle(config).setVal(rtop.target, rtop.counter + 1))
            } otherwise {
                stack.write(sp + 1, RasStackBundle(config).setVal(io.wdata, 1))
                sp := sp + 1
            }
        }
        
        when(io.popen === True) {
            when (rtop.counter === 1) {
                sp := sp - 1
            } otherwise {
                stack.write(sp, RasStackBundle(config).setVal(rtop.target, rtop.counter - 1))
            }
        }
    }.otherwise {
        stack.write(io.reSp, RasStackBundle(config).setVal(io.reTop, io.reCount))
        sp := io.reSp
    }
    
    io.rtop := rtop.target
    io.rcount := rtop.counter
    io.rsp := sp
}
