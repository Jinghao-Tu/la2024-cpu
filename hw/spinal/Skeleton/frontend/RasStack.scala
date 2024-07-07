package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._
import Skeleton.bundle.CRUOp.mask

case class RasStack(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pushen  = in(Bool())
        val wdata = in(Bits(32 bits)) // mem address width, 记得改成参数化
        val popen = in(Bool())
        val rdata = out(Bits(32 bits)) // mem address width, 记得改成参数化
    }
    
    // 改进方案: 仿照香山, 增加一个计数器, 压缩栈顶项

    val stack = Mem(Bits(config.rasStackWidth bits), config.rasStackDepth)
    val sp = Reg(UInt(log2Up(config.rasStackDepth) bits)) init(0)
    
    when(io.pushen === True) {
        stack.write(sp + 1, io.wdata)
        sp := sp + 1
    }
    
    when(io.popen === True) {
        sp := sp - 1
    }
    
    io.rdata := stack.readSync(sp)
}
