package Skeleton.debug

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class DebugQueue(config: CPUConfig) extends Component {
    val io = new Bundle {
        val debug0_valid = in(Bool)
        val debug1_valid = in(Bool)

        val debug0_wb_pc    = in(UInt(config.valen bits))
        val debug0_wb_wen   = in(Bool)
        val debug0_wb_wnum  = in(UInt(config.arfIdxWidth bits))
        val debug0_wb_wdata = in(UInt(config.wordLength bits))
        val debug1_wb_pc    = in(UInt(config.valen bits))
        val debug1_wb_wen   = in(Bool)
        val debug1_wb_wnum  = in(UInt(config.arfIdxWidth bits))
        val debug1_wb_wdata = in(UInt(config.wordLength bits))

        val debug_wb_pc    = out(UInt(config.valen bits))
        val debug_wb_wen   = out(UInt(config.valen / 8 bits))
        val debug_wb_wnum  = out(UInt(config.arfIdxWidth bits))
        val debug_wb_wdata = out(UInt(config.wordLength bits))
    }
    
    val QueueSize = 1024
    val queue     = Mem(wordType = LSDebugBundle(config), wordCount = QueueSize)
    val queueValid = Reg(Bits(QueueSize bits)) init(0)
    val readPtr   = Reg(UInt(log2Up(QueueSize) bits)) init(0)
    val writePtr  = Reg(UInt(log2Up(QueueSize) bits)) init(0)
    val writePtr0 = UInt(log2Up(QueueSize) bits)
    val writePtr1 = UInt(log2Up(QueueSize) bits)
    writePtr0 := writePtr
    writePtr1 := writePtr + 1

    when(io.debug0_valid) {
        queue.write(writePtr0, LSDebugBundle(config).setVal(B(io.debug0_wb_pc), B(4 bits, default -> io.debug0_wb_wen), B(io.debug0_wb_wnum), B(io.debug0_wb_wdata)))
        queueValid(writePtr0) := True
        writePtr := writePtr0 + 1
    }

    when(io.debug1_valid) {
        queue.write(writePtr1, LSDebugBundle(config).setVal(B(io.debug1_wb_pc), B(4 bits, default -> io.debug1_wb_wen), B(io.debug1_wb_wnum), B(io.debug1_wb_wdata)))
        queueValid(writePtr1) := True
        writePtr := writePtr1 + 1
    }
    
    val readEn = queueValid(readPtr)
    val readEnReg1 = RegNext(readEn)
    val readEntry = queue.readSync(readPtr) // 1-cycle delay to readEn
    val debug_wb_pc = UInt(config.valen bits)
    val debug_wb_wen = UInt(config.valen / 8 bits)
    val debug_wb_wnum = UInt(config.arfIdxWidth bits)
    val debug_wb_wdata = UInt(config.wordLength bits)
    when(readEn) {
        queueValid(readPtr) := False
        readPtr := readPtr + 1
    }
    when(readEnReg1) {
        // assign values at 1 cycle delay
        debug_wb_pc    := readEntry.wb_pc.asUInt
        debug_wb_wen   := readEntry.wb_rf_wen.asUInt
        debug_wb_wnum  := readEntry.wb_rf_wnum.asUInt
        debug_wb_wdata := readEntry.wb_rf_wdata.asUInt
    } .otherwise {
        debug_wb_pc    := U(0)
        debug_wb_wen   := U(0)
        debug_wb_wnum  := U(0)
        debug_wb_wdata := U(0)
    }

    io.debug_wb_pc    := debug_wb_pc
    io.debug_wb_wen   := debug_wb_wen
    io.debug_wb_wnum  := debug_wb_wnum
    io.debug_wb_wdata := debug_wb_wdata
}
