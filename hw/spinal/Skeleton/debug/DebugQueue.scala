package Skeleton.debug

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class DebugQueue(config: CPUConfig) extends Component {
    val io = new Bundle {
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
    val queue     = Mem(wordType = LSDebugBundle(config), wordCount = QueueSize) init(List.fill(QueueSize)(LSDebugBundle(config).defaultVal))
    val queueValid = Mem(wordType = Bool, wordCount = QueueSize) init(List.fill(QueueSize)(False))
    val readPtr   = Reg(UInt(log2Up(QueueSize) bits)) init(U(0))
    val writePtr  = Reg(UInt(log2Up(QueueSize) bits)) init(U(0))
    val writePtr0 = UInt(log2Up(QueueSize) bits)
    val writePtr1 = UInt(log2Up(QueueSize) bits)
    writePtr0 := writePtr
    writePtr1 := writePtr + 1

    when(io.debug0_wb_wen) {
        queue.write(writePtr0, LSDebugBundle(config).setVal(B(io.debug0_wb_pc), B(15, 4 bits), B(io.debug0_wb_wnum), B(io.debug0_wb_wdata)))
        queueValid(writePtr0) := True
        writePtr := writePtr0 + 1
    }

    when(io.debug1_wb_wen) {
        queue.write(writePtr1, LSDebugBundle(config).setVal(B(io.debug1_wb_pc), B(15, 4 bits), B(io.debug1_wb_wnum), B(io.debug1_wb_wdata)))
        queueValid(writePtr1) := True
        writePtr := writePtr1 + 1
    }
    
    val readEn = queueValid(readPtr)
    when(readEn) {
        io.debug_wb_pc    := U(queue.readSync(readPtr).wb_pc)
        io.debug_wb_wen   := U(queue.readSync(readPtr).wb_rf_wen)
        io.debug_wb_wnum  := U(queue.readSync(readPtr).wb_rf_wnum)
        io.debug_wb_wdata := U(queue.readSync(readPtr).wb_rf_wdata)
        readPtr := readPtr + 1
    }
}
