package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class InstrQueue(config: CPUConfig) extends Component {
    val io = new Bundle {
        val in = master(InstrQueueInBundle(config))
        val out = slave(InstrQueueOutBundle(config))
    }
    def instrQueueIdxWidth = log2Up(config.instrQueueSize)
    def instrQueuePtrWidth = instrQueueIdxWidth + 1
    val queue = Vec.fill(config.instrQueueSize)(Reg(InstrQueueEntry(config)))
    val head = Vec.fill(config.fetchWidth)(Reg(UInt(instrQueuePtrWidth bits)))
    val headIdx = Vec.fill(config.fetchWidth)(UInt(instrQueueIdxWidth bits))
    (0 until config.fetchWidth).map(i => {
        head(i).init(U(i))
        headIdx(i) := head(i)(0, instrQueueIdxWidth bits)
    })
    val tail = Vec.fill(config.decodeWidth)(Reg(UInt(instrQueuePtrWidth bits)))
    val tailIdx = Vec.fill(config.decodeWidth)(UInt(instrQueueIdxWidth bits))
    (0 until config.decodeWidth).map(i => {
        tail(i).init(U(i))
        tailIdx(i) := head(i)(0, instrQueueIdxWidth bits)
    })

    val fetchNum = CountOne(io.in.allowMask)
    val dispatchNum = CountOne(io.out.allowMask)
    val allowBit = Bits(config.fetchWidth bits)
    val allowMask = Bits(config.fetchWidth bits)
    val availBit = Bits(config.decodeWidth bits)
    io.in.allowMask := allowMask & io.in.availMask
    (0 until config.fetchWidth).map(i => {
        allowBit(i) := headIdx(i) =/= tailIdx(0) + dispatchNum
        allowMask(i) := allowBit(i downto 0).andR
    })
    (0 until config.fetchWidth).map(i => {
        when (io.in.allowMask(i)) {
            queue(headIdx(i)) := io.in.info(i)
        }
        head(i) := head(i) + fetchNum
    })
    (0 until config.decodeWidth).map(i => {
        io.out.info(i) := queue(tailIdx(i))
        io.out.dispatchInfo(i) := preDecode(queue(tailIdx(i)).inst)
        tail(i) := tail(i) + dispatchNum
    })
    (0 until config.decodeWidth).map(i => {
        availBit(i) := tailIdx(i) =/= headIdx(0)
        io.out.availMask(i) := availBit(i downto 0).andR
    })

    def preDecode(inst: Bits): DispatchInfo = {
        val info = DispatchInfo(config)
        val rd = inst(4 downto 0)
        val rj = inst(9 downto 5)
        val r0 = B"1'b0".resized
        val r1 = B"1'b1".resized
        // Go for the fxxking decoding!
        switch(inst) {
            is(Insts.RDCNTID_W) {
                info.fuType := FUType.counter
                info.ard := rj
            }
            is(Insts.RDCNTVL_W, Insts.RDCNTVH_W) {
                info.fuType := FUType.counter
                info.ard := rd
            }
            is(Insts.ADD_W, Insts.ADDI_W, Insts.SUB_W, 
               Insts.SLT, Insts.SLTI, Insts.SLTU, Insts.SLTUI, 
               Insts.NOR, Insts.AND, Insts.ANDI, Insts.OR, Insts.ORI, Insts.XOR, Insts.XORI, 
               Insts.SLL_W, Insts.SLLI_W, Insts.SRL_W, Insts.SRLI_W, Insts.SRA_W, 
               Insts.LU12I_W, Insts.PCADDU12I, 
               Insts.ERTN, Insts.IDLE,
               Insts.JIRL
               ) {
                info.fuType := FUType.alu
                info.ard := rd
            }
            is(Insts.BREAK, Insts.SYSCALL,
               Insts.B, 
               Insts.BEQ, Insts.BNE, Insts.BLT, Insts.BGE, Insts.BLTU, Insts.BGEU) {
                info.fuType := FUType.alu
                info.ard := r0
            }
            is(Insts.BL) {
                info.fuType := FUType.alu
                info.ard := r1
            }
            is(Insts.CSRRD, Insts.CSRWR, Insts.CSRXCHG) {
                info.fuType := FUType.csr
                info.ard := rd
            }
            is(Insts.MUL_W, Insts.MULH_W, Insts.MULH_WU) {
                info.fuType := FUType.mulu
                info.ard := rd
            }
            is(Insts.DIV_W, Insts.MOD_W, Insts.DIV_WU, Insts.MOD_WU) {
                info.fuType := FUType.divu
                info.ard := rd
            }
            is(Insts.LL_W, Insts.SC_W, Insts.LD_B, Insts.LD_BU, Insts.LD_H, Insts.LD_HU, 
               Insts.PRELD) {
                info.fuType := FUType.lsu
                info.ard := rd
            }
            is(Insts.CACOP, Insts.TLBSRCH, Insts.TLBRD, Insts.TLBWR, Insts.TLBFILL, Insts.INVTLB, 
               Insts.ST_B, Insts.ST_H, Insts.ST_W, 
               Insts.DBAR, Insts.IBAR) {
                info.fuType := FUType.lsu
                info.ard := r0
            }
            default {
                info.fuType := FUType.alu
                info.ard := r0
            }
        }
        return info
    }
}