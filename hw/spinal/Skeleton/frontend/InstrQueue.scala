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
    val queue = Vec.fill(config.instrQueueSize)(Reg(InstrQueueEntry(config)))
    val valid = Vec.fill(config.instrQueueSize)(Reg(Bool()))
    valid.foreach(_ init(False))
    val head = Vec.fill(config.fetchWidth)(Reg(UInt(instrQueueIdxWidth bits)))
    (0 until config.fetchWidth).map(i => {
        head(i).init(U(i))
    })
    val tail = Vec.fill(config.decodeWidth)(Reg(UInt(instrQueueIdxWidth bits)))
    (0 until config.decodeWidth).map(i => {
        tail(i).init(U(i))
    })

    val infoOut = Vec.fill(config.decodeWidth)(Reg(InstrQueueEntry(config)))
    val dispatchInfoOut = Vec.fill(config.decodeWidth)(Reg(DispatchInfo(config)))
    val availMaskOut = Reg(Bits(config.decodeWidth bits))
    availMaskOut.init(B(0).resized)

    val fetchNum = CountOne(io.in.allowMask)
    val dispatchNum = CountOne(io.out.allowMask)
    val allowMask = Bits(config.fetchWidth bits)
    io.in.allowMask := allowMask & io.in.availMask
    (0 until config.fetchWidth).map(i => {
        allowMask(i) := ~valid(tail(i))
        when (io.in.allowMask(i)) {
            queue(tail(i)) := io.in.info(i)
        }
        tail(i) := tail(i) + fetchNum
    })
    (0 until config.decodeWidth).map(i => {
        infoOut(i) := queue(head(i)+dispatchNum)
        dispatchInfoOut(i) := preDecode(queue(head(i)+dispatchNum).inst)
        availMaskOut(i) := valid(head(i)+dispatchNum)
        head(i) := head(i) + dispatchNum
    })

    (0 until config.instrQueueSize).map(i => {
        val idxMatchMaskFetch = Bits(config.fetchWidth bits)
        (0 until config.fetchWidth).map(j => {
            idxMatchMaskFetch(j) := tail(j) === i && io.in.allowMask(j)
        })
        val idxMatchMaskDispatch = Bits(config.decodeWidth bits)
        (0 until config.decodeWidth).map(j => {
            idxMatchMaskDispatch(j) := head(j) === i && io.out.allowMask(j)
        })
        valid(i) := idxMatchMaskDispatch.orR ? False | (valid(i) || idxMatchMaskFetch.orR)
    })

    io.out.info := infoOut
    io.out.dispatchInfo := dispatchInfoOut
    io.out.availMask := availMaskOut

    def preDecode(inst: Bits): DispatchInfo = {
        val info = DispatchInfo(config)
        val rd = inst(4 downto 0)
        val rj = inst(9 downto 5)
        val rk = inst(14 downto 10)
        val r0 = B"1'b0".resized
        val r1 = B"1'b1".resized
        // Default value
        info.ard := r0
        info.asrc(0) := r0
        info.asrc(1) := r0
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
            is(Insts.ADD_W, Insts.SUB_W, 
               Insts.SLT, Insts.SLTU, 
               Insts.NOR, Insts.AND, Insts.OR, Insts.XOR, 
               Insts.SLL_W, Insts.SRL_W, Insts.SRA_W
               ) {
                info.fuType := FUType.alu
                info.ard := rd
                info.asrc(0) := rj
                info.asrc(1) := rk
            }
            is(Insts.ADDI_W, 
               Insts.SLTI, Insts.SLTUI, 
               Insts.ANDI, Insts.ORI, Insts.XORI, 
               Insts.SLLI_W, Insts.SRLI_W, Insts.SRAI_W, 
               Insts.JIRL) {
                info.fuType := FUType.alu
                info.ard := rd
                info.asrc(0) := rj
            }
            is(Insts.BREAK, Insts.SYSCALL, 
               Insts.ERTN, Insts.IDLE,
               Insts.B) {
                info.fuType := FUType.alu
            }
            is (Insts.LU12I_W, Insts.PCADDU12I) {
                info.fuType := FUType.alu
                info.ard := rd
            }
            is(Insts.BL) {
                info.fuType := FUType.alu
                info.ard := r1
            }
            is (Insts.BEQ, Insts.BNE, Insts.BLT, Insts.BGE, Insts.BLTU, Insts.BGEU) {
                info.fuType := FUType.alu
                info.asrc(0) := rj
                info.asrc(1) := rd
            }
            is(Insts.CSR) {
                info.fuType := FUType.csr
                info.ard := rd
                info.asrc(0) := rd
                when (rj =/= B(0).resized && rj =/= B(1).resized) { // CSRXCHG
                    info.asrc(1) := rj
                }
            }
            is(Insts.MUL_W, Insts.MULH_W, Insts.MULH_WU) {
                info.fuType := FUType.mulu
                info.ard := rd
                info.asrc(0) := rj
                info.asrc(1) := rk
            }
            is(Insts.DIV_W, Insts.MOD_W, Insts.DIV_WU, Insts.MOD_WU) {
                info.fuType := FUType.divu
                info.ard := rd
                info.asrc(0) := rj
                info.asrc(1) := rk
            }
            is(Insts.LL_W, Insts.LD_B, Insts.LD_BU, Insts.LD_H, Insts.LD_HU, Insts.LD_W) {
                info.fuType := FUType.lsu
                info.ard := rd
                info.asrc(0) := rj
            }
            is(Insts.CACOP, Insts.PRELD) {
                info.fuType := FUType.lsu
                info.asrc(0) := rj
            }
            is(Insts.TLBSRCH, Insts.TLBRD, Insts.TLBWR, Insts.TLBFILL, 
               Insts.DBAR, Insts.IBAR) {
                info.fuType := FUType.lsu
            }
            is(Insts.INVTLB) {
                info.fuType := FUType.lsu
                info.asrc(0) := rj
                info.asrc(1) := rk
            }
            is(Insts.SC_W) {
                info.fuType := FUType.lsu
                info.ard := rd
                info.asrc(0) := rj
                info.asrc(1) := rd
            }
            is(Insts.ST_B, Insts.ST_H, Insts.ST_W) {
                info.fuType := FUType.lsu
                info.asrc(0) := rj
                info.asrc(1) := rd
            }
            default {
                info.fuType := FUType.alu
            }
        }
        return info
    }
}