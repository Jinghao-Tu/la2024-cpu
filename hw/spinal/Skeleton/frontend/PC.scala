package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class PC(config: CPUConfig) extends Component {
    val io = new Bundle {
        val iCacheFeed = Vec.fill(config.fetchWidth)(master Stream(ICacheReqBundle(config))) // 0-latency!
        val pc = Vec.fill(config.fetchWidth)(master Flow(UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // 0-latency!
        val branchInfo = in(Vec.fill(config.fetchWidth)(BranchInfo(config)))
        val flush = in(Bool())
        val redirectPC = in(UInt(config.valen bits))
        
        val validFromBPU = in(Bits(config.fetchListWidth bits)) // validFromBPU(0) must be true
    }
    
    val queue1Size = config.fetchListWidth + 1
    val queue2Size = 32
    
    // to iCache
    val pcQueueToICache = Vec.fill(queue2Size)(Reg(UInt(config.valen bits)))
    val branchInfoQueueToICache = Vec.fill(queue2Size)(Reg(BranchInfo(config)))
    val validQueueToICache = Vec.fill(queue2Size)(Reg(Bool()))
    val head = Reg(UInt(log2Up(queue2Size) bits)) init(0)
    val tail = Reg(UInt(log2Up(queue2Size) bits)) init(queue2Size-1)
    val stallToQ2 = validQueueToICache(tail) && validQueueToICache(tail + U(config.fetchWidth))
    // init
    (0 until queue2Size).map(i => {
        pcQueueToICache(i).init(U(0).resized)
        branchInfoQueueToICache(i).init(BranchInfo(config).resetVal)
        validQueueToICache(i).init(False)
        when(io.flush) {
            validQueueToICache(i) := False
        }
    })
    // send to iCache
    val iCacheAcceptMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        io.iCacheFeed(i).valid := io.flush ? False | validQueueToICache(head + U(i))
        io.iCacheFeed(i).payload.address := pcQueueToICache(head + U(i))
        io.iCacheFeed(i).payload.size := LSUSizeOp.word
        io.iCacheFeed(i).payload.branchInfo := branchInfoQueueToICache(head + U(i))
        iCacheAcceptMask(i) := io.iCacheFeed(i).ready & io.iCacheFeed(i).valid
        when(iCacheAcceptMask(i)) {
            validQueueToICache(head + U(i)) := False
        }
    })
    head := Mux(io.flush, U(0), head + CountOne(iCacheAcceptMask))

    // from BPU
    val pcQueueFromBPU = Seq.fill(queue1Size)(Vec.fill(config.fetchWidth)(Reg(UInt(config.valen bits))))
    val branchInfoQueueFromBPU = Seq.fill(queue1Size)(Vec.fill(config.fetchWidth)(Reg(BranchInfo(config))))
    val validQueueFromBPU = Seq.fill(queue1Size)(Vec.fill(config.fetchWidth)(Reg(Bool())))
    val validQueue1 = Bits(config.fetchListWidth bits)
    (0 until config.fetchListWidth).map(i => {
        validQueue1(i) := validQueueFromBPU(i)(0)
    })
    // init
    (0 until queue1Size).map(i => {
        (0 until config.fetchWidth).map(j => {
            pcQueueFromBPU(i)(j).init(U(0).resized)
            branchInfoQueueFromBPU(i)(j).init(BranchInfo(config).resetVal)
            validQueueFromBPU(i)(j).init(False)
        })
    })
    // receive from BPU
    val lastValidIdx = OHToUInt(OHMasking.last(io.validFromBPU & validQueue1))
    val stallQ1 = stallToQ2 && validQueueFromBPU(queue1Size - 1)(0)
    val stallQ1Reg = RegNext(!io.flush && stallQ1)
    val stallQ1PcReg = Vec.fill(config.fetchWidth)(Reg(UInt(config.valen bits)))
    val stallQ1ValidReg = Vec.fill(config.fetchWidth)(Reg(Bool()))
    // init
    stallQ1Reg.init(True)
    (0 until config.fetchWidth).map(i => {
        stallQ1PcReg(i).init(U(config.resetVector + i*(config.instLength/8)))
        stallQ1ValidReg(i).init(True)
    })
    //
    (0 until config.fetchWidth).map(i => {
        stallQ1PcReg(i) := pcQueueFromBPU(queue1Size - 1)(i)
        stallQ1ValidReg(i) := Mux(!io.flush & stallQ1, validQueueFromBPU(queue1Size - 1)(i), False)
    })
    (0 until queue1Size).map(i => {
        (0 until config.fetchWidth).map(j => {
            if (i == 0) {
                when (io.flush) {
                    pcQueueFromBPU(i)(j) := io.redirectPC + j*(config.instLength/8)
                    validQueueFromBPU(i)(j) := True
                } .elsewhen (stallQ1Reg) {
                    pcQueueFromBPU(i)(j) := stallQ1PcReg(j)
                    validQueueFromBPU(i)(j) := stallQ1ValidReg(j)
                } .otherwise {
                    pcQueueFromBPU(i)(j) := io.npc(j).payload
                    validQueueFromBPU(i)(j) := stallQ1 ? False | io.npc(j).valid
                }
                branchInfoQueueFromBPU(i)(j) := BranchInfo(config).resetVal
            } else {
                pcQueueFromBPU(i)(j) := pcQueueFromBPU(i-1)(j)
                branchInfoQueueFromBPU(i)(j) := Mux(U(i-1) === lastValidIdx, io.branchInfo(j), branchInfoQueueFromBPU(i-1)(j))
                validQueueFromBPU(i)(j) := Mux(io.flush || (U(i) <= lastValidIdx) || stallQ1, False, validQueueFromBPU(i-1)(j))
            }
        })
    })
    // send to queue to iCache
    val queue2AcceptMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        queue2AcceptMask(i) := !stallToQ2 && validQueueFromBPU(queue1Size-1)(i) && !io.flush
        when(queue2AcceptMask(i)) {
            pcQueueToICache(tail + U(i + 1)) := pcQueueFromBPU(queue1Size-1)(i)
            branchInfoQueueToICache(tail + U(i + 1)) := branchInfoQueueFromBPU(queue1Size-1)(i)
            validQueueToICache(tail + U(i + 1)) := True
        }
    })
    tail := Mux(io.flush, U(queue2Size - 1), tail + CountOne(queue2AcceptMask))
    
    // send to BPU
    (0 until config.fetchWidth).map(i => {
        io.pc(i).valid := Mux(io.flush, True, Mux(stallQ1Reg, stallQ1ValidReg(i), io.npc(i).valid))
        io.pc(i).payload := Mux(io.flush, io.redirectPC + i*(config.instLength/8),Mux(stallQ1Reg, stallQ1PcReg(i), io.npc(i).payload))
    })
}