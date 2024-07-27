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
    
    // TODO: 参数化
    
    // to iCache
    val pcQueueToICache = Vec.fill(32)(Reg(UInt(config.valen bits)))
    val branchInfoQueueToICache = Vec.fill(32)(Reg(BranchInfo(config)))
    val validQueueToICache = Vec.fill(32)(Reg(Bool()))
    val head = Reg(UInt(log2Up(32) bits)) init(0)
    val tail = Reg(UInt(log2Up(32) bits)) init(32-1)
    val full = ((tail + U(1)) === head) && validQueueToICache(tail)
    val empty1 = ((tail + U(2)) === head) && validQueueToICache(tail)
    val stallToQ2 = (full || empty1)
    // val empty = (tail + U(1) === head) && !validQueueToICache(tail) // no need
    // init
    (0 until 32).map(i => {
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
        iCacheAcceptMask(i) := io.iCacheFeed(i).ready && io.iCacheFeed(i).valid
        when(iCacheAcceptMask(i)) {
            validQueueToICache(head + U(i)) := False
        }
    })
    head := Mux(io.flush, U(0), head + CountOne(iCacheAcceptMask))

    // from BPU
    val pcQueueFromBPU = Seq.fill(4)(Vec.fill(config.fetchWidth)(Reg(UInt(config.valen bits))))
    val branchInfoQueueFromBPU = Seq.fill(4)(Vec.fill(config.fetchWidth)(Reg(BranchInfo(config))))
    val validQueueFromBPU = Seq.fill(4)(Vec.fill(config.fetchWidth)(Reg(Bool())))
    val validQueue = Bits(config.fetchListWidth bits)
    (0 until config.fetchListWidth).map(i => {
        // validQueue(i) := validQueueFromBPU(i).orR
        validQueue(i) := validQueueFromBPU(i)(0)
    })
    // init
    (0 until 4).map(i => {
        (0 until config.fetchWidth).map(j => {
            if (i == 0) {
                pcQueueFromBPU(i)(j).init(U(config.resetVector + j*(config.instLength/8)))
                branchInfoQueueFromBPU(i)(j).init(BranchInfo(config).resetVal)
                validQueueFromBPU(i)(j).init(True)
                when(io.flush) {
                    validQueueFromBPU(i)(j) := False
                }
            } else {
                pcQueueFromBPU(i)(j).init(U(0).resized)
                branchInfoQueueFromBPU(i)(j).init(BranchInfo(config).resetVal)
                validQueueFromBPU(i)(j).init(False)
            }
        })
    })
    // receive from BPU
    val lastValidIdx = OHToUInt(io.validFromBPU & validQueue).resize(log2Up(4))
    val stallQ1Reg = Reg(Bool).init(False)
    val stallQ1 = stallToQ2 && validQueueFromBPU(4 - 1).orR
    when (!stallQ1) {
        (0 until 4).map(i => {
            (0 until config.fetchWidth).map(j => {
                // normal
                if (i == 0) {
                    pcQueueFromBPU(i)(j) := io.flush ? (io.redirectPC + j*(config.instLength/8)) | io.npc(j).payload
                    branchInfoQueueFromBPU(i)(j) := BranchInfo(config).resetVal
                    validQueueFromBPU(i)(j) := io.flush ? True | io.npc(j).valid
                } else {
                    when(U(i) <= lastValidIdx) {
                        // should be flushed
                        pcQueueFromBPU(i)(j) := U(0).resized
                        branchInfoQueueFromBPU(i)(j) := BranchInfo(config).resetVal
                        validQueueFromBPU(i)(j) := False
                    } .otherwise {
                        pcQueueFromBPU(i)(j) := pcQueueFromBPU(i-1)(j)
                        branchInfoQueueFromBPU(i)(j) := Mux(lastValidIdx === U(i-1), io.branchInfo(j), branchInfoQueueFromBPU(i-1)(j))
                        validQueueFromBPU(i)(j) := io.flush ? False | validQueueFromBPU(i-1)(j)
                    }
                }
            })
        })
        stallQ1Reg := False
    } .otherwise {
        // stall but last cycle is normal, and next cycle will be normal
        (0 until config.fetchWidth).map(j => {
            (1 until 4).map(i => {
                validQueueFromBPU(i)(j) := False
            })
            pcQueueFromBPU(0)(j) := io.flush ? (io.redirectPC + j*(config.instLength/8)) | pcQueueFromBPU(4 - 1)(j)
            branchInfoQueueFromBPU(0)(j) := BranchInfo(config).resetVal
            validQueueFromBPU(0)(j) := io.flush ? True | validQueueFromBPU(4 - 1)(j)
        })
    }
    // send to queue to iCache
    val queue2AcceptMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        queue2AcceptMask(i) := !stallToQ2 && validQueueFromBPU(3)(i) && !io.flush
        when(queue2AcceptMask(i)) {
            pcQueueToICache(tail + U(i + 1)) := pcQueueFromBPU(3)(i)
            branchInfoQueueToICache(tail + U(i + 1)) := branchInfoQueueFromBPU(3)(i)
            validQueueToICache(tail + U(i + 1)) := True
        }
    })
    tail := Mux(io.flush, U(32 - 1), tail + CountOne(queue2AcceptMask))
    
    // send to BPU
    (0 until config.fetchWidth).map(i => {
        io.pc(i).valid := validQueueFromBPU(0)(i)
        io.pc(i).payload := pcQueueFromBPU(0)(i)
    })
}