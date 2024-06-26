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
    }
    val acceptMask = Bits(config.fetchWidth bits)
    val pc = Vec.fill(config.fetchWidth)(Reg(UInt(config.valen bits)))
    val branchInfo = Vec.fill(config.fetchWidth)(Reg(BranchInfo(config)))
    val valid = Vec.fill(config.fetchWidth)(Reg(Bool())) // Make sure that AT LEAST ONE PC IS VALID at any time

    val nextPCListMid = Vec.fill(config.fetchWidth)(Vec.fill(config.fetchWidth * 2)(UInt(config.valen bits)))
    val nextBranchInfoListMid = Vec.fill(config.fetchWidth)(Vec.fill(config.fetchWidth * 2)(BranchInfo(config)))
    val nextValidListMid = Vec.fill(config.fetchWidth)(Vec.fill(config.fetchWidth * 2)(Bool()))

    val nextPCList = Vec.fill(config.fetchWidth * 2)(UInt(config.valen bits))
    val nextBranchInfoList = Vec.fill(config.fetchWidth * 2)(BranchInfo(config))
    val nextValidList = Vec.fill(config.fetchWidth * 2)(Bool())

    (0 until config.fetchWidth).map(i => {
        pc(i).init(U(config.resetVector + i*(config.instLength/8)))
        branchInfo(i).init(BranchInfo(config).resetVal)
        valid(i).init(True)
        acceptMask(i) := io.iCacheFeed(i).ready && valid(i)
        
        (0 until config.fetchWidth).map(j => { // Total diaster. Can imagine timing violation.
            if (i == 0) {
                nextPCListMid(0)(j) := pc(j)
                nextBranchInfoListMid(0)(j) := branchInfo(j)
                nextValidListMid(0)(j) := valid(j)
                nextPCListMid(0)(j+config.fetchWidth) := io.npc(j).payload
                nextValidListMid(0)(j+config.fetchWidth) := io.npc(j).valid
                nextBranchInfoListMid(0)(j+config.fetchWidth) := io.branchInfo(j)
            } else {
                if (j + 1 < config.fetchWidth) {
                    nextPCListMid(i)(j) := nextValidListMid(i-1).asBits(j downto 0).andR ? nextPCListMid(i-1)(j) | nextPCListMid(i-1)(j+1)
                    nextBranchInfoListMid(i)(j) := nextValidListMid(i-1).asBits(j downto 0).andR ? nextBranchInfoListMid(i-1)(j) | nextBranchInfoListMid(i-1)(j+1)
                    nextValidListMid(i)(j) := nextValidListMid(i-1).asBits(j downto 0).andR ? nextValidListMid(i-1)(j) | nextValidListMid(i-1)(j+1)
                    nextPCListMid(i)(j+config.fetchWidth) := nextValidListMid(i-1).asBits(j+config.fetchWidth downto 0).andR ? nextPCListMid(i-1)(j+config.fetchWidth) | nextPCListMid(i-1)(j+config.fetchWidth+1)
                    nextBranchInfoListMid(i)(j+config.fetchWidth) := nextValidListMid(i-1).asBits(j+config.fetchWidth downto 0).andR ? nextBranchInfoListMid(i-1)(j+config.fetchWidth) | nextBranchInfoListMid(i-1)(j+config.fetchWidth+1)
                    nextValidListMid(i)(j+config.fetchWidth) := nextValidListMid(i-1).asBits(j+config.fetchWidth downto 0).andR ? nextValidListMid(i-1)(j+config.fetchWidth) | nextValidListMid(i-1)(j+config.fetchWidth+1)
                } else {
                    nextPCListMid(i)(j) := nextValidListMid(i-1).asBits(j downto 0).andR ? nextPCListMid(i-1)(j) | nextPCListMid(i-1)(j+1)
                    nextBranchInfoListMid(i)(j) := nextValidListMid(i-1).asBits(j downto 0).andR ? nextBranchInfoListMid(i-1)(j) | nextBranchInfoListMid(i-1)(j+1)
                    nextValidListMid(i)(j) := nextValidListMid(i-1).asBits(j downto 0).andR ? nextValidListMid(i-1)(j) | nextValidListMid(i-1)(j+1)
                    nextPCListMid(i)(j+config.fetchWidth) := nextValidListMid(i-1).asBits(j+config.fetchWidth downto 0).andR ? nextPCListMid(i-1)(j+config.fetchWidth) | U(0).resized
                    nextBranchInfoListMid(i)(j+config.fetchWidth) := nextValidListMid(i-1).asBits(j+config.fetchWidth downto 0).andR ? nextBranchInfoListMid(i-1)(j+config.fetchWidth) | BranchInfo(config).resetVal
                    nextValidListMid(i)(j+config.fetchWidth) := nextValidListMid(i-1).asBits(j+config.fetchWidth downto 0).andR ? nextValidListMid(i-1)(j+config.fetchWidth) | False
                }
            }
        })

        pc(i) := io.flush ? (io.redirectPC + i*(config.instLength/8)) | nextPCListMid(config.fetchWidth-1)(i+CountOne(acceptMask))
        branchInfo(i) := io.flush ? BranchInfo(config).resetVal | nextBranchInfoListMid(config.fetchWidth-1)(i+CountOne(acceptMask))
        valid(i) := io.flush ? True | nextValidListMid(config.fetchWidth-1)(i+CountOne(acceptMask))
    })

    (0 until config.fetchWidth).map(i => {
        io.iCacheFeed(i).valid := valid(i)
        io.iCacheFeed(i).payload.address := pc(i)
        io.iCacheFeed(i).payload.size := LSUSizeOp.word
        io.iCacheFeed(i).payload.branchInfo := branchInfo(i)
        io.pc(i).valid := valid(i)
        io.pc(i).payload := pc(i)
    })
}