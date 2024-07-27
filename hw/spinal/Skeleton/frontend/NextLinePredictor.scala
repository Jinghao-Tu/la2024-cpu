package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val lastPC = slave Flow(UInt(config.valen bits)) // time base
        val nextBase = master Flow(UInt(config.valen bits)) // 0-latency
        val branchInfo = out(BranchInfo(config)) // 0-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config)))
        
        val GHR = in(UInt(config.ghrWidth bits))
    }

    val nextBase = UInt(config.valen bits)
    val lastPC = UInt(config.valen bits)
    lastPC := io.lastPC.payload

    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR

    // val BTB = Mem(BTBBundle(config), wordCount = config.btbSize) // branch target buffer
    // val BHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) // branch history table
    val BTB = Vec.fill(config.btbSize)(RegInit(BTBBundle(config).resetVal))
    val BHT = Vec.fill(config.bhtSize)(RegInit(U(1, config.bhtWidth bits)))
    
    // TODO: 更改 hash 算法
    // def hash_index(pc: UInt, GHR: UInt, level: Int): UInt = {
    //     var hash = U(0, 10 bits)
    //     for (i <- 0 until 1 << (level - 1)) {
    //         hash = hash ^ GHR(i * 10 + 9 downto i * 10)
    //     }
    //     for (i <- 0 until 3) {
    //         hash = hash ^ pc(i * 10 + 11 downto i * 10 + 2)
    //     }
    //     hash
    // }
    
    def hash_tag(pc: UInt): UInt = {
        var hash = U(0, 8 bits)
        for (i <- 0 until 4) {
            hash = hash ^ pc(i * 8 + 7 downto i * 8)
        }
        hash
    }

// --------------------------------------------------------------------------------------------------------------------------------
    val predictTarget = UInt(config.valen bits)
    val predictTaken = Bool()
    val predictJumpInst = Bool()

    // val index = hash_index(lastPC, GHR, 4)
    val index = lastPC(log2Up(config.bhtSize)+1 downto 2)
    val tag = hash_tag(lastPC)
    val bht_item = BHT(index) // 异步读, 延迟很大
    val btb_item = BTB(lastPC(log2Up(config.btbSize)+1 downto 2)) // 异步读, 延迟很大
    predictTaken := bht_item.orR
    predictJumpInst := btb_item.valid && btb_item.tag === tag
    switch(predictJumpInst && predictTaken) {
        is(True) {
            predictTarget := btb_item.target
        }
        default {
            predictTarget := lastPC + 4
        }
    }
    
    nextBase := predictTarget
    
    io.nextBase.valid := io.lastPC.valid // 0-latency
    io.nextBase.payload := nextBase
    io.branchInfo.predictTarget := predictTarget
    io.branchInfo.predictTaken := predictTaken & predictJumpInst
    io.branchInfo.predictJumpInst := predictJumpInst
    io.branchInfo.GHR := U(0).resized
    io.branchInfo.pc := lastPC
// --------------------------------------------------------------------------------------------------------------------------------

    val updateMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        updateMask(i) := io.updateInfo(i).valid
    })

    (0 until config.retireWidth).map(i => {
        val updatePC = io.updateInfo(i).payload.pc

        val updateIsJumpInst = io.updateInfo(i).payload.branchResult.isJumpInst
        val updateTaken = io.updateInfo(i).payload.branchResult.taken
        val updateTarget = io.updateInfo(i).payload.branchResult.targetPC

        val updatePredictTarget = io.updateInfo(i).payload.branchInfo.predictTarget
        val updatePredictTaken = io.updateInfo(i).payload.branchInfo.predictTaken
        val updatePredictIsJumpInst = io.updateInfo(i).payload.branchInfo.predictJumpInst
        val updateGHR = io.updateInfo(i).payload.branchInfo.GHR
        
        // val updatePredictFail = updatePredictTarget =/= updateTarget || updatePredictTaken =/= updateTaken || updatePredictIsJumpInst =/= updateIsJumpInst
        val updatePredictFail = io.updateInfo(i).payload.branchResult.predictFail

        // val updIdx = hash_index(updatePC, updateGHR, 4)
        val updIdx = updatePC(log2Up(config.bhtSize)+1 downto 2)
        val updTag = hash_tag(updatePC)

        // TODO: 没有记录也不跳转的就不增加记录.
        when (updateMask(i)) {
            when (updateIsJumpInst) {
                // update bht
                BHT(updIdx) := BHT(updIdx) |<< U(1) + updateTaken.asUInt
                
                // update btb
                BTB(updatePC(log2Up(config.btbSize)+1 downto 2)) := BTBBundle(config).setVal(True, updTag, updateTarget)
            } .elsewhen (updatePredictFail) {
                // update btb
                BTB(updatePC(log2Up(config.btbSize)+1 downto 2)) := BTBBundle(config).resetVal
            }
        }
    })

}