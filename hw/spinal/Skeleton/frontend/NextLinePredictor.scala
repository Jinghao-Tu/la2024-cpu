package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // time base
        val nextBase = out(UInt(config.valen bits)) // 0-latency
        val branchInfo = master Flow(BranchInfo(config)) // 0-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config)))
        
        val GHR = in(UInt(config.ghrWidth bits))
    }
    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    val lastPC = UInt(config.valen bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    lastPC := io.pc(lastPCIdx).payload

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR

    val BTB = Mem(BTBBundle(config), wordCount = config.btbSize) init(Seq.fill(config.btbSize)(BTBBundle(config).resetVal)) // branch target buffer
    val BHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) init(Seq.fill(config.bhtSize)(U(0, config.bhtWidth bits))) // branch history table
    
    // TODO: 更改 hash 算法
    def hash_index(pc: UInt, GHR: UInt, level: Int): UInt = {
        var hash = U(0, 10 bits)
        for (i <- 0 until 1 << (level - 1)) {
            hash = hash ^ GHR(i * 10 + 9 downto i * 10)
        }
        for (i <- 0 until 3) {
            hash = hash ^ pc(i * 10 + 11 downto i * 10 + 2)
        }
        hash
    }
    
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

    val index = hash_index(lastPC, GHR, 4)
    val tag = hash_tag(lastPC)
    predictTaken := BHT.readAsync(index) === 3 || BHT.readAsync(index) === 2
    predictJumpInst := BTB.readAsync(lastPC(7 downto 2)).valid && BTB.readAsync(lastPC(7 downto 2)).tag === tag
    switch(predictJumpInst && predictTaken) {
        is(True) {
            predictTarget := BTB.readAsync(lastPC(7 downto 2)).target
        }
        is(False) {
            predictTarget := lastPC + 4
        }
    }
    
    nextBase := predictTarget
    
    io.nextBase := nextBase
    io.branchInfo.valid := fetchMask(lastPCIdx)
    io.branchInfo.payload.predictTarget := predictTarget
    io.branchInfo.payload.predictTaken := predictTaken
    io.branchInfo.payload.predictJumpInst := predictJumpInst
// --------------------------------------------------------------------------------------------------------------------------------

    val updateFetchMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        updateFetchMask(i) := io.updateInfo(i).valid
    })

    (0 until config.retireWidth).map(i => {
        val updatePC = io.updateInfo(i).payload.pc
        val updateIsJumpInst = io.updateInfo(i).payload.isJumpInst || io.updateInfo(i).payload.isCallInst
        val updateTaken = io.updateInfo(i).payload.taken
        val updateTarget = io.updateInfo(i).payload.target

        val updatePredictTarget = io.updateInfo(i).payload.branchInfo.predictTarget
        val updatePredictTaken = io.updateInfo(i).payload.branchInfo.predictTaken
        val updatePredictIsJumpInst = io.updateInfo(i).payload.branchInfo.predictJumpInst || io.updateInfo(i).payload.isCallInst
        val updateGHR = io.updateInfo(i).payload.branchInfo.GHR
        
        val updatePredictFail = updatePredictTarget =/= updateTarget || updatePredictTaken =/= updateTaken || updatePredictIsJumpInst =/= updateIsJumpInst

        val index = hash_index(io.pc(i).payload, updateGHR, 4)
        val tag = hash_tag(io.pc(i).payload)

        // TODO: 更改更新策略
        when (updateFetchMask(i)) {
            when (updateIsJumpInst) {
                // update bht
                switch (updateTaken) {
                    is(True) {
                        BHT(index) := BHT(index) +| 1
                    }
                    is(False) {
                        BHT(index) := BHT(index) -| 1
                    }
                }
                
                // update btb
                BTB.write(updatePC(7 downto 2), BTBBundle(config).setVal(True, tag, updateTarget))
            } otherwise {
                // update btb
                BTB.write(updatePC(7 downto 2), BTBBundle(config).resetVal)
            }
        }
    })

}