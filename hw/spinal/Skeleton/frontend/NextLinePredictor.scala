package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._
import oshi.jna.platform.windows.NtDll.UNICODE_STRING

case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // time base
        val npc = Vec.fill(config.fetchWidth)(master Flow(UInt(config.valen bits))) // 0-latency
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config))) // 0-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config)))
        
        val GHR = in(UInt(config.ghrWidth bits))
    }
    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    nextBase := io.pc(lastPCIdx).payload + 4

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR

    val BTB = Mem(UInt(config.btbWidth bits), wordCount = config.btbSize) init(Seq.fill(config.btbSize)(U(0, config.btbWidth bits))) // branch target buffer
    val BHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) init(Seq.fill(config.bhtSize)(U(0, config.bhtWidth bits))) // branch history table
    
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
    val predictTarget = Vec.fill(config.fetchWidth)(UInt(config.valen bits))
    val predictTaken = Vec.fill(config.fetchWidth)(Bool)
    val predictJumpInst = Vec.fill(config.fetchWidth)(Bool)

    (0 until config.fetchWidth).map(i => {
        val index = hash_index(io.pc(i).payload, GHR, 4)
        val tag = hash_tag(io.pc(i).payload)
        predictTaken(i) := BHT.readAsync(index) === 3 || BHT.readAsync(index) === 2
        predictJumpInst(i) := (BTB.readAsync(io.pc(i).payload(7 downto 2))(39 downto 32) === tag) && BTB.readAsync(io.pc(i).payload(7 downto 2))(40)
        switch(predictJumpInst(i) && predictTaken(i)) {
            is(True) {
                predictTarget(i) := BTB.readAsync(io.pc(i).payload(7 downto 2))(31 downto 0)
            }
            is(False) {
                predictTarget(i) := io.pc(i).payload + 4
            }
        }
    })
    
    (config.fetchWidth-1 until -1 by -1).map(i => {
        when (predictJumpInst(i)) {
            when (predictTaken(i)) {
                nextBase := predictTarget(i)
            }
        }
    })
    
    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := True
        io.npc(i).payload := nextBase + i |<< log2Up(config.instLength / 8)
        io.branchInfo(i).predictTarget := predictTarget(i)
        io.branchInfo(i).predictTaken := predictTaken(i)
        io.branchInfo(i).predictJumpInst := predictJumpInst(i)
        io.branchInfo(i).GHR := U(0).resized
        io.branchInfo(i).sp := U(0).resized
        io.branchInfo(i).rasTop := U(0).resized
    })

// --------------------------------------------------------------------------------------------------------------------------------

    val updateFetchMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        updateFetchMask(i) := io.updateInfo(i).valid
    })

    (0 until config.retireWidth).map(i => {
        val updatePC = io.updateInfo(i).pc
        val updateIsJumpInst = io.updateInfo(i).isJumpInst
        val updateTaken = io.updateInfo(i).taken
        val updateTarget = io.updateInfo(i).target

        val updatePredictTarget = io.updateInfo(i).predictTarget
        val updatePredictTaken = io.updateInfo(i).predictTaken
        val updatePredictJumpInst = io.updateInfo(i).predictJumpInst
        val updateGHR = io.updateInfo(i).GHR
        
        val updatePredictFail = updatePredictTarget =/= updateTarget || updatePredictTaken =/= updateTaken || updatePredictJumpInst =/= updateIsJumpInst

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
                BTB.write(updatePC(7 downto 2), (True.asUInt @@ tag @@ updateTarget))
            } otherwise {
                // update btb
                BTB.write(updatePC(7 downto 2), (False.asUInt @@ tag @@ updateTarget))
            }
        }
    })

}