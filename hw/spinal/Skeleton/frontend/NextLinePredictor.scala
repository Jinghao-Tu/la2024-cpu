package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._
import oshi.jna.platform.windows.NtDll.UNICODE_STRING

// TODO: 0 延迟的下一行预测器, 采用 BTB 实现
case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(master Flow(UInt(config.valen bits))) // 0-latency!
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config)))
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config))) // 0-latency!
    }
    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    nextBase := io.pc(lastPCIdx).payload + 4

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val GHR = Reg(UInt(config.ghrWidth bits)) init(0)

    val BTB = Mem(UInt(config.btbWidth bits), wordCount = config.btbSize) init(Seq.fill(config.btbSize)(U(0, config.btbWidth bits)))
    val BHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) init(Seq.fill(config.bhtSize)(U(0, config.bhtWidth bits)))
    
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
    val predictPC = Vec.fill(config.fetchWidth)(UInt(config.valen bits))
    val predictResult = Vec.fill(config.fetchWidth)(Bool)
    val predictHit = Vec.fill(config.fetchWidth)(Bool)
    val nextGHR = UInt(config.ghrWidth bits)
    nextGHR := (GHR @@ False).resized

    (0 until config.fetchWidth).map(i => {
        val index = hash_index(io.pc(i).payload, GHR, 4)
        val tag = hash_tag(io.pc(i).payload)
        predictResult(i) := BHT.readAsync(index) === 3 || BHT.readAsync(index) === 2
        predictHit(i) := (BTB.readAsync(io.pc(i).payload(7 downto 2))(39 downto 32) === tag) && BTB.readAsync(io.pc(i).payload(7 downto 2))(40)
        switch(predictHit(i) && predictResult(i)) {
            is(True) {
                predictPC(i) := BTB.readAsync(io.pc(i).payload(7 downto 2))(31 downto 0)
            }
            is(False) {
                predictPC(i) := io.pc(i).payload + 4
            }
        }
    })
    
    (config.fetchWidth-1 until -1 by -1).map(i => {
        when (predictHit(i)) {
            when (predictResult(i)) {
                nextBase := predictPC(i)
            }
            nextGHR := (GHR @@ predictResult(i)).resized
        }
    })
    
    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := True
        io.npc(i).payload := nextBase + i |<< log2Up(config.instLength / 8)
        io.branchInfo(i).predictPC := predictPC(i)
        io.branchInfo(i).predictResult := predictResult(i)
        io.branchInfo(i).GHR := GHR
    })

    GHR := nextGHR

// --------------------------------------------------------------------------------------------------------------------------------

    val updateFetchMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        updateFetchMask(i) := io.updateInfo(i).valid
    })

    (0 until config.fetchWidth).map(i => {
        val index = hash_index(io.pc(i).payload, GHR, 4)
        val tag = hash_tag(io.pc(i).payload)
        
        val updatePC = io.updateInfo(i).payload.pc
        val updateIsJumpInst = io.updateInfo(i).payload.isJumpInst
        val updateTaken = io.updateInfo(i).payload.taken
        val updatePredictFail = io.updateInfo(i).payload.predictFail
        val updateTarget = io.updateInfo(i).payload.target
        val updateGHR = io.updateInfo(i).payload.GHR

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
                
                // update ghr
                when (updatePredictFail) {
                    GHR := (updateGHR @@ updateTaken).resized
                }
            }
        }
    })

}