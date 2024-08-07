package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class BranchPredictUnit(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow (UInt(config.valen bits)))
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits)))
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config))) // match to pc, given with npc
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config))) 
        val flush = in(Bool()) // for rescure
        val validFromBPU = out(Bits(config.fetchListWidth bits))
        
        val total = if (config.debug) out(UInt(64 bits)) else null
        val right = if (config.debug) out(UInt(64 bits)) else null
        val ext = if (config.debug) out(Bool()) else null
    }

    val GHR = RegInit(U(0, config.ghrWidth bits)) // global history register

    val fetchMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    val lastPC = Flow(UInt(config.valen bits))
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    lastPC := io.pc(lastPCIdx)
    
    // next line predictor - 1-latency - stage-1
    val nextLinePredictor = NextLinePredictor(config)
    val nlpNextBase = master Flow(UInt(config.valen bits))
    val nlpBranchInfo = BranchInfo(config)
    lastPC <> nextLinePredictor.io.lastPC
    nlpNextBase <> nextLinePredictor.io.nextBase
    nlpBranchInfo <> nextLinePredictor.io.branchInfo
    io.updateInfo <> nextLinePredictor.io.updateInfo
    GHR <> nextLinePredictor.io.GHR
    
    // full predictor - 1-latency - stage-1
    // val fullPredictor = FullPredictor(config)
    // val fpNextBase = master Flow(UInt(config.valen bits))
    // val fpBranchInfo = BranchInfo(config)
    // lastPC <> fullPredictor.io.lastPC
    // fpNextBase <> fullPredictor.io.nextBase
    // fpBranchInfo <> fullPredictor.io.branchInfo
    // io.updateInfo <> fullPredictor.io.updateInfo
    // GHR <> fullPredictor.io.GHR
    
    val stageNextBase = Vec.fill(config.fetchListWidth)(Flow(UInt(config.valen bits)))
    val stageNextBaseReg = Vec.fill(config.fetchListWidth)(Reg(Flow(UInt(config.valen bits))))
    val stageBranchInfo = Vec.fill(config.fetchListWidth)(BranchInfo(config))

    val stageLastPCIdxReg = Vec.fill(config.fetchListWidth)(Reg(UInt(log2Up(config.fetchWidth) bits)))
    (0 until config.fetchListWidth).map(i => {
        if (i == 0) {
            stageLastPCIdxReg(i) := lastPCIdx
        } else {
            stageLastPCIdxReg(i) := stageLastPCIdxReg(i-1)
        }
    })
    
    // stage-1
    stageNextBase(0).payload := nlpNextBase.payload
    stageNextBase(0).valid := nlpNextBase.valid
    stageBranchInfo(0) := nlpBranchInfo
    stageNextBaseReg(0).payload := stageNextBase(0).payload
    stageNextBaseReg(0).valid := stageNextBase(0).valid

    // stage-1
    // stageNextBase(0).payload := fpNextBase.payload
    // stageNextBase(0).valid := fpNextBase.valid
    // stageBranchInfo(0) := fpBranchInfo
    // stageNextBaseReg(0).payload := stageNextBase(0).payload
    // stageNextBaseReg(0).valid := stageNextBase(0).valid
    
    val validFromBPU = Bits(config.fetchListWidth bits)
    val lastNextBaseIdx = OHToUInt(OHMasking.last(validFromBPU))
    val validCounter = RegInit(B(1, config.fetchListWidth bits))
    when (io.flush) {
        validCounter := B(1, config.fetchListWidth bits)
    } .otherwise {
        val validCounterMask = Bits(config.fetchListWidth bits)
        (0 until config.fetchListWidth).map(i => {
            validCounterMask(i) := U(i) >= lastNextBaseIdx
        })
        validCounter := (validCounter & validCounterMask) |<< U(1) | B(1).resized
    }
    (0 until config.fetchListWidth).map(i => {
        if (i == 0) validFromBPU(i) := True
        else {
            validFromBPU(i) := stageNextBase(i).valid && stageNextBase(i).payload =/= stageNextBaseReg(i-1).payload && validCounter(i)
        }
    })
    io.validFromBPU := validFromBPU
    
    // FTB
    val ftb = FTB(config)
    stageNextBase(lastNextBaseIdx) <> ftb.io.nextBase
    ftb.io.npc <> io.npc
    ftb.io.updateInfo <> io.updateInfo
    ftb.io.GHR := GHR
    
    // generate branch info
    val branchInfo = Vec.fill(config.fetchWidth)(BranchInfo(config))
    (0 until config.fetchWidth).map(i => {
        when(U(i) === stageLastPCIdxReg(lastNextBaseIdx)) {
            branchInfo(i).predictTarget := stageNextBase(lastNextBaseIdx).payload
            branchInfo(i).predictTaken := stageBranchInfo(lastNextBaseIdx).predictTaken
            branchInfo(i).predictJumpInst := stageBranchInfo(lastNextBaseIdx).predictJumpInst
            if (config.debug) branchInfo(i).pc := stageBranchInfo(lastNextBaseIdx).pc
        }.otherwise {
            branchInfo(i).predictTarget := U(0).resized 
            branchInfo(i).predictTaken := False
            branchInfo(i).predictJumpInst := False
            if (config.debug) branchInfo(i).pc := U(0).resized
        }
        branchInfo(i).GHR := stageBranchInfo(lastNextBaseIdx).GHR
    })
    (0 until config.fetchWidth).map(i => {
        io.branchInfo(i) := branchInfo(i)
    })
    
    // update GHR
    val failMask = Bits(config.fetchWidth bits)
    (0 until config.retireWidth).map(i => {
        failMask(i) := io.updateInfo(i).valid && io.updateInfo(i).payload.predictFail
    })
    val firstFailIdx = OHToUInt(OHMasking.first(failMask))
    val firstFailHit = failMask.orR
    when(firstFailHit & io.flush) {
        GHR := (io.updateInfo(firstFailIdx).payload.GHR |<< U(1)) + io.updateInfo(firstFailIdx).payload.taken.asUInt
    } .elsewhen(!io.flush) {
        GHR := (branchInfo(stageLastPCIdxReg(lastNextBaseIdx)).GHR |<< U(1)) + branchInfo(stageLastPCIdxReg(lastNextBaseIdx)).predictTaken.asUInt
    }
    
    // calculate right rate
    if (config.debug) {
        val total = RegInit(U(0, 64 bits))
        val right = RegInit(U(0, 64 bits))
        val updateMask = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(i => {
            updateMask(i) := io.updateInfo(i).valid
        })
        val predRightMask = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(i => {
            predRightMask(i) := io.updateInfo(i).valid & ~io.updateInfo(i).payload.predictFail
        })
        when (updateMask.orR) {
            total := total + CountOne(updateMask)
            right := right + CountOne(predRightMask)
        }
        io.total := total
        io.right := right
        io.ext := total.andR
    }
}
