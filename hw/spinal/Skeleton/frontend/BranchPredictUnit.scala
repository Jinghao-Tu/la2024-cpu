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
    val stageGHR = Vec.fill(config.fetchListWidth)(Reg(UInt(config.ghrWidth bits)))
    (0 until config.fetchListWidth).map(i => {
        if (i == 0 || i == 1) stageGHR(i) := GHR
        else stageGHR(i) := stageGHR(i-1)
    })
    

    val fetchMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    val lastPC = Flow(UInt(config.valen bits))
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    lastPC := io.pc(lastPCIdx)
    
    // next line predictor - 0-latency - stage-0
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
    val stageBranchInfo = Vec.fill(config.fetchListWidth)(BranchInfo(config))
    val stageNextBaseReg = Vec.fill(config.fetchListWidth)(Reg(Flow(UInt(config.valen bits))))
    
    // stage-0
    stageNextBase(0).payload := nlpNextBase.payload
    stageNextBase(0).valid := nlpNextBase.valid
    stageBranchInfo(0) := nlpBranchInfo
    stageNextBaseReg(0).payload := stageNextBase(0).payload
    stageNextBaseReg(0).valid := stageNextBase(0).valid

    // stage-1
    // stageNextBase(1).payload := fpNextBase.payload
    // stageNextBase(1).valid := fpNextBase.valid
    // stageBranchInfo(1) := fpBranchInfo
    stageNextBase(1).payload := U(0).resized
    stageNextBase(1).valid := False
    stageBranchInfo(1) := BranchInfo(config).resetVal
    stageNextBaseReg(1).payload := stageNextBase(1).payload
    stageNextBaseReg(1).valid := stageNextBase(1).valid
    
    val validFromBPU = Bits(config.fetchListWidth bits)
    (0 until config.fetchListWidth).map(i => {
        if (i == 0) validFromBPU(i) := True
        else {
            validFromBPU(i) := stageNextBase(i).valid && stageNextBase(i).payload =/= stageNextBaseReg(i-1).payload
        }
    })
    // val lastNextBaseIdx = OHToUInt(OHMasking.last(validFromBPU))
    val lastNextBaseIdx = U(0, log2Up(config.fetchListWidth) bits)
    // io.validFromBPU := validFromBPU
    io.validFromBPU := B(1, config.fetchListWidth bits)
    
    // FTB
    val ftb = FTB(config)
    val npc = Vec.fill(config.fetchWidth)(Flow(UInt(config.valen bits)))
    stageNextBase(lastNextBaseIdx) <> ftb.io.nextBase
    ftb.io.npc <> io.npc
    ftb.io.updateInfo <> io.updateInfo
    ftb.io.GHR := GHR
    
    // generate branch info
    val branchInfo = Vec.fill(config.fetchWidth)(BranchInfo(config))
    (0 until config.fetchWidth).map(i => {
        when(U(i) === lastPCIdx) {
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
        branchInfo(i).GHR := Mux(lastNextBaseIdx === U(0), GHR, stageGHR(lastNextBaseIdx))
    })
    (0 until config.fetchWidth).map(i => {
        io.branchInfo(i) := branchInfo(i)
    })
    
    // update GHR
    when(io.flush) {
        val failMask = Bits(config.fetchWidth bits)
        (0 until config.retireWidth).map(i => {
            failMask(i) := io.updateInfo(i).valid && io.updateInfo(i).payload.predictFail
        })
        val lastFailIdx = OHToUInt(OHMasking.last(failMask))
        GHR := io.updateInfo(lastFailIdx).payload.GHR
    } .otherwise {
        GHR := (branchInfo(lastPCIdx).GHR |<< U(1)) | branchInfo(lastPCIdx).predictTaken.asUInt.resized
    }
    
    // calculate right rate
    if (config.debug) {
        val total = Reg(UInt(64 bits)) init(0)
        val right = Reg(UInt(64 bits)) init(0)
        val updateMask = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(i => {
            updateMask(i) := io.updateInfo(i).valid && io.updateInfo(i).payload.isJumpInst
        })
        val predRightMask = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(i => {
            predRightMask(i) := io.updateInfo(i).valid && io.updateInfo(i).payload.isJumpInst && ~io.updateInfo(i).payload.predictFail
        })
        when (updateMask.orR) {
            total := total + CountOne(updateMask)
            right := right + CountOne(predRightMask)
        }
        val ext = RegInit(False)
        when (total.andR) {
            ext := True
        }
        io.total := total
        io.right := right
        io.ext := ext
    }
}
