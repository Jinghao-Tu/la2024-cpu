package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class BranchPredictUnit(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow (UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits))) // 0-latency!
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config))) // match to pc, given with npc
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config))) 
        val flush = in(Bool())
        val fetch1_flush = out(Bool())
    }

    val GHR = Reg(UInt(config.ghrWidth bits)) // global history register

    val fetchMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    val lastPC = Flow(UInt(config.valen bits))
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    lastPC := io.pc(lastPCIdx)
    
    // next line predictor
    val nextLinePredictor = NextLinePredictor(config)
    val nlpNextBase = master Flow(UInt(config.valen bits))
    val nlpBranchInfo = BranchInfo(config)
    lastPC <> nextLinePredictor.io.lastPC
    nlpNextBase <> nextLinePredictor.io.nextBase
    nlpBranchInfo <> nextLinePredictor.io.branchInfo
    io.updateInfo <> nextLinePredictor.io.updateInfo
    GHR <> nextLinePredictor.io.GHR
    
    // full predictor
    val fullPredictor = FullPredictor(config)
    val fpNextBase = master Flow(UInt(config.valen bits))
    val fpBranchInfo = BranchInfo(config)
    lastPC <> fullPredictor.io.lastPC
    fpNextBase <> fullPredictor.io.nextBase
    fpBranchInfo <> fullPredictor.io.branchInfo
    io.updateInfo <> fullPredictor.io.updateInfo
    GHR <> fullPredictor.io.GHR
    
    // return address stack predictor
    val rasPredictor = RasPredictor(config)
    val rasNextBase = master Flow(UInt(config.valen bits))
    val rasBranchInfo = BranchInfo(config)
    lastPC <> rasPredictor.io.lastPC
    rasNextBase <> rasPredictor.io.nextBase
    rasBranchInfo <> rasPredictor.io.branchInfo
    io.updateInfo <> rasPredictor.io.updateInfo

    val rasTop = UInt(config.valen bits)
    val rasSP = UInt(log2Up(config.rasStackDepth) bits)
    rasTop <> rasPredictor.io.rasTop
    rasSP <> rasPredictor.io.rasSP
    
    // save for next cycle
    // nlp next base, io.pc, lastPCIdx, ghr ,rasSP, rasTop
    val nlpNextBaseReg = RegNext(nlpNextBase.payload)
    val pcRegList = Vec.fill(config.fetchWidth)(Reg(UInt(config.valen bits)))
    (0 until config.fetchWidth).map(i => {
        pcRegList(i) := RegNextWhen(io.pc(i).payload, io.pc(i).valid)
    })
    val lastPCIdxReg = RegNext(lastPCIdx)
    val GHRReg = RegNext(GHR)
    val rasSPReg = RegNext(rasSP)
    val rasTopReg = RegNext(rasTop)

    // choose next base
    val nextBase = master Flow(UInt(config.valen bits))
    val nextBaseSeq = scala.collection.Seq(
        rasNextBase,
        fpNextBase,
        rasNextBase
    )
    val nextBaseSel = scala.collection.Seq(
        rasNextBase.valid && rasNextBase.payload =/= nlpNextBaseReg,
        fpNextBase.valid && fpNextBase.payload =/= nlpNextBaseReg,
        nlpNextBase.valid
    )
    nextBase := PriorityMux(nextBaseSel, nextBaseSeq)
    
    // FTB
    val ftb = FTB(config)
    nextBase >> ftb.io.nextBase
    io.updateInfo <> ftb.io.updateInfo
    // npc is connected at last
    
    // generate branch info
    val branchInfo = Vec.fill(config.fetchWidth)(BranchInfo(config))
    (0 until config.fetchWidth).map(i => {
        // branch info match to pc, given with npc
        when (nextBaseSel(2)) {
            when (U(i) === lastPCIdx) {
                branchInfo(i).predictTarget := nlpBranchInfo.predictTarget
                branchInfo(i).predictTaken := nlpBranchInfo.predictTaken
                branchInfo(i).predictJumpInst := nlpBranchInfo.predictJumpInst
            } .otherwise {
                branchInfo(i).predictTarget := io.pc(i).payload + config.instLength / 8
                branchInfo(i).predictTaken := False
                branchInfo(i).predictJumpInst := False
            }
            branchInfo(i).GHR := GHR
            branchInfo(i).rasSP := rasSP
            branchInfo(i).rasTop := rasTop
        } .elsewhen(nextBaseSel(1)) {
            when (U(i) === lastPCIdx) {
                branchInfo(i).predictTarget := fpBranchInfo.predictTarget
                branchInfo(i).predictTaken := fpBranchInfo.predictTaken
                branchInfo(i).predictJumpInst := fpBranchInfo.predictJumpInst
            } .otherwise {
                branchInfo(i).predictTarget := pcRegList(i) + config.instLength / 8
                branchInfo(i).predictTaken := False
                branchInfo(i).predictJumpInst := False
            }
            branchInfo(i).GHR := GHRReg
            branchInfo(i).rasSP := rasSPReg
            branchInfo(i).rasTop := rasTopReg
        } .elsewhen(nextBaseSel(0)) {
            when (U(i) === lastPCIdxReg) {
                branchInfo(i).predictTarget := rasBranchInfo.predictTarget
                branchInfo(i).predictTaken := rasBranchInfo.predictTaken
                branchInfo(i).predictJumpInst := rasBranchInfo.predictJumpInst
            } .otherwise {
                branchInfo(i).predictTarget := pcRegList(i) + config.instLength / 8
                branchInfo(i).predictTaken := False
                branchInfo(i).predictJumpInst := False
            }
            branchInfo(i).GHR := GHRReg
            branchInfo(i).rasSP := rasSPReg
            branchInfo(i).rasTop := rasTopReg
        } .otherwise {
            branchInfo(i) := BranchInfo(config).resetVal
        }
    })
    
    // io
    io.npc <> ftb.io.npc
    io.branchInfo := branchInfo
    io.fetch1_flush := False
    
    // update GHR
    when (nextBaseSel(2)) {
        GHR := GHR |<< U(1) + branchInfo(lastPCIdx).predictTaken.asUInt
    } .otherwise {
        GHR := GHRReg |<< U(1) + branchInfo(lastPCIdxReg).predictTaken.asUInt
    }
    
    // TODO: rescue the GHR and rasStack
}
