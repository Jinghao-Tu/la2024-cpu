package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class ROB(config: CPUConfig) extends Component { // Also retire logic
    val io = new Bundle {
        val dispatch = slave(ROBDispatchIOBundle(config))
        val commit = Vec.fill(config.issueWidth)(slave(ROBCommitIOBundle(config)))
        val retireARAT = Vec.fill(config.retireWidth)(master(RATIOBundle(true, false, config)))
        val retireFreeList = master(FreeListRetireIOBundle(config))
        val retireLSU = slave(LSUROBBundle(config))
        val wakeupMem = out(Bool())
        val updateBPU = Vec.fill(config.retireWidth)(master Flow(BPUUpdateBundle(config)))
        val csrCtrl = master(ROBCSRBundle(config))
        val flush = out(Bool())
        val interrupt = in(Bool()) // For IDLE
        val redirectPC = out(UInt(config.wordLength bits)) // Stage 1
    }
    val rob = Vec.fill(config.robSize)(Reg(ROBEntry(config)))
    rob.foreach(_ init(ROBEntry(config).resetVal))
    val head = Vec.fill(config.retireWidth)(Reg(UInt(config.robIdxWidth bits)))
    (0 until config.retireWidth).map(i => { head(i).init(U(i)) })
    val tail = Vec.fill(config.decodeWidth)(Reg(UInt(config.robIdxWidth bits)))
    (0 until config.decodeWidth).map(i => { tail(i).init(U(i)) })
    val stage = ROBPipelineBundle(config)

    val retireMask = Bits(config.retireWidth bits).noCombLoopCheck() // May need to tune code for verilator
    val inhibitNextRetireMask = Bits(config.retireWidth bits)
    val flushMask = Bits(config.retireWidth bits)
    val flush = Bool()
    val idleEn = rob(head(0)).valid && rob(head(0)).specialOp === ROBSpecialOp.idle && io.interrupt // Handle IDLE in a special way

    val dispatchNum = CountOne(io.dispatch.allowMask)
    val retireNum = CountOne(retireMask)
    (0 until config.decodeWidth).map(i => { 
        stage.availROBMask(i) := ~(rob(tail(i)+dispatchNum).valid) // OR will retire this cycle? TBD!
        io.dispatch.robIdx(i) := tail(i).asBits // This is fine, direct from register
    })
    (0 until config.retireWidth).map(i => {
        if (i == 0) { // BP result controls retirement of the next instruction
            retireMask(i) := rob(head(i)).valid & rob(head(i)).isComplete & ~rob(head(i)).exceptionInfo.exception
            flushMask(i) := rob(head(i)).valid & rob(head(i)).isComplete & (rob(head(i)).exceptionInfo.exception | inhibitNextRetireMask(i))
        } else {
            retireMask(i) := rob(head(i)).valid & rob(head(i)).isComplete & ~rob(head(i)).exceptionInfo.exception & (retireMask(i-1 downto 0).andR) & ~inhibitNextRetireMask(i-1)
            flushMask(i) := rob(head(i)).valid & rob(head(i)).isComplete & (rob(head(i)).exceptionInfo.exception | inhibitNextRetireMask(i)) & (retireMask(i-1 downto 0).andR) & ~inhibitNextRetireMask(i-1)
        }
    })
    flush := flushMask.orR || idleEn

    (0 until config.robSize).map(i => {
        val idxMatchMaskDispatch = Bits(config.decodeWidth bits)
        (0 until config.decodeWidth).map(j => {
            idxMatchMaskDispatch(j) := tail(j) === i && io.dispatch.allowMask(j)
        })
        val idxMatchMaskCommit = Bits(config.issueWidth bits)
        (0 until config.issueWidth).map(j => {
            idxMatchMaskCommit(j) := io.commit(j).valid && io.commit(j).robIdx === i
        })
        val idxMatchMaskRetire = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(j => {
            idxMatchMaskRetire(j) := head(j) === i && retireMask(j)
        })
        rob(i).valid := (idxMatchMaskRetire.orR || io.flush) ? False | (rob(i).valid || idxMatchMaskDispatch.orR)
        rob(i).isComplete := (idxMatchMaskRetire.orR || io.flush) ? False | (rob(i).isComplete || idxMatchMaskCommit.orR)
    })
    (0 until config.decodeWidth).map(i => {
        when (io.dispatch.allowMask(i)) {
            rob(tail(i)).pc := io.dispatch.pc(i)
            rob(tail(i)).ard := io.dispatch.ard(i)
            rob(tail(i)).prd := io.dispatch.prd(i)
            rob(tail(i)).pprd := io.dispatch.pprd(i)
            rob(tail(i)).specialOp := io.dispatch.specialOp(i)
        }
        tail(i) := io.flush ? U(i).resized | tail(i) + dispatchNum
    })
    (0 until config.issueWidth).map(i => {
        when (io.commit(i).valid) {
            rob(io.commit(i).robIdx.asUInt).branchResult := io.commit(i).branchResult
            rob(io.commit(i).robIdx.asUInt).exceptionInfo := io.commit(i).exceptionInfo
        }
    })
    (0 until config.retireWidth).map(i => {
        inhibitNextRetireMask(i) := rob(head(i)).specialOp === ROBSpecialOp.lsuAction || rob(head(i)).specialOp === ROBSpecialOp.ll || rob(head(i)).specialOp === ROBSpecialOp.writeCSR || rob(head(i)).specialOp === ROBSpecialOp.ertn || rob(head(i)).branchResult.predictFail
        head(i) := io.flush ? U(i) | head(i) + retireNum
    })

    val delayedFlush = Reg(Bool())
    delayedFlush.init(False)
    delayedFlush := io.flush

    val noPPRDMask = Bits(config.retireWidth bits)
    val lsuActionMask = Bits(config.retireWidth bits)
    val llMask = Bits(config.retireWidth bits)
    val writeCSRMask = Bits(config.retireWidth bits)
    val ertnMask = Bits(config.retireWidth bits)
    val normalExceptionMask = Bits(config.retireWidth bits)
    val tlbrExceptionMask = Bits(config.retireWidth bits)
    val falseTakenMask = Bits(config.retireWidth bits)
    val retirePC = Vec.fill(config.retireWidth)(UInt(config.wordLength bits))
    val retireSNPC = Vec.fill(config.retireWidth)(UInt(config.wordLength bits))
    val retireTargetPC = Vec.fill(config.retireWidth)(UInt(config.wordLength bits))
    val retireEPC = idleEn ? retireSNPC(0) | MuxOH(flushMask, retirePC)

    val ertn = (ertnMask & retireMask).orR
    val normalException = MuxOH(flushMask, normalExceptionMask.asBools) | idleEn
    val tlbrException = MuxOH(flushMask, tlbrExceptionMask.asBools)
    val falseTaken = MuxOH(flushMask, falseTakenMask.asBools)
    val snpc = MuxOH(flushMask, retireSNPC)
    val targetPC = MuxOH(flushMask, retireTargetPC)

    val noPPRDMaskMid = Vec.fill(config.retireWidth)(Vec.fill(config.retireWidth)(Bool()))
    val freePRFIdxMid = Vec.fill(config.retireWidth)(Vec.fill(config.retireWidth)(Bits(config.prfIdxWidth bits)))
    val freePRFIdx = freePRFIdxMid(config.retireWidth-1)
    (0 until config.retireWidth).map(i => {
        (0 until config.retireWidth).map(j => {
            if (i == 0) {
                noPPRDMaskMid(0)(j) := noPPRDMask(j)
                freePRFIdxMid(0)(j) := rob(head(i)).pprd
            } else {
                if (j + 1 < config.retireWidth) {
                    noPPRDMaskMid(i)(j) := noPPRDMaskMid(i-1).asBits(j downto 0).andR ? noPPRDMaskMid(i-1)(j) | noPPRDMaskMid(i-1)(j+1)
                    freePRFIdxMid(i)(j) := noPPRDMaskMid(i-1).asBits(j downto 0).andR ? freePRFIdxMid(i-1)(j) | freePRFIdxMid(i-1)(j+1)
                } else {
                    noPPRDMaskMid(i)(j) := noPPRDMaskMid(i-1).asBits(j downto 0).andR ? noPPRDMaskMid(i-1)(j) | False
                    freePRFIdxMid(i)(j) := noPPRDMaskMid(i-1).asBits(j downto 0).andR ? freePRFIdxMid(i-1)(j) | B(0).resized
                }
            }
        })
    })

    (0 until config.retireWidth).map(i => {
        // WAW is handled by assign sequence, last assignment wins, no need for special treatment
        stage.retireARAT(i).ard := rob(head(i)).ard
        stage.retireARAT(i).prd := rob(head(i)).prd
        stage.retireARAT(i).wen := retireMask(i)

        noPPRDMask(i) := rob(head(i)).pprd === B(0).resized
        lsuActionMask(i) := rob(head(i)).specialOp === ROBSpecialOp.lsuAction
        llMask(i) := rob(head(i)).specialOp === ROBSpecialOp.ll
        writeCSRMask(i) := rob(head(i)).specialOp === ROBSpecialOp.writeCSR
        ertnMask(i) := rob(head(i)).specialOp === ROBSpecialOp.ertn
        normalExceptionMask(i) := rob(head(i)).isComplete && rob(head(i)).exceptionInfo.exception && (rob(head(i)).exceptionInfo.eCode =/= ECode.TLBR.eCode || rob(head(i)).exceptionInfo.eSubCode =/= ECode.TLBR.eSubCode)
        tlbrExceptionMask(i) := rob(head(i)).isComplete && rob(head(i)).exceptionInfo.exception && (rob(head(i)).exceptionInfo.eCode === ECode.TLBR.eCode && rob(head(i)).exceptionInfo.eSubCode === ECode.TLBR.eSubCode)
        falseTakenMask(i) := rob(head(i)).isComplete && ~rob(head(i)).branchResult.branchResult && rob(head(i)).branchResult.predictFail
        retirePC(i) := rob(head(i)).pc
        retireSNPC(i) := rob(head(i)).pc + 4
        retireTargetPC(i) := rob(head(i)).branchResult.targetPC
        stage.freePRFIdx(i) := freePRFIdx(i)
        stage.retireROBIdx(i) := head(i).asBits
        stage.retireEn(i) := retireMask(i)
        stage.updateBPU(i).pc := retirePC(i)
        stage.updateBPU(i).isJumpInst := rob(head(i)).specialOp === ROBSpecialOp.bpuUpdate
        stage.updateBPU(i).taken := rob(head(i)).branchResult.branchResult
        stage.updateBPU(i).predictFail := rob(head(i)).branchResult.predictFail
        stage.updateBPU(i).target := retireTargetPC(i)
    })
    stage.freePRFNum := CountOne(~noPPRDMask & retireMask)
    stage.wakeupMem := (lsuActionMask & retireMask).orR
    stage.retireLLBitUpdate := (llMask & retireMask).orR
    stage.retireWriteCSR := (writeCSRMask & retireMask).orR
    stage.retireERTN := ertn
    stage.retireNormalException := normalException
    stage.retireTLBRException := tlbrException
    stage.retireEPC := retireEPC
    stage.flush := flush
    stage.redirectPC := normalException ? io.csrCtrl.eentry | (tlbrException ? io.csrCtrl.tlbrentry | (ertn ? io.csrCtrl.era | (falseTaken ? snpc | targetPC))) // Ugly, but not that ugly

    val stageReg = Reg(ROBPipelineBundle(config))
    stageReg.init(ROBPipelineBundle(config).resetVal)

    stageReg := io.flush ? ROBPipelineBundle(config).resetVal | stage

    io.dispatch.availMask := stageReg.availROBMask
    io.retireARAT := stageReg.retireARAT
    io.retireFreeList.prfIdx := stageReg.freePRFIdx
    io.retireFreeList.writeNum := stageReg.freePRFNum
    io.retireFreeList.delayedFlush := delayedFlush
    io.retireLSU.robIdx := stageReg.retireROBIdx
    io.retireLSU.allowRetire := stageReg.retireEn
    io.wakeupMem := stageReg.wakeupMem
    io.csrCtrl.llBitUpdate := stageReg.retireLLBitUpdate
    io.csrCtrl.writeCSR := stageReg.retireWriteCSR
    io.csrCtrl.ertn := stageReg.retireERTN
    io.csrCtrl.normalException := stageReg.retireNormalException
    io.csrCtrl.tlbrException := stageReg.retireTLBRException
    io.csrCtrl.epc := stageReg.retireEPC
    io.flush := stageReg.flush
    io.redirectPC := stageReg.redirectPC

    (0 until config.retireWidth).map(i => {
        io.updateBPU(i).valid := stageReg.retireEn(i)
        io.updateBPU(i).payload := stageReg.updateBPU(i)
    })

}

case class ROBPipelineBundle(config: CPUConfig) extends Bundle {
    val availROBMask = Bits(config.decodeWidth bits)
    val retireARAT = Vec.fill(config.retireWidth)(RATIOBundle(true, false, config))
    val freePRFIdx = Vec.fill(config.retireWidth)(Bits(config.prfIdxWidth bits))
    val freePRFNum = UInt(config.retireNumWidth bits)
    val retireROBIdx = Vec.fill(config.retireWidth)(Bits(config.robIdxWidth bits))
    val retireEn = Vec.fill(config.retireWidth)(Bool())
    val wakeupMem = Bool()
    val retireLLBitUpdate = Bool()
    val retireWriteCSR = Bool()
    val retireERTN = Bool()
    val retireNormalException = Bool()
    val retireTLBRException = Bool()
    val retireEPC = UInt(config.wordLength bits)
    val updateBPU = Vec.fill(config.retireWidth)(BPUUpdateBundle(config))
    val flush = Bool()
    val redirectPC = UInt(config.wordLength bits)

    def resetVal: ROBPipelineBundle = {
        val value = ROBPipelineBundle(config)
        (0 until config.retireWidth).map(i => {
            value.retireARAT(i).ard := B(0).resized
            value.retireARAT(i).prd := B(0).resized
            value.retireARAT(i).wen := False
            value.freePRFIdx(i) := B(0).resized
            value.retireROBIdx(i) := B(0).resized
            value.retireEn(i) := False
            value.updateBPU(i).pc := U(0).resized
            value.updateBPU(i).isJumpInst := False
            value.updateBPU(i).taken := False
            value.updateBPU(i).predictFail := False
            value.updateBPU(i).target := U(0).resized
        })
        value.availROBMask := B(0).resized
        value.freePRFNum := U(0).resized
        value.wakeupMem := False
        value.retireLLBitUpdate := False
        value.retireWriteCSR := False
        value.retireERTN := False
        value.retireNormalException := False
        value.retireTLBRException := False
        value.retireEPC := U(0).resized
        value.flush := False
        value.redirectPC := U(0).resized
        return value
    }
}