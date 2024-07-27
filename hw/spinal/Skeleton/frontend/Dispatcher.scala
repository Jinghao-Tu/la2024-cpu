package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class Dispatcher(config: CPUConfig) extends Component { // Also renamer in fact
    val io = new Bundle {
        val input = master(InstrQueueOutBundle(config))
        val aluHasCSRInst = in(Vec.fill(2)(Bool()))
        val rob = master(ROBDispatchIOBundle(config))
        val sratWrite = Vec.fill(config.decodeWidth)(master(RATIOBundle(true, false, config)))
        val sratReadSrc = Vec.fill(config.decodeWidth)(Vec.fill(2)(master(RATIOBundle(false, false, config))))
        val sratReadPPRD = Vec.fill(config.decodeWidth)(master(RATIOBundle(false, false, config)))
        val freelist = master(FreeListDispatchIOBundle(config))
        val plv = in(CSRBundle(config).crmd.plv) // Decoder needs this
        val alu0IQ = master Stream(IssueQueueDispatchIOBundle(FUType.csr, config))
        val alu1IQ = master Stream(IssueQueueDispatchIOBundle(FUType.counter, config))
        val muluIQ = master Stream(IssueQueueDispatchIOBundle(FUType.mulu, config))
        val divuIQ = master Stream(IssueQueueDispatchIOBundle(FUType.divu, config))
        val lsuIQ = master Stream(IssueQueueDispatchIOBundle(FUType.lsu, config))
    }
    val resAvail = Bits(config.decodeWidth bits)
    val needRd = Bits(config.decodeWidth bits)
    (0 until config.decodeWidth).map(i => {
        needRd(i) := io.input.dispatchInfo(i).ard =/= B(0).resized
    })
    val freelistRdShuffle = Vec.fill(config.decodeWidth)(Bits(config.prfIdxWidth bits))
    val freelistAvail = Bits(config.decodeWidth bits)
    (0 until config.decodeWidth).map(i => {
        if (i == 0) {
            when (needRd(i)) {
                freelistRdShuffle(i) := io.freelist.prfIdx(0)
                freelistAvail(i) := io.freelist.availMask(0)
            } otherwise {
                freelistRdShuffle(i) := B(0).resized
                freelistAvail(i) := True
            }
        } else {
            when (needRd(i)) {
                freelistRdShuffle(i) := io.freelist.prfIdx(CountOne(needRd(i-1 downto 0)).resize(log2Up(config.decodeWidth) bits)) // Use cascaded MUXes if timing needs optimization
                freelistAvail(i) := io.freelist.availMask(CountOne(needRd(i-1 downto 0)).resize(log2Up(config.decodeWidth) bits))
            } otherwise {
                freelistRdShuffle(i) := B(0).resized
                freelistAvail(i) := True
            }
        }
    })
    val robAvail = io.rob.availMask
    val iqAvail = Bits(config.decodeWidth bits)

    val csrReq = Bits(config.decodeWidth bits)
    val counterReq = Bits(config.decodeWidth bits)
    val alu0Req = Bits(config.decodeWidth bits)
    val alu1Req = Bits(config.decodeWidth bits)
    val muluReq = Bits(config.decodeWidth bits)
    val divuReq = Bits(config.decodeWidth bits)
    val lsuReq = Bits(config.decodeWidth bits)
    (0 until config.decodeWidth).map(i => {
        csrReq(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.csr && ~io.aluHasCSRInst(0) && io.alu0IQ.ready
        counterReq(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.counter && ~io.aluHasCSRInst(1) && io.alu1IQ.ready
        alu0Req(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.alu && io.alu0IQ.ready
        alu1Req(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.alu && io.alu1IQ.ready
        muluReq(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.mulu && io.muluIQ.ready
        divuReq(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.divu && io.divuIQ.ready
        lsuReq(i) := io.input.availMask(i) && io.input.dispatchInfo(i).fuType === FUType.lsu && io.lsuIQ.ready
    })
    // Dispatch scheme for ALU insts is described as below:
    // ALU with lower number has priority on inst dispatch, so cascaded masking is used here(timing may be a problem!)
    // At now only the pattern (Arithmetic, CSR) will waste hardware source
    val alu0Sel = OHMasking.first(csrReq | alu0Req)
    val alu1Sel = OHMasking.first((counterReq | alu1Req) & ~alu0Sel)
    val muluSel = OHMasking.first(muluReq)
    val divuSel = OHMasking.first(divuReq)
    val lsuSel = OHMasking.first(lsuReq)
    iqAvail := alu0Sel | alu1Sel | muluSel | divuSel | lsuSel
    resAvail := freelistAvail & robAvail & iqAvail
    val dispatchMask = Bits(config.decodeWidth bits)
    (0 until config.decodeWidth).map(i => {
        dispatchMask(i) := resAvail(i downto 0).andR & io.input.availMask(i)
    })
    io.input.allowMask := dispatchMask
    io.rob.allowMask := dispatchMask
    io.freelist.disPatchNum := CountOne(dispatchMask & needRd)

    val decoder = Array.fill(config.decodeWidth)(Decoder(config))
    val alu0Candidate = Vec.fill(config.decodeWidth)(IssueQueueDispatchIOBundle(FUType.csr, config))
    val alu1Candidate = Vec.fill(config.decodeWidth)(IssueQueueDispatchIOBundle(FUType.counter, config))
    val muluCandidate = Vec.fill(config.decodeWidth)(IssueQueueDispatchIOBundle(FUType.mulu, config))
    val divuCandidate = Vec.fill(config.decodeWidth)(IssueQueueDispatchIOBundle(FUType.divu, config))
    val lsuCandidate = Vec.fill(config.decodeWidth)(IssueQueueDispatchIOBundle(FUType.lsu, config))
    // These handle RAW and WAW hazards
    val actualSrc = Vec.fill(config.decodeWidth)(Vec.fill(2)(Bits(config.prfIdxWidth bits)))
    val actualRd = Vec.fill(config.decodeWidth)(Bits(config.prfIdxWidth bits))
    val actualpRd = Vec.fill(config.decodeWidth)(Bits(config.prfIdxWidth bits))
    val actualSrcReady = Vec.fill(config.decodeWidth)(Vec.fill(2)(Bool()))
    (0 until config.decodeWidth).map(i => {
        (0 until 2).map(j => {
            val rawMask = Bits(i+1 bits) // Use cascaded MUXes if timing needs optimization
            val pSrcVec = Vec.fill(i+1)(Bits(config.prfIdxWidth bits))
            (0 until i+1).map(k => {
                if (k == 0) {
                    rawMask(k) := True
                    pSrcVec(k) := io.sratReadSrc(i)(j).prd
                }
                else {
                    rawMask(k) := io.input.dispatchInfo(k-1).ard === io.input.dispatchInfo(i).asrc(j)
                    pSrcVec(k) := actualRd(k-1)
                }
            })
            actualSrc(i)(j) := MuxOH(OHMasking.last(rawMask), pSrcVec)
            actualSrcReady(i)(j) := Mux(rawMask(i downto 1).orR, False, io.sratReadSrc(i)(j).valid)
        })
        actualRd(i) := freelistRdShuffle(i)
        val wawMask = Bits(i+1 bits) // Use cascaded MUXes if timing needs optimization
        val pRdVec = Vec.fill(i+1)(Bits(config.prfIdxWidth bits))
        (0 until i+1).map(j => {
            if (j == 0) {
                    wawMask(j) := True
                    pRdVec(j) := io.sratReadPPRD(i).prd
                }
                else {
                    wawMask(j) := io.input.dispatchInfo(j-1).ard === io.input.dispatchInfo(i).ard
                    pRdVec(j) := actualRd(j-1)
                }
        })
        actualpRd(i) := MuxOH(OHMasking.last(wawMask), pRdVec)
    })

    (0 until config.decodeWidth).map(i => {
        decoder(i).io.info := io.input.info(i)
        decoder(i).io.plv := io.plv

        // For IQ Mux selection
        alu0Candidate(i).robIdx := io.rob.robIdx(i)
        alu1Candidate(i).robIdx := io.rob.robIdx(i)
        muluCandidate(i).robIdx := io.rob.robIdx(i)
        divuCandidate(i).robIdx := io.rob.robIdx(i)
         lsuCandidate(i).robIdx := io.rob.robIdx(i)
        alu0Candidate(i).branchInfo := decoder(i).io.branchInfo
        alu1Candidate(i).branchInfo := decoder(i).io.branchInfo
        muluCandidate(i).branchInfo := decoder(i).io.branchInfo
        divuCandidate(i).branchInfo := decoder(i).io.branchInfo
         lsuCandidate(i).branchInfo := decoder(i).io.branchInfo
        alu0Candidate(i).branchResult := decoder(i).io.branchResult
        alu1Candidate(i).branchResult := decoder(i).io.branchResult
        muluCandidate(i).branchResult := decoder(i).io.branchResult
        divuCandidate(i).branchResult := decoder(i).io.branchResult
         lsuCandidate(i).branchResult := decoder(i).io.branchResult
        alu0Candidate(i).exceptionInfo := decoder(i).io.exceptionInfo
        alu1Candidate(i).exceptionInfo := decoder(i).io.exceptionInfo
        muluCandidate(i).exceptionInfo := decoder(i).io.exceptionInfo
        divuCandidate(i).exceptionInfo := decoder(i).io.exceptionInfo
         lsuCandidate(i).exceptionInfo := decoder(i).io.exceptionInfo
        alu0Candidate(i).pc := decoder(i).io.pc
        alu1Candidate(i).pc := decoder(i).io.pc
        muluCandidate(i).pc := decoder(i).io.pc
        divuCandidate(i).pc := decoder(i).io.pc
         lsuCandidate(i).pc := decoder(i).io.pc
        alu0Candidate(i).prd := actualRd(i)
        alu1Candidate(i).prd := actualRd(i)
        muluCandidate(i).prd := actualRd(i)
        divuCandidate(i).prd := actualRd(i)
         lsuCandidate(i).prd := actualRd(i)
        (0 until 2).map(j => {
            alu0Candidate(i).psrc(j) := actualSrc(i)(j)
            alu1Candidate(i).psrc(j) := actualSrc(i)(j)
            muluCandidate(i).psrc(j) := actualSrc(i)(j)
            divuCandidate(i).psrc(j) := actualSrc(i)(j)
             lsuCandidate(i).psrc(j) := actualSrc(i)(j)
            alu0Candidate(i).srcReady(j) := actualSrcReady(i)(j)
            alu1Candidate(i).srcReady(j) := actualSrcReady(i)(j)
            muluCandidate(i).srcReady(j) := actualSrcReady(i)(j)
            divuCandidate(i).srcReady(j) := actualSrcReady(i)(j)
             lsuCandidate(i).srcReady(j) := actualSrcReady(i)(j)
        })
        alu0Candidate(i).imm := decoder(i).io.imm
        alu1Candidate(i).imm := decoder(i).io.imm
        muluCandidate(i).imm := decoder(i).io.imm
        divuCandidate(i).imm := decoder(i).io.imm
         lsuCandidate(i).imm := decoder(i).io.imm
        alu0Candidate(i).uop := decoder(i).io.uopALU0
        alu1Candidate(i).uop := decoder(i).io.uopALU1
        muluCandidate(i).uop := decoder(i).io.uopMULU
        divuCandidate(i).uop := decoder(i).io.uopDIVU
         lsuCandidate(i).uop := decoder(i).io.uopLSU
        alu0Candidate(i).roop := decoder(i).io.roopALU0
        alu1Candidate(i).roop := decoder(i).io.roopALU1
         lsuCandidate(i).roop := decoder(i).io.roopLSU

        // Bind ROB IO
        io.rob.pc(i) := decoder(i).io.pc
        io.rob.ard(i) := io.input.dispatchInfo(i).ard
        io.rob.prd(i) := actualRd(i)
        io.rob.pprd(i) := actualpRd(i)
        io.rob.specialOp(i) := decoder(i).io.specialOp

        // Bind sRAT IO
        io.sratWrite(i).ard := io.input.dispatchInfo(i).ard
        io.sratWrite(i).prd := actualRd(i)
        io.sratWrite(i).wen := dispatchMask(i) & needRd(i)
        io.sratReadPPRD(i).ard := io.input.dispatchInfo(i).ard
        (0 until 2).map(j => {
            io.sratReadSrc(i)(j).ard := io.input.dispatchInfo(i).asrc(j)
        })
    })
    io.alu0IQ.valid := (alu0Sel & dispatchMask).orR
    io.alu0IQ.payload := MuxOH(alu0Sel, alu0Candidate)
    io.alu1IQ.valid := (alu1Sel & dispatchMask).orR
    io.alu1IQ.payload := MuxOH(alu1Sel, alu1Candidate)
    io.muluIQ.valid := (muluSel & dispatchMask).orR
    io.muluIQ.payload := MuxOH(muluSel, muluCandidate)
    io.divuIQ.valid := (divuSel & dispatchMask).orR
    io.divuIQ.payload := MuxOH(divuSel, divuCandidate)
    io.lsuIQ.valid := (lsuSel & dispatchMask).orR
    io.lsuIQ.payload := MuxOH(lsuSel, lsuCandidate)
}