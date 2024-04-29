package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class IssueQueue(size: Int, issueByOrder: Boolean, iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Component {
    val wakeupPortNum = iqType match {
        case FUType.counter => config.aluWakeCount * 2 + config.lsuWakeCount // From ALU0, ALU1, LSU
        case FUType.csr => config.aluWakeCount * 2 + config.lsuWakeCount // From ALU0, ALU1, LSU
        case FUType.mulu => config.lsuWakeCount // From LSU
        case FUType.lsu => config.aluWakeCount * 2 + config.lsuWakeCount + config.muluWakeCount // From ALU0, ALU1, LSU, MULU
        case _ => 0
    }
    val io = new Bundle {
        val input = slave Stream(IssueQueueDispatchIOBundle(iqType, config)) // 0-latency!
        val csrInQueue = if (iqType == FUType.counter || iqType == FUType.csr) out(Bool()) else null
        val output = master Stream(IssueQueueROIOBundle(iqType, config)) // 1-latency, has a pipeline register
        val writebackSignal = in(Vec.fill(config.issueWidth)(Bits(config.prfIdxWidth bits)))
        val earlyWakeup = if (wakeupPortNum > 0) Vec.fill(wakeupPortNum)(slave(Flow(Bits(config.prfIdxWidth bits)))) else null // 0-latency!
        val wakeOut = if (iqType == FUType.counter || iqType == FUType.csr) master(Flow(Bits(config.prfIdxWidth bits))) else null // 0-latency!
    }
    val queue = Vec.fill(size)(Reg(IssueQueueEntry(iqType, config)))
    queue.foreach(_ init(IssueQueueEntry(iqType, config).resetVal)) // The only thing important is to make valid false

    val readyToIssue = Bits(size bits)
    (0 until size).map(i => {
        if (iqType != FUType.divu) {
            if (i > 0 && issueByOrder) {
                readyToIssue(i) := False
            } else {
                readyToIssue(i) := queue(i).valid & queue(i).srcReady(0) & queue(i).srcReady(1)
            }
        } else {
            if (i > 0 && issueByOrder) {
                readyToIssue(i) := False
            } else {
                readyToIssue(i) := queue(i).valid & queue(i).srcReady.asBits.andR
            }
        }
    })
    val issueVector = OHMasking.first(readyToIssue)
    val shiftAhead = Bits(size bits)
    val emptyEntry = Bits(size+1 bits)
    (0 until size+1).map(i => { if (i < size) emptyEntry(i) := ~(queue(i).valid) else emptyEntry(i) := True })
    val writeVector = OHMasking.first(emptyEntry)
    val appendEntry = IssueQueueEntry(iqType, config) // The entry which might be append to queue tail this cycle
    appendEntry.valid := io.input.valid
    appendEntry.robIdx := io.input.robIdx
    if (iqType == FUType.counter || iqType == FUType.csr) appendEntry.branchInfo := io.input.payload.branchInfo
    else appendEntry.branchResult := io.input.payload.branchResult
    appendEntry.exceptionInfo := io.input.payload.exceptionInfo
    appendEntry.pc := io.input.payload.pc
    appendEntry.prd := io.input.payload.prd
    appendEntry.psrc := io.input.payload.psrc
    appendEntry.imm := io.input.payload.imm
    appendEntry.uop := io.input.payload.uop
    if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) appendEntry.roop := io.input.payload.roop
    if (wakeupPortNum > 0) {
        appendEntry.srcReady := io.input.payload.srcReady | monitorWriteback(io.input.payload.psrc) | monitorWakeup(io.input.payload.psrc)
    } else {
        appendEntry.srcReady := io.input.payload.srcReady | monitorWriteback(io.input.payload.psrc)
    }

    val queueNext = Vec.fill(size)(IssueQueueEntry(iqType, config)) // OK, let's get next value of the queue
    queueNext.allowOverride()
    
    (0 until size).map(i => { 
        shiftAhead(i) := readyToIssue(i downto 0).orR & io.output.ready
        emptyEntry(i) := ~(queue(i).valid)
        when (shiftAhead(i)) {
            if (i < size-1) {
                when (writeVector(i+1)) {
                    queueNext(i) := appendEntry
                } otherwise {
                    queueNext(i) := queue(i+1)
                    if (wakeupPortNum > 0) {
                        queueNext(i).srcReady := queue(i+1).srcReady | monitorWriteback(queue(i).psrc) | monitorWakeup(queue(i).psrc)
                    } else {
                        queueNext(i).srcReady := queue(i+1).srcReady | monitorWriteback(queue(i).psrc)
                    }
                }
            } else {
                when (writeVector(i+1)) {
                    queueNext(i) := appendEntry
                } otherwise {
                    queueNext(i) := IssueQueueEntry(iqType, config).resetVal
                }
            }
        } otherwise {
            when (writeVector(i)) {
                queueNext(i) := appendEntry
            } otherwise {
                queueNext(i) := queue(i)
                if (wakeupPortNum > 0) {
                    queueNext(i).srcReady := queue(i).srcReady | monitorWriteback(queue(i).psrc) | monitorWakeup(queue(i).psrc)
                } else {
                    queueNext(i).srcReady := queue(i).srcReady | monitorWriteback(queue(i).psrc)
                }
            }
        }
    })
    queue := queueNext
    
    if (iqType == FUType.counter || iqType == FUType.csr) {
        val isCSRinst = Bits(size bits)
        (0 until size).map(i => { isCSRinst(i) := (queue(i).roop.aluROOp === ALUROOp.csr) && queue(i).valid })
        io.csrInQueue := isCSRinst.orR // Note that this will have 1 cycle bubble on continous CSR operations
        // Not compressing bubble to shorten cycle time
    }
    io.input.ready := emptyEntry(size-1 downto 0).orR | io.output.fire
    val issueEntry = MuxOH(issueVector, queue)
    io.output.valid := readyToIssue.orR
    io.output.robIdx := issueEntry.robIdx
    if (iqType == FUType.counter || iqType == FUType.csr) io.output.payload.branchInfo := issueEntry.branchInfo
    else io.output.payload.branchResult := issueEntry.branchResult
    io.output.payload.exceptionInfo := issueEntry.exceptionInfo
    io.output.payload.pc := issueEntry.pc
    io.output.payload.prd := issueEntry.prd
    io.output.payload.psrc := issueEntry.psrc
    io.output.payload.imm := issueEntry.imm
    io.output.payload.uop := issueEntry.uop
    if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) io.output.payload.roop := issueEntry.roop
    if (iqType == FUType.counter || iqType == FUType.csr) {
        io.wakeOut.payload := issueEntry.prd
        io.wakeOut.valid := io.output.fire
    }

    def monitorWriteback(psrc: Vec[Bits]): Vec[Bool] = {
        val valid = Vec.fill(2)(Vec.fill(config.issueWidth)(Bool()))
        (0 until config.issueWidth).map(i => { valid(0)(i) := psrc(0) === io.writebackSignal(i) })
        (0 until config.issueWidth).map(i => { valid(1)(i) := psrc(1) === io.writebackSignal(i) })
        val value = Vec.fill(2)(Bool())
        (0 until 2).map(i => { value(i) := valid(i).orR })
        return value
    }
    def monitorWakeup(psrc: Vec[Bits]): Vec[Bool] = {
        if (wakeupPortNum > 0) {
            val valid = Vec.fill(2)(Vec.fill(wakeupPortNum)(Bool()))
            (0 until wakeupPortNum).map(i => valid(0)(i) := (psrc(0) === io.earlyWakeup(i).payload) & io.earlyWakeup(i).valid)
            (0 until wakeupPortNum).map(i => valid(1)(i) := (psrc(1) === io.earlyWakeup(i).payload) & io.earlyWakeup(i).valid)
            val value = Vec.fill(2)(Bool())
            if (iqType == FUType.counter || iqType == FUType.csr) { // Self-wakeup at issue stage
                (0 until 2).map(i => { value(i) := valid(i).orR | ((psrc(i) === io.wakeOut.payload) & io.wakeOut.valid) })
            } else {
                (0 until 2).map(i => { value(i) := valid(i).orR })
            }
            return value
        } else {
            return null
        }
    }
}