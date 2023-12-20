package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class IssueQueue(size: Int, iqType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Component {
    val wakeupPortNum = iqType match {
        case FUType.counter => 2 // From ALU0, LSU
        case FUType.csr => 2 // From ALU1, LSU
        case FUType.mulu => 1 // From LSU
        case FUType.lsu => 3 // From ALU0, ALU1, LSU, MULU
        case _ => 0
    }
    val io = new Bundle {
        val input = slave Stream(IssueQueueDispatchIOBundle(iqType, config)) // 0-latency!
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
            readyToIssue(i) := queue(i).valid & (queue(i).srcReady(0) | queue(i).srcWakeup(0)) & (queue(i).srcReady(1) | queue(i).srcWakeup(1))
        } else {
            readyToIssue(i) := queue(i).valid & queue(i).srcReady.asBits.andR
        }
    })
    val issueVector = OHMasking.first(readyToIssue)
    val shiftAhead = Bits(size bits)
    val emptyEntry = Bits(size+1 bits)
    (0 until size+1).map(i => { if (i < size) emptyEntry(i) := ~(queue(i).valid) else emptyEntry(i) := True })
    val writeVector = OHMasking.first(emptyEntry)
    val updatedEntry = Vec.fill(size)(IssueQueueEntry(iqType, config))
    val appendEntry = IssueQueueEntry(iqType, config) // The entry which might be append to queue tail this cycle
    appendEntry.valid := io.input.valid
    appendEntry.branchInfo := io.input.payload.branchInfo
    appendEntry.exceptionInfo := io.input.payload.exceptionInfo
    appendEntry.pc := io.input.payload.pc
    appendEntry.prd := io.input.payload.prd
    appendEntry.psrc := io.input.payload.psrc
    appendEntry.imm := io.input.payload.imm
    appendEntry.uop := io.input.payload.uop
    if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.lsu) appendEntry.roop := io.input.payload.roop
    appendEntry.srcReady := io.input.payload.srcReady | monitorWriteback(io.input.payload.psrc)
    if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.mulu || iqType == FUType.lsu) appendEntry.srcWakeup := monitorWakeup(io.input.payload.psrc)
    (0 until size).map(i => {
        updatedEntry(i) := queue(i)
        updatedEntry(i).allowOverride
        updatedEntry(i).srcReady := queue(i).srcReady | monitorWriteback(queue(i).psrc)
        if (iqType == FUType.counter || iqType == FUType.csr || iqType == FUType.mulu || iqType == FUType.lsu) updatedEntry(i).srcWakeup := monitorWakeup(queue(i).psrc) // Note that this will clean wakeup signal from the last cycle as we expected
    })
    val queueNext = Vec.fill(size)(IssueQueueEntry(iqType, config)) // OK, let's get next value of the queue
    
    (0 until size).map(i => { 
        shiftAhead(i) := readyToIssue(i downto 0).orR & io.output.ready
        emptyEntry(i) := ~(queue(i).valid)
        when (shiftAhead(i)) {
            if (i < size-1) {
                when (writeVector(i+1)) {
                    queueNext(i) := appendEntry
                } otherwise {
                    queueNext(i) := queue(i+1)
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
            }
        }
    })
    queue := queueNext
    
    io.input.ready := emptyEntry(size-1 downto 0).orR | io.output.ready
    val issueEntry = MuxOH(issueVector, queue)
    io.output.valid := readyToIssue.orR
    io.output.payload.branchInfo := issueEntry.branchInfo
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