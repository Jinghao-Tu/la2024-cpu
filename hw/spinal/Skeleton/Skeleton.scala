package Skeleton

import spinal.core._
import spinal.lib._

import bundle._
import config._
import frontend._
import backend._
import fu._
import mem._
import debug._

// Hardware definition
case class Skeleton(config: CPUConfig) extends Component {
  	val io = new Bundle {
		val aclk = in(Bool())
		val aresetn = in(Bool())
		val intrpt = in(Bits(Defs.extInterruptNum bits))

		val axi = master(AXIBundle(true, config)).setName("")

		val debug = out(LSDebugBundle(config))
  	}
  	setDefinitionName("mycpu_top")
  	noIoPrefix()

	val cpuClockDomain = ClockDomain(
		clock = io.aclk,
		reset = io.aresetn
	)
	val cpuClockingArea = new ClockingArea(cpuClockDomain) {
		val arbiter = AXIArbiter(config)
		val tlb = TLB(config)
		val memService = MemService(config)
		val sRAT = SRAT(config)
		val aRAT = ARAT(config)
		val freeList = FreeList(config)
		val pc = PC(config)
		val nextLinePredictor = NextLinePredictor(config)
		val iCache = ICache(config)
		val fuLSU  = DCache(config)
		val prf = PRF(config)
		val rob = ROB(config)
		val csr = CSR(config)
		val areaFlushReset = new ResetArea(rob.io.flush, true) {
			val instQueue = InstrQueue(config)
			val dispatcher = Dispatcher(config)
			val issueQueueALU0 = IssueQueue(4, false, FUType.csr    , config)
			val issueQueueALU1 = IssueQueue(4, false, FUType.counter, config)
			val issueQueueMULU = IssueQueue(4, false, FUType.mulu   , config)
			val issueQueueDIVU = IssueQueue(4,  true, FUType.divu   , config)
			val issueQueueLSU  = IssueQueue(4,  true, FUType.lsu    , config)
			val roALU0 = ReadOperandLogic(FUType.csr    , config)
			val roALU1 = ReadOperandLogic(FUType.counter, config)
			val roMULU = ReadOperandLogic(FUType.mulu   , config)
			val roDIVU = ReadOperandLogic(FUType.divu   , config)
			val roLSU  = ReadOperandLogic(FUType.lsu    , config)
			val fuALU0 = ALU(FUType.csr, config)
			val fuALU1 = ALU(FUType.counter, config)
			val fuMULU = MULU(config)
			val fuDIVU = DIVU(config)
			val commitALU0 = CommitLogic(config)
			val commitALU1 = CommitLogic(config)
			val commitMULU = CommitLogic(config)
			val commitDIVU = CommitLogic(config)
			val commitLSU  = CommitLogic(config)

			sRAT.io.writePort <> dispatcher.io.sratWrite
			sRAT.io.updatePort(0) <> commitALU0.io.srat
			sRAT.io.updatePort(1) <> commitALU1.io.srat
			sRAT.io.updatePort(2) <> commitMULU.io.srat
			sRAT.io.updatePort(3) <> commitDIVU.io.srat
			sRAT.io.updatePort(4) <>  commitLSU.io.srat
			sRAT.io.srcReadPort <> dispatcher.io.sratReadSrc
			sRAT.io.prevPRDReadPort <> dispatcher.io.sratReadPPRD

			freeList.io.dispatch <> dispatcher.io.freelist

			iCache.io.output <> instQueue.io.in

			instQueue.io.out <> dispatcher.io.input

			dispatcher.io.aluHasCSRInst(0) <> issueQueueALU0.io.csrInQueue
			dispatcher.io.aluHasCSRInst(1) <> issueQueueALU1.io.csrInQueue
			dispatcher.io.rob <> rob.io.dispatch
			dispatcher.io.plv <> csr.io.plv
			dispatcher.io.alu0IQ >> issueQueueALU0.io.input
			dispatcher.io.alu1IQ >> issueQueueALU1.io.input
			dispatcher.io.muluIQ >> issueQueueMULU.io.input
			dispatcher.io.divuIQ >> issueQueueDIVU.io.input
			dispatcher.io.lsuIQ  >> issueQueueLSU.io.input

			issueQueueALU0.io.output >-> roALU0.io.cmd
			issueQueueALU0.io.writebackSignal(0) <> commitALU0.io.prf.idx
			issueQueueALU0.io.writebackSignal(1) <> commitALU1.io.prf.idx
			issueQueueALU0.io.writebackSignal(2) <> commitMULU.io.prf.idx
			issueQueueALU0.io.writebackSignal(3) <> commitDIVU.io.prf.idx
			issueQueueALU0.io.writebackSignal(4) <>  commitLSU.io.prf.idx
			issueQueueALU0.io.earlyWakeup(0) << issueQueueALU0.io.wakeOut
			issueQueueALU0.io.earlyWakeup(1) << roALU0.io.wakeOut
			issueQueueALU0.io.earlyWakeup(2) << fuALU0.io.wakeOut
			issueQueueALU0.io.earlyWakeup(3) << issueQueueALU1.io.wakeOut
			issueQueueALU0.io.earlyWakeup(4) << roALU1.io.wakeOut
			issueQueueALU0.io.earlyWakeup(5) << fuALU1.io.wakeOut
			issueQueueALU0.io.earlyWakeup(6) << fuLSU.io.wakeOut(0)
			issueQueueALU0.io.earlyWakeup(7) << fuLSU.io.wakeOut(1)

			issueQueueALU1.io.output >-> roALU1.io.cmd
			issueQueueALU1.io.writebackSignal(0) <> commitALU0.io.prf.idx
			issueQueueALU1.io.writebackSignal(1) <> commitALU1.io.prf.idx
			issueQueueALU1.io.writebackSignal(2) <> commitMULU.io.prf.idx
			issueQueueALU1.io.writebackSignal(3) <> commitDIVU.io.prf.idx
			issueQueueALU1.io.writebackSignal(4) <>  commitLSU.io.prf.idx
			issueQueueALU1.io.earlyWakeup(0) << issueQueueALU0.io.wakeOut
			issueQueueALU1.io.earlyWakeup(1) << roALU0.io.wakeOut
			issueQueueALU1.io.earlyWakeup(2) << fuALU0.io.wakeOut
			issueQueueALU1.io.earlyWakeup(3) << issueQueueALU1.io.wakeOut
			issueQueueALU1.io.earlyWakeup(4) << roALU1.io.wakeOut
			issueQueueALU1.io.earlyWakeup(5) << fuALU1.io.wakeOut
			issueQueueALU1.io.earlyWakeup(6) << fuLSU.io.wakeOut(0)
			issueQueueALU1.io.earlyWakeup(7) << fuLSU.io.wakeOut(1)

			issueQueueMULU.io.output >-> roMULU.io.cmd
			issueQueueMULU.io.writebackSignal(0) <> commitALU0.io.prf.idx
			issueQueueMULU.io.writebackSignal(1) <> commitALU1.io.prf.idx
			issueQueueMULU.io.writebackSignal(2) <> commitMULU.io.prf.idx
			issueQueueMULU.io.writebackSignal(3) <> commitDIVU.io.prf.idx
			issueQueueMULU.io.writebackSignal(4) <>  commitLSU.io.prf.idx
			issueQueueMULU.io.earlyWakeup(0) << fuLSU.io.wakeOut(0)
			issueQueueMULU.io.earlyWakeup(1) << fuLSU.io.wakeOut(1)

			issueQueueDIVU.io.output >-> roDIVU.io.cmd
			issueQueueDIVU.io.writebackSignal(0) <> commitALU0.io.prf.idx
			issueQueueDIVU.io.writebackSignal(1) <> commitALU1.io.prf.idx
			issueQueueDIVU.io.writebackSignal(2) <> commitMULU.io.prf.idx
			issueQueueDIVU.io.writebackSignal(3) <> commitDIVU.io.prf.idx
			issueQueueDIVU.io.writebackSignal(4) <>  commitLSU.io.prf.idx

			issueQueueLSU.io.output >-> roLSU.io.cmd
			issueQueueLSU.io.writebackSignal(0) <> commitALU0.io.prf.idx
			issueQueueLSU.io.writebackSignal(1) <> commitALU1.io.prf.idx
			issueQueueLSU.io.writebackSignal(2) <> commitMULU.io.prf.idx
			issueQueueLSU.io.writebackSignal(3) <> commitDIVU.io.prf.idx
			issueQueueLSU.io.writebackSignal(4) <>  commitLSU.io.prf.idx
			issueQueueLSU.io.earlyWakeup( 0) << issueQueueALU0.io.wakeOut
			issueQueueLSU.io.earlyWakeup( 1) << roALU0.io.wakeOut
			issueQueueLSU.io.earlyWakeup( 2) << fuALU0.io.wakeOut
			issueQueueLSU.io.earlyWakeup( 3) << issueQueueALU1.io.wakeOut
			issueQueueLSU.io.earlyWakeup( 4) << roALU1.io.wakeOut
			issueQueueLSU.io.earlyWakeup( 5) << fuALU1.io.wakeOut
			issueQueueLSU.io.earlyWakeup( 6) << fuLSU.io.wakeOut(0)
			issueQueueLSU.io.earlyWakeup( 7) << fuLSU.io.wakeOut(1)
			issueQueueLSU.io.earlyWakeup( 8) << fuMULU.io.wakeOut(0)
			issueQueueLSU.io.earlyWakeup( 9) << fuMULU.io.wakeOut(1)
			issueQueueLSU.io.earlyWakeup(10) << fuMULU.io.wakeOut(2)

			roALU0.io.toFU >-> fuALU0.io.input
			roALU0.io.forward(0) << fuALU0.io.forward
			roALU0.io.forward(1) << commitALU0.io.forward
			roALU0.io.forward(2) << fuALU1.io.forward
			roALU0.io.forward(3) << commitALU1.io.forward
			roALU0.io.forward(4) << commitLSU.io.forward
			roALU0.io.prf <> prf.io.read(0)
			roALU0.io.csr <> csr.io.swRead
			roALU0.io.interrupt <> csr.io.interrupt

			roALU1.io.toFU >-> fuALU1.io.input
			roALU1.io.forward(0) << fuALU0.io.forward
			roALU1.io.forward(1) << commitALU0.io.forward
			roALU1.io.forward(2) << fuALU1.io.forward
			roALU1.io.forward(3) << commitALU1.io.forward
			roALU1.io.forward(4) << commitLSU.io.forward
			roALU1.io.prf <> prf.io.read(1)
			roALU1.io.counter <> csr.io.counter
			roALU1.io.interrupt <> csr.io.interrupt

			roMULU.io.toFU >-> fuMULU.io.input
			roMULU.io.forward(0) << commitLSU.io.forward
			roMULU.io.prf <> prf.io.read(2)
			roMULU.io.interrupt <> csr.io.interrupt

			roDIVU.io.toFU >-> fuDIVU.io.input
			roDIVU.io.prf <> prf.io.read(3)
			roDIVU.io.interrupt <> csr.io.interrupt

			roLSU.io.toFU >-> fuLSU.io.input
			roLSU.io.forward(0) << fuALU0.io.forward
			roLSU.io.forward(1) << commitALU0.io.forward
			roLSU.io.forward(2) << fuALU1.io.forward
			roLSU.io.forward(3) << commitALU1.io.forward
			roLSU.io.forward(4) << commitLSU.io.forward
			roLSU.io.forward(5) << fuMULU.io.forward
			roLSU.io.forward(6) << commitMULU.io.forward
			roLSU.io.prf <> prf.io.read(4)
			roLSU.io.interrupt <> csr.io.interrupt

			fuALU0.io.output >-> commitALU0.io.input
			fuALU0.io.csrWrite <> csr.io.swWrite

			fuALU1.io.output >-> commitALU1.io.input

			fuMULU.io.output >-> commitMULU.io.input

			fuDIVU.io.output >-> commitDIVU.io.input

			fuLSU.io.output >-> commitLSU.io.input

			commitALU0.io.rob <> rob.io.commit(0)
			commitALU0.io.prf <> prf.io.write(0)

			commitALU1.io.rob <> rob.io.commit(1)
			commitALU1.io.prf <> prf.io.write(1)

			commitMULU.io.rob <> rob.io.commit(2)
			commitMULU.io.prf <> prf.io.write(2)

			commitDIVU.io.rob <> rob.io.commit(3)
			commitDIVU.io.prf <> prf.io.write(3)

			commitLSU.io.rob <> rob.io.commit(4)
			commitLSU.io.prf <> prf.io.write(4)
		}

		arbiter.io.out <> io.axi
		arbiter.io.iCache <> iCache.io.axi
		arbiter.io.dCache <> fuLSU.io.axi

		tlb.io.iCacheReq <> iCache.io.tlb
		tlb.io.dCacheReq <> fuLSU.io.tlb
		tlb.io.csrInfo <> csr.io.tlbCSRInfo
		tlb.io.csrWrite <> csr.io.tlbCSRWrite
		tlb.io.ctrl <> memService.io.TLBCtrl

		memService.io.input << fuLSU.io.specialOpBufferUpdate
		memService.io.iCacheCtrl <> iCache.io.ctrl
		memService.io.dCacheCtrl <> fuLSU.io.ctrl
		memService.io.flush <> rob.io.flush
		memService.io.wake <> rob.io.wakeupMem

		sRAT.io.delayedRecovery <> rob.io.retireFreeList.delayedFlush
		sRAT.io.recoveryPort <> aRAT.io.recoveryPort

		aRAT.io.retirePort <> rob.io.retireARAT

		freeList.io.retire <> rob.io.retireFreeList

		fuLSU.io.retireComm <> rob.io.retireLSU
		fuLSU.io.plv <> csr.io.plv
		fuLSU.io.llBitComm <> csr.io.llBitComm
		fuLSU.io.flush <> rob.io.flush
		fuLSU.io.badv <> csr.io.badvDCache

		(0 until config.fetchWidth).map(i => {
			pc.io.iCacheFeed(i) >> iCache.io.input(i)
			pc.io.pc(i) >> nextLinePredictor.io.pc(i)
			pc.io.npc(i) << nextLinePredictor.io.npc(i)
		})

		(0 until config.retireWidth).map(i => {
			nextLinePredictor.io.updateInfo(i) << rob.io.updateBPU(i)
		})

		pc.io.branchInfo <> nextLinePredictor.io.branchInfo
		pc.io.flush <> rob.io.flush
		pc.io.redirectPC <> rob.io.redirectPC

		iCache.io.plv <> csr.io.plv
		iCache.io.flush <> rob.io.flush
		iCache.io.badv <> csr.io.badvICache

		rob.io.csrCtrl <> csr.io.ctrl
		rob.io.interrupt <> csr.io.interrupt

		csr.io.extInt <> io.intrpt
		csr.io.flush <> rob.io.flush
		
		if (config.debug) {
			val debugQueue = DebugQueue(config)
			debugQueue.io.debug0_valid    <> rob.io.updateBPU(0).valid
			debugQueue.io.debug0_wb_pc    <> rob.io.updateBPU(0).payload.pc
			debugQueue.io.debug0_wb_wen   <> rob.io.retireARAT(0).wen
			debugQueue.io.debug0_wb_wnum  <> U(rob.io.retireARAT(0).ard)
			debugQueue.io.debug0_wb_wdata <> prf.io.debugRegs(rob.io.retireARAT(0).prd.asUInt)
			debugQueue.io.debug1_valid    <> rob.io.updateBPU(1).valid
			debugQueue.io.debug1_wb_pc    <> rob.io.updateBPU(1).payload.pc
			debugQueue.io.debug1_wb_wen   <> rob.io.retireARAT(1).wen
			debugQueue.io.debug1_wb_wnum  <> U(rob.io.retireARAT(1).ard)
			debugQueue.io.debug1_wb_wdata <> prf.io.debugRegs(rob.io.retireARAT(1).prd.asUInt)
			io.debug.wb_pc       <> debugQueue.io.debug_wb_pc.asBits
			io.debug.wb_rf_wen   <> debugQueue.io.debug_wb_wen.asBits
			io.debug.wb_rf_wnum  <> debugQueue.io.debug_wb_wnum.asBits
			io.debug.wb_rf_wdata <> debugQueue.io.debug_wb_wdata.asBits
		} else {
			io.debug.wb_pc    := B(0).resized
			io.debug.wb_rf_wen := B(0).resized
			io.debug.wb_rf_wnum := B(0).resized
			io.debug.wb_rf_wdata := B(0).resized
		}
	}

}

object SkeletonVerilog extends App {
  	Config.spinal.generateVerilog(Skeleton(CPUConfig()))
  	// Config.spinal.generateVerilog(DebugQueue(CPUConfig()))
}
