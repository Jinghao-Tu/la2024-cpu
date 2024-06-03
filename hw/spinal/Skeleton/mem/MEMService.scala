package Skeleton.mem

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import Skeleton.bundle._
import Skeleton.config._

case class MemService(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Flow(SpecialOpBufferUpdateBundle(config))
        val iCacheCtrl = master(CacheCtrlBundle(config))
        val dCacheCtrl = master(CacheCtrlBundle(config))
        val TLBCtrl = master(TLBCtrlBundle(config))
        val flush = in(Bool())
        val wake = in(Bool())
    }

    // Some special operation stuff
    val opBuffer = Reg(SpecialOpBufferBundle(config))
    val bufferLock = Reg(Bool())
    opBuffer.init(SpecialOpBufferBundle(config).resetVal)
    bufferLock.init(False)

    when (io.input.valid && ~bufferLock) {
        opBuffer.op := io.input.payload.uop.lsuOp
        opBuffer.hint := io.input.payload.uop.lsuCoOp
        opBuffer.vaddr := io.input.payload.vaddr
        opBuffer.asid := io.input.payload.asid
    }
    when (io.input.valid && ~io.flush) {
        bufferLock := True
    }
    when (io.flush) {
        bufferLock := False
    }

    val iCacheStoreTag = Reg(Bool())
    val iCacheIndexInvalidate = Reg(Bool())
    val iCacheHitInvalidate = Reg(Bool())
    val dCacheStoreTag = Reg(Bool())
    val dCacheIndexInvalidate = Reg(Bool())
    val dCacheHitInvalidate = Reg(Bool())
    iCacheStoreTag.init(False)
    iCacheIndexInvalidate.init(False)
    iCacheHitInvalidate.init(False)
    dCacheStoreTag.init(False)
    dCacheIndexInvalidate.init(False)
    dCacheHitInvalidate.init(False)

    val tlbOp = Reg(TLBOp())
    val tlbInvGlobal = Reg(Bool())
    val tlbInvLocal = Reg(Bool())
    val tlbInvLocalVAMatch = Reg(Bool())
    val tlbInvLocalVANotMatch = Reg(Bool())
    tlbOp.init(TLBOp.nop)
    tlbInvGlobal.init(False)
    tlbInvLocal.init(False)
    tlbInvLocalVAMatch.init(False)
    tlbInvLocalVANotMatch.init(False)

    io.iCacheCtrl.stall := False // By default
    io.iCacheCtrl.cacopVA := opBuffer.vaddr
    io.iCacheCtrl.cacopStoreTag := iCacheStoreTag
    io.iCacheCtrl.cacopIndexInvalidate := iCacheIndexInvalidate
    io.iCacheCtrl.cacopHitInvalidate := iCacheHitInvalidate

    io.dCacheCtrl.stall := False // By default
    io.dCacheCtrl.cacopVA := opBuffer.vaddr
    io.dCacheCtrl.cacopStoreTag := dCacheStoreTag
    io.dCacheCtrl.cacopIndexInvalidate := dCacheIndexInvalidate
    io.dCacheCtrl.cacopHitInvalidate := dCacheHitInvalidate

    io.TLBCtrl.op := tlbOp
    io.TLBCtrl.invGlobal := tlbInvGlobal
    io.TLBCtrl.invLocal := tlbInvLocal
    io.TLBCtrl.invLocalVAMatch := tlbInvLocalVAMatch
    io.TLBCtrl.invLocalVANotMatch := tlbInvLocalVANotMatch
    io.TLBCtrl.invVA := opBuffer.vaddr(13, config.valen - 13 bits).asBits
    io.TLBCtrl.asid := opBuffer.asid

    val invCounter = Counter(0 to config.tlbSize-1)
    val replaceCounter = Counter(0 to config.tlbSize-1)
    replaceCounter.increment() // Increments every cycle
    io.TLBCtrl.index := Mux(tlbOp === TLBOp.fill, replaceCounter.value, invCounter.value)

    val tlbOpNext = TLBOp()
    switch (opBuffer.op) {
        is (LSUOp.tlbsrch) { tlbOpNext := TLBOp.srch }
        is (LSUOp.tlbrd  ) { tlbOpNext := TLBOp.read }
        is (LSUOp.tlbwr  ) { tlbOpNext := TLBOp.write }
        is (LSUOp.tlbfill) { tlbOpNext := TLBOp.fill }
        is (LSUOp.invtlb ) { tlbOpNext := TLBOp.inv }
        default            { tlbOpNext := TLBOp.nop }
    }

    val fsm = new StateMachine {
        val idle = new State with EntryPoint
        val waitToBegin = new State
        val waitToFinish = new State
        val cacop = new State
        val tlb = new State

        idle
            .whenIsActive {
                when (io.wake) {
                    goto(waitToBegin)
                }
            }
        waitToBegin
            .whenIsActive {
                io.iCacheCtrl.stall := True
                io.dCacheCtrl.stall := True
                when (~(io.iCacheCtrl.busy || io.dCacheCtrl.busy)) {
                    when (opBuffer.op === LSUOp.cacop) {
                        iCacheStoreTag := (opBuffer.hint(4 downto 3) === B(0).resized) && (opBuffer.hint(2 downto 0) === B(0).resized)
                        iCacheIndexInvalidate := (opBuffer.hint(4 downto 3) === B(1).resized) && (opBuffer.hint(2 downto 0) === B(0).resized)
                        iCacheHitInvalidate := (opBuffer.hint(4 downto 3) === B(2).resized) && (opBuffer.hint(2 downto 0) === B(0).resized)
                        dCacheStoreTag := (opBuffer.hint(4 downto 3) === B(0).resized) && ~(opBuffer.hint(2 downto 0) === B(0).resized)
                        dCacheIndexInvalidate := (opBuffer.hint(4 downto 3) === B(1).resized) && ~(opBuffer.hint(2 downto 0) === B(0).resized)
                        dCacheHitInvalidate := (opBuffer.hint(4 downto 3) === B(2).resized) && ~(opBuffer.hint(2 downto 0) === B(0).resized)

                        goto(cacop)
                    } elsewhen (opBuffer.op === LSUOp.tlbsrch || opBuffer.op === LSUOp.tlbrd || opBuffer.op === LSUOp.tlbwr || opBuffer.op === LSUOp.tlbfill || opBuffer.op === LSUOp.invtlb) {
                        tlbOp := tlbOpNext
                        tlbInvGlobal := opBuffer.hint === B(0).resized || opBuffer.hint === B(1).resized || opBuffer.hint === B(2).resized || opBuffer.hint === B(6).resized
                        tlbInvLocal := opBuffer.hint === B(0).resized || opBuffer.hint === B(1).resized || opBuffer.hint === B(3).resized
                        tlbInvLocalVAMatch := opBuffer.hint === B(4).resized || opBuffer.hint === B(5).resized || opBuffer.hint === B(6).resized
                        tlbInvLocalVANotMatch := opBuffer.hint === B(4).resized
                        invCounter.clear()
                        
                        goto(tlb)
                    } otherwise { // For BAR insts
                        goto(idle)
                    }
                }
            }
        cacop
            .whenIsActive {                
                io.iCacheCtrl.stall := True
                io.dCacheCtrl.stall := True

                iCacheStoreTag := False
                iCacheIndexInvalidate := False
                iCacheHitInvalidate := False
                dCacheStoreTag := False
                dCacheIndexInvalidate := False
                dCacheHitInvalidate := False

                goto(waitToFinish)
            }
        tlb
            .whenIsActive {
                io.iCacheCtrl.stall := True
                io.dCacheCtrl.stall := True
                
                when (tlbOp === TLBOp.inv) {
                    invCounter.increment()

                    when(invCounter.willOverflow) {
                        tlbOp := TLBOp.nop
                        tlbInvGlobal := False
                        tlbInvLocal := False
                        tlbInvLocalVAMatch := False
                        tlbInvLocalVANotMatch := False

                        goto (idle)
                    }
                } otherwise {
                    tlbOp := TLBOp.nop
                    tlbInvGlobal := False
                    tlbInvLocal := False
                    tlbInvLocalVAMatch := False
                    tlbInvLocalVANotMatch := False

                    goto (idle)
                }

            }
        waitToFinish
            .whenIsActive {
                io.iCacheCtrl.stall := True
                io.dCacheCtrl.stall := True
                when (~(io.iCacheCtrl.busy || io.dCacheCtrl.busy)) {
                    goto(idle)
                }
            }
    }
}

case class SpecialOpBufferBundle(config: CPUConfig) extends Bundle {
    val op = LSUOp()
    val hint = Bits(5 bits)
    val vaddr = UInt(config.valen bits)
    val asid = Bits(10 bits)

    def resetVal: SpecialOpBufferBundle = {
        val value = SpecialOpBufferBundle(config)
        value.op := LSUOp.preld // NOP
        value.hint := B(0).resized
        value.vaddr := U(0).resized
        value.asid := B(0).resized
        return value
    }
}