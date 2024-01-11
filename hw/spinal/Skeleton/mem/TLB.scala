package Skeleton.mem

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class TLB(config: CPUConfig) extends Component {
    val io = new Bundle {
        val iCacheReq = slave(TLBRequestBundle(config))
        val dCacheReq = slave(TLBRequestBundle(config))
        val csrInfo = master(TLBCSRInfo(config))
        val csrWrite = master(TLBCSRWrite(config))
        val ctrl = slave(TLBCtrlBundle(config))
    }
    val tlbStorage = Vec.fill(config.tlbSize)(Reg(TLBEntry(config)))
    tlbStorage.foreach(_ init(TLBEntry(config).resetVal))
    translate(io.iCacheReq, io.csrInfo.datf)
    translate(io.dCacheReq, io.csrInfo.datm)
    // Default values
    io.csrWrite.asid := B(0).resized
    io.csrWrite.tlbidx.index := B(0).resized
    io.csrWrite.tlbidx.rsv0 := B(0).resized
    io.csrWrite.tlbidx.ps := B(0).resized
    io.csrWrite.tlbidx.rsv1 := False
    io.csrWrite.tlbidx.ne := False
    io.csrWrite.tlbehi.rsv := B(0).resized
    io.csrWrite.tlbehi.vppn := B(0).resized
    io.csrWrite.tlbelo0.v := False
    io.csrWrite.tlbelo0.d := False
    io.csrWrite.tlbelo0.plv := B(0).resized
    io.csrWrite.tlbelo0.mat := B(0).resized
    io.csrWrite.tlbelo0.g := False
    io.csrWrite.tlbelo0.rsv0 := False
    io.csrWrite.tlbelo0.ppn := B(0).resized
    if (config.palen!=36) io.csrWrite.tlbelo0.rsv1 := B(0).resized
    io.csrWrite.tlbelo1.v := False
    io.csrWrite.tlbelo1.d := False
    io.csrWrite.tlbelo1.plv := B(0).resized
    io.csrWrite.tlbelo1.mat := B(0).resized
    io.csrWrite.tlbelo1.g := False
    io.csrWrite.tlbelo1.rsv0 := False
    io.csrWrite.tlbelo1.ppn := B(0).resized
    if (config.palen!=36) io.csrWrite.tlbelo1.rsv1 := B(0).resized
    io.csrWrite.idxWen := False
    io.csrWrite.entryWen := False

    val entryToFill = TLBEntry(config)
    entryToFill.vppn := io.csrInfo.tlbehi.vppn
    entryToFill.ps := io.csrInfo.tlbidx.ps
    entryToFill.g := io.csrInfo.tlbelo0.g & io.csrInfo.tlbelo1.g
    entryToFill.asid := io.csrInfo.asid
    entryToFill.e := (io.csrInfo.ecode === B(0x3F).resized) | ~io.csrInfo.tlbidx.ne
    entryToFill.pp0.ppn := io.csrInfo.tlbelo0.ppn
    entryToFill.pp0.plv := io.csrInfo.tlbelo0.plv
    entryToFill.pp0.mat := io.csrInfo.tlbelo0.mat
    entryToFill.pp0.d := io.csrInfo.tlbelo0.d
    entryToFill.pp0.v := io.csrInfo.tlbelo0.v
    entryToFill.pp1.ppn := io.csrInfo.tlbelo1.ppn
    entryToFill.pp1.plv := io.csrInfo.tlbelo1.plv
    entryToFill.pp1.mat := io.csrInfo.tlbelo1.mat
    entryToFill.pp1.d := io.csrInfo.tlbelo1.d
    entryToFill.pp1.v := io.csrInfo.tlbelo1.v

    val replaceCounter = Counter(0 to config.tlbSize-1)
    replaceCounter.increment() // Increments every cycle

    switch(io.ctrl.op) {
        is(TLBOp.srch) {
            val entryHitMap = Vec.fill(config.tlbSize)(Bool())
            val entryPageMask = Vec.fill(config.tlbSize)(Bits(config.valen-12 bits))
            val hit = entryHitMap.sContains(True)
            (0 until config.tlbSize).map(i => {
                entryHitMap(i) := reqHit(tlbStorage(i), entryPageMask(i), io.csrInfo.tlbehi.vppn)
                entryPageMask(i) := reqPageMask(tlbStorage(i))
            })
            io.csrWrite.tlbidx.index := Mux(hit, OHToUInt(entryHitMap).asBits, io.csrInfo.tlbidx.index)
            io.csrWrite.tlbidx.ne := ~hit
            io.csrWrite.tlbidx.ps := io.csrInfo.tlbidx.ps
            io.csrWrite.tlbidx.rsv0 := io.csrInfo.tlbidx.rsv0
            io.csrWrite.tlbidx.rsv1 := io.csrInfo.tlbidx.rsv1
            io.csrWrite.idxWen := True
        }
        is(TLBOp.read) {
            val valid = Bool()
            valid := tlbStorage(io.ctrl.index).e
            io.csrWrite.asid := Mux(valid, tlbStorage(io.ctrl.index).asid, B(0).resized)
            io.csrWrite.tlbehi.vppn := Mux(valid, tlbStorage(io.ctrl.index).vppn, B(0).resized)
            io.csrWrite.tlbehi.rsv := B(0).resized
            io.csrWrite.tlbelo1.v := Mux(valid, tlbStorage(io.ctrl.index).pp0.v, False)
            io.csrWrite.tlbelo0.d := Mux(valid, tlbStorage(io.ctrl.index).pp0.d, False)
            io.csrWrite.tlbelo0.plv := Mux(valid, tlbStorage(io.ctrl.index).pp0.plv, B(0).resized)
            io.csrWrite.tlbelo0.mat := Mux(valid, tlbStorage(io.ctrl.index).pp0.mat, B(0).resized)
            io.csrWrite.tlbelo0.g := Mux(valid, tlbStorage(io.ctrl.index).g, False)
            io.csrWrite.tlbelo0.rsv0 := False
            io.csrWrite.tlbelo0.ppn := Mux(valid, tlbStorage(io.ctrl.index).pp0.ppn, B(0).resized)
            if (config.palen!=36) io.csrWrite.tlbelo0.rsv1 := B(0).resized
            io.csrWrite.idxWen := True
            io.csrWrite.entryWen := True
        }
        is(TLBOp.write) {
            tlbStorage(io.ctrl.index) := entryToFill
        }
        is(TLBOp.fill) {
            // Randomly select one, not recommended to use
            tlbStorage(replaceCounter.value) := entryToFill
        }
        is(TLBOp.inv) {
            // Actually we don't need to check for valid entry since we only do a invalid operation here
            val globalMatch = tlbStorage(io.ctrl.index).g
            val pageMask = reqPageMask(tlbStorage(io.ctrl.index))
            val vaMatch = ((tlbStorage(io.ctrl.index).vppn ^ io.ctrl.invVA.resizeLeft(config.valen-13)) === (io.ctrl.invVA.resizeLeft(config.valen-13) ^ pageMask.resizeLeft(config.valen-13)))
            val localVAMatch = ~globalMatch & vaMatch
            val localVANotMatch = ~globalMatch & ~vaMatch
            when ((io.ctrl.invGlobal && globalMatch) || (io.ctrl.invLocalVAMatch && localVAMatch) || (io.ctrl.invLocalVANotMatch && localVANotMatch)) {
                tlbStorage(io.ctrl.index).e := False
            }
        }
        default { // NOP, do nothing

        }
    }

    def dmwPrivilegeCheck(dmwNo: Int): Bool = {
        require(dmwNo == 0 || dmwNo == 1)
        if (dmwNo == 0) {
            return (io.csrInfo.plv === B"2'b11" && io.csrInfo.dmw0.plv3) || (io.csrInfo.plv === B"2'b00" && io.csrInfo.dmw0.plv0)
        } else {
            return (io.csrInfo.plv === B"2'b11" && io.csrInfo.dmw1.plv3) || (io.csrInfo.plv === B"2'b00" && io.csrInfo.dmw1.plv0)
        }
    }
    def reqHit(entry: TLBEntry, pageMask: Bits, virtPageNumber: Bits): Bool = {
        return (entry.g || entry.asid === io.csrInfo.asid) && entry.e && (entry.vppn ^ pageMask.resizeLeft(config.valen-13)) === (virtPageNumber.resizeLeft(config.valen-13) ^ pageMask.resizeLeft(config.valen-13))
    }
    def reqLoBit(entry: TLBEntry, requestBundle: TLBRequestBundle): Bool = { // Ugly, needs optimization
        return (entry.ps === B"6'd12")? requestBundle.virtPageNumber(0) | requestBundle.virtPageNumber(9)
    }
    def reqPageMask(entry: TLBEntry): Bits = { // Ugly, needs optimization
        return (entry.ps === B"6'd12")? B"20'x0" | B"20'x1FF"
    }
    def TLBLookUp(requestBundle: TLBRequestBundle): Unit = {
        val entryHitMap = Vec.fill(config.tlbSize)(Bool())
        val entryPageMask = Vec.fill(config.tlbSize)(Bits(config.valen-12 bits))
        val entryLoBit = Vec.fill(config.tlbSize)(Bool())
        (0 until config.tlbSize).map(i => {
            entryHitMap(i) := reqHit(tlbStorage(i), entryPageMask(i), requestBundle.virtPageNumber)
            entryPageMask(i) := reqPageMask(tlbStorage(i))
            entryLoBit(i) := reqLoBit(tlbStorage(i), requestBundle)
        })
        val hitEntry = MuxOH(entryHitMap, tlbStorage)
        val pageMask = MuxOH(entryHitMap, entryPageMask)
        val loBit = MuxOH(entryHitMap, entryLoBit)
        val hitPageInfo = Mux(loBit, hitEntry.pp1, hitEntry.pp0)
        requestBundle.pageInfo.ppn := (hitPageInfo.ppn & ~pageMask.resized) | (requestBundle.virtPageNumber.resized & pageMask.resized)
        requestBundle.pageInfo.plv := hitPageInfo.plv
        requestBundle.pageInfo.mat := hitPageInfo.mat
        requestBundle.pageInfo.d := hitPageInfo.d
        requestBundle.pageInfo.v := hitPageInfo.v
        requestBundle.hit := entryHitMap.sContains(True)
    }
    def translate(requestBundle: TLBRequestBundle, datMode: Bits): Unit = {
        when (io.csrInfo.pg && !io.csrInfo.da) { // Mapped translate mode
            when ((requestBundle.virtPageNumber(config.valen-13 downto config.valen-15) === io.csrInfo.dmw0.vseg && dmwPrivilegeCheck(0))) { // Meet direct map window 0
                requestBundle.pageInfo.ppn := io.csrInfo.dmw0.pseg ## requestBundle.virtPageNumber(config.valen-16 downto 0)
                requestBundle.pageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
                requestBundle.pageInfo.mat := io.csrInfo.dmw0.mat
                requestBundle.pageInfo.d := True // Just to ensure that no dirty check fault will be thrown
                requestBundle.pageInfo.v := True // Just to ensure that no page fault will be thrown
                requestBundle.hit := True // Just to ensure that no TLB miss exception will be thrown
            } elsewhen (requestBundle.virtPageNumber(config.valen-13 downto config.valen-15) === io.csrInfo.dmw1.vseg && dmwPrivilegeCheck(1)) { // Meet direct map window 1
                requestBundle.pageInfo.ppn := io.csrInfo.dmw1.pseg ## requestBundle.virtPageNumber(config.valen-16 downto 0)
                requestBundle.pageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
                requestBundle.pageInfo.mat := io.csrInfo.dmw1.mat
                requestBundle.pageInfo.d := True // Just to ensure that no dirty check fault will be thrown
                requestBundle.pageInfo.v := True // Just to ensure that no page fault will be thrown
                requestBundle.hit := True // Just to ensure that no TLB miss exception will be thrown
            } otherwise { // Nothing met, looking up real TLB
                TLBLookUp(requestBundle)
            }
        } elsewhen (!io.csrInfo.pg && io.csrInfo.da) { // Direct translate mode
            requestBundle.pageInfo.ppn := requestBundle.virtPageNumber.resized
            requestBundle.pageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
            requestBundle.pageInfo.mat := datMode
            requestBundle.pageInfo.d := True // Just to ensure that no dirty check fault will be thrown
            requestBundle.pageInfo.v := True // Just to ensure that no page fault will be thrown
            requestBundle.hit := True // Just to ensure that no TLB miss exception will be thrown
        } otherwise { // Whoever know what this is? Just copy direct mode codes
            requestBundle.pageInfo.ppn := requestBundle.virtPageNumber.resized
            requestBundle.pageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
            requestBundle.pageInfo.mat := datMode
            requestBundle.pageInfo.d := True // Just to ensure that no dirty check fault will be thrown
            requestBundle.pageInfo.v := True // Just to ensure that no page fault will be thrown
            requestBundle.hit := True // Just to ensure that no TLB miss exception will be thrown
        }
    }
}