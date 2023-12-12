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
    }
    val tlbStorage = Vec.fill(config.tlbsize)(Reg(TLBEntry(config)))
    tlbStorage.foreach(_ init(TLBEntry(config).resetVal()))
    translate(io.iCacheReq)
    translate(io.dCacheReq)
    tlbStorage.foreach(entry => {entry := TLBEntry(config).resetVal()}) // Temporarily used for passing elaboration

    def dmwPrivilegeCheck(dmwNo: Int): Bool = {
        require(dmwNo == 0 || dmwNo == 1)
        if (dmwNo == 0) {
            return (io.csrInfo.plv === B"2'b11" && io.csrInfo.dmw0.plv3) || (io.csrInfo.plv === B"2'b00" && io.csrInfo.dmw0.plv0)
        } else {
            return (io.csrInfo.plv === B"2'b11" && io.csrInfo.dmw1.plv3) || (io.csrInfo.plv === B"2'b00" && io.csrInfo.dmw1.plv0)
        }
    }
    def reqHit(entry: TLBEntry, pageMask: Bits, requestBundle: TLBRequestBundle): Bool = {
        return (entry.g || entry.asid === io.csrInfo.asid) && entry.e && (entry.vppn ^ pageMask.resizeLeft(config.valen-13)) === (requestBundle.virtPageNumber(config.valen-13 downto 1) ^ pageMask.resizeLeft(config.valen-13))
    }
    def reqLoBit(entry: TLBEntry, requestBundle: TLBRequestBundle): Bool = { // Ugly, needs optimization
        return (entry.ps === B"6'd12")? requestBundle.virtPageNumber(0) | requestBundle.virtPageNumber(9)
    }
    def reqPageMask(entry: TLBEntry): Bits = { // Ugly, needs optimization
        return (entry.ps === B"6'd12")? B"20'x0" | B"20'x1FF"
    }
    def translate(requestBundle: TLBRequestBundle): Unit = {
        val phyPageInfo = RegInit(TLBPhyPageInfo(config).resetVal())
        val translateHit = RegInit(False)
        requestBundle.pageInfo := phyPageInfo
        requestBundle.hit := translateHit
        when (io.csrInfo.pg && !io.csrInfo.da) { // Mapped translate mode
            when ((requestBundle.virtPageNumber(config.valen-13 downto config.valen-15) === io.csrInfo.dmw0.vseg && dmwPrivilegeCheck(0))) { // Meet direct map window 0
                phyPageInfo.ppn := io.csrInfo.dmw0.pseg ## requestBundle.virtPageNumber(config.valen-16 downto 0)
                phyPageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
                phyPageInfo.mat := io.csrInfo.dmw0.mat
                phyPageInfo.d := True // Just to ensure that no dirty check fault will be thrown
                phyPageInfo.v := True // Just to ensure that no page fault will be thrown
                translateHit := True // Just to ensure that no TLB miss exception will be thrown
            } elsewhen (requestBundle.virtPageNumber(config.valen-13 downto config.valen-15) === io.csrInfo.dmw1.vseg && dmwPrivilegeCheck(1)) { // Meet direct map window 1
                phyPageInfo.ppn := io.csrInfo.dmw1.pseg ## requestBundle.virtPageNumber(config.valen-16 downto 0)
                phyPageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
                phyPageInfo.mat := io.csrInfo.dmw1.mat
                phyPageInfo.d := True // Just to ensure that no dirty check fault will be thrown
                phyPageInfo.v := True // Just to ensure that no page fault will be thrown
                translateHit := True // Just to ensure that no TLB miss exception will be thrown
            } otherwise { // Nothing met, looking up real TLB
                val entryHitMap = Vec.fill(config.tlbsize)(Bool())
                val entryPageMask = Vec.fill(config.tlbsize)(Bits(config.valen-12 bits))
                val entryLoBit = Vec.fill(config.tlbsize)(Bool())
                (0 until config.tlbsize).map(i => {
                    entryHitMap(i) := reqHit(tlbStorage(i), entryPageMask(i), requestBundle)
                    entryPageMask(i) := reqPageMask(tlbStorage(i))
                    entryLoBit(i) := reqLoBit(tlbStorage(i), requestBundle)
                })
                val hitEntry = MuxOH(entryHitMap, tlbStorage)
                val pageMask = MuxOH(entryHitMap, entryPageMask)
                val loBit = MuxOH(entryHitMap, entryLoBit)
                val hitPageInfo = Mux(loBit, hitEntry.pp1, hitEntry.pp0)
                phyPageInfo.ppn := (hitPageInfo.ppn & ~pageMask.resized) | (requestBundle.virtPageNumber.resized & pageMask.resized)
                translateHit := entryHitMap.sContains(True)
            }
        } elsewhen (!io.csrInfo.pg && io.csrInfo.da) { // Direct translate mode
            phyPageInfo.ppn := requestBundle.virtPageNumber.resized
            phyPageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
            phyPageInfo.mat := io.csrInfo.datf
            phyPageInfo.d := True // Just to ensure that no dirty check fault will be thrown
            phyPageInfo.v := True // Just to ensure that no page fault will be thrown
            translateHit := True // Just to ensure that no TLB miss exception will be thrown
        } otherwise { // Whoever knows what this is?
            // This should never happen, but if OS insists just make it happen
            // Behaves the same as direct translate mode
            phyPageInfo.ppn := requestBundle.virtPageNumber.resized
            phyPageInfo.plv := io.csrInfo.plv // Just to ensure that no privilege check fault will be thrown
            phyPageInfo.mat := io.csrInfo.datf
            phyPageInfo.d := True // Just to ensure that no dirty check fault will be thrown
            phyPageInfo.v := True // Just to ensure that no page fault will be thrown
            translateHit := True // Just to ensure that no TLB miss exception will be thrown
        }
    }
}