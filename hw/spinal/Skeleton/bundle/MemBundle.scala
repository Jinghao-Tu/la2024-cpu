package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class AXIBundle(hasWrite: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Cache
    // Slave: IO
    val arid    = Bits(config.axiIdWidth bits)
    val araddr  = Bits(config.axiAddressWidth bits)
    val arlen   = Bits(8 bits)
    val arsize  = Bits(3 bits)
    val arburst = Bits(2 bits)
    val arlock  = Bits(2 bits)
    val arcache = Bits(4 bits)
    val arprot  = Bits(3 bits)
    val arvalid = Bool()
    val arready = Bool()

    val rid    = Bits(config.axiIdWidth bits)
    val rdata  = Bits(config.axiDataWidth bits)
    val rresp  = Bits(2 bits)
    val rlast  = Bool()
    val rvalid = Bool()
    val rready = Bool()

    val awid    = hasWrite generate Bits(config.axiIdWidth bits)
    val awaddr  = hasWrite generate Bits(config.axiAddressWidth bits)
    val awlen   = hasWrite generate Bits(8 bits)
    val awsize  = hasWrite generate Bits(3 bits)
    val awburst = hasWrite generate Bits(2 bits)
    val awlock  = hasWrite generate Bits(2 bits)
    val awcache = hasWrite generate Bits(4 bits)
    val awprot  = hasWrite generate Bits(3 bits)
    val awvalid = hasWrite generate Bool()
    val awready = hasWrite generate Bool()

    val wid    = hasWrite generate Bits(config.axiIdWidth bits)
    val wdata  = hasWrite generate Bits(config.axiDataWidth bits)
    val wstrb  = hasWrite generate Bits(4 bits)
    val wlast  = hasWrite generate Bool()
    val wvalid = hasWrite generate Bool()
    val wready = hasWrite generate Bool()

    val bid    = hasWrite generate Bits(config.axiIdWidth bits)
    val bresp  = hasWrite generate Bits(2 bits)
    val bvalid = hasWrite generate Bool()
    val bready = hasWrite generate Bool()

    def asMaster(): Unit = {
        in(arready, rid, rdata, rresp, rlast, rvalid, awready, wready, bid, bresp, bvalid)
        out(arid, araddr, arlen, arsize, arburst, arlock, arcache, arprot, arvalid, rready, awid, awaddr, awlen, awsize, awburst, awlock, awcache, awprot, awvalid, wid, wdata, wstrb, wlast, wvalid, bready)
    }

    def arFire: Bool = { arvalid & arready }
    def  rFire: Bool = {  rvalid &  rready }
    def awFire: Bool = { awvalid & awready }
    def  wFire: Bool = {  wvalid &  wready }
    def  bFire: Bool = {  bvalid &  bready }
}

case class SpecialOpBufferUpdateBundle(config: CPUConfig) extends Bundle {
    val uop = uopBundle(FUType.lsu, config)
    val vaddr = UInt(config.valen bits)
    val asid = Bits(10 bits)
}

case class LLBitBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: LSU
    // Slave: LL Buffer
    val actualAddr = Bits(config.palen bits)
    val toUpdateAddr = Bits(config.palen bits)
    val wen = Bool()
    val llBit = CSRBundle(config).llbctl.rollb

    def asMaster(): Unit = {
        in(actualAddr, llBit)
        out(toUpdateAddr, wen)
    }
}

case class BADVBundle(fromLSU: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: IFU / LSU
    // Slave: BADV Buffer
    val robIdx = fromLSU generate Bits(config.robIdxWidth bits)
    val vaddr = Bits(config.valen bits)
    val wen = Bool()

    def asMaster(): Unit = {
        out(robIdx, vaddr, wen)
    }
}

case class CacheCtrlBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: SpecialOP Controller
    // Slave: Cache
    val busy = Bool()
    val stall = Bool()
    val cacopVA = UInt(config.valen bits) // Has different meanings in different methods
    val cacopStoreTag = Bool() // For direct index method
    val cacopIndexInvalidate = Bool() // For direct index method
    val cacopHitInvalidate = Bool() // For index query method

    def asMaster(): Unit = {
        in(busy)
        out(stall, cacopVA, cacopStoreTag, cacopIndexInvalidate, cacopHitInvalidate)
    }
}

case class ICacheReqBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // 0-latency Stream!
    // Master: PC
    // Slave: Cache
    val address = UInt(config.valen bits)
    val size = LSUSizeOp()
    val branchInfo = BranchInfo(config)
    
    def asMaster(): Unit = {
        out(address, size, branchInfo)
    }
}

case class TLBRespondBundle(config: CPUConfig) extends Bundle {
    val hit = Bool()
    val pageInfo = TLBPhyPageInfo(config)
}

case class TLBCSRWrite(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: TLB
    // Slave: CSR
    val tlbidx = CSRBundle(config).tlbidx
    val tlbehi = CSRBundle(config).tlbehi
    val tlbelo0 = CSRBundle(config).tlbelo
    val tlbelo1 = CSRBundle(config).tlbelo
    val asid = Bits(10 bits)
    val idxWen = Bool()
    val entryWen = Bool()

    def asMaster(): Unit = {
        out(tlbidx, tlbehi, tlbelo0, tlbelo1, asid, idxWen, entryWen)
    }
}

case class TLBCtrlBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: LSU
    // Slave: TLB
    val op = TLBOp()
    val invGlobal = Bool()
    val invLocal = Bool()
    val invLocalVAMatch = Bool()
    val invLocalVANotMatch = Bool()
    val index = UInt(config.tlbSizeWidth bits)
    val invVA = Bits(config.valen-13 bits)
    val asid = Bits(10 bits)

    def asMaster(): Unit = {
        out(op, invGlobal, invLocal, invLocalVAMatch, invLocalVANotMatch, index, invVA, asid)
    }
}

object TLBOp extends SpinalEnum {
    val nop, srch, read, write, fill, inv = newElement()
}

case class TLBRequestBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Cache
    // Slave: TLB
    val hit = Bool()
    val pageInfo = TLBPhyPageInfo(config)
    val virtPageNumber = Bits(config.valen-12 bits)

    override def asMaster(): Unit = {
        in(hit, pageInfo)
        out(virtPageNumber)
    }
}

case class TLBCSRInfo(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: TLB
    // Slave: CSR
    val asid = Bits(10 bits)
    val plv = CSRBundle(config).crmd.plv
    val da = CSRBundle(config).crmd.da
    val pg = CSRBundle(config).crmd.pg
    val datf = CSRBundle(config).crmd.datf
    val datm = CSRBundle(config).crmd.datm
    val dmw0 = CSRBundle(config).dmw
    val dmw1 = CSRBundle(config).dmw

    // For TLB insts
    val ecode = CSRBundle(config).estat.ecode
    val tlbidx = CSRBundle(config).tlbidx
    val tlbehi = CSRBundle(config).tlbehi
    val tlbelo0 = CSRBundle(config).tlbelo
    val tlbelo1 = CSRBundle(config).tlbelo
    
    override def asMaster(): Unit = {
        in(asid, plv, da, pg, datf, datm, dmw0, dmw1, ecode, tlbidx, tlbehi, tlbelo0, tlbelo1)
    }
}

case class TLBPhyPageInfo(config: CPUConfig) extends Bundle {
    val ppn = Bits(config.palen-12 bits)
    val plv = Bits(2 bits)
    val mat = Bits(2 bits)
    val d = Bool()
    val v = Bool()
    def resetVal: TLBPhyPageInfo = {
        val value = TLBPhyPageInfo(config)
        value.ppn := B(config.palen-12 bits, default -> False)
        value.plv := B(2 bits, default -> False)
        value.mat := B(2 bits, default -> False)
        value.d := False
        value.v := False
        return value
    }
}

case class TLBEntry(config: CPUConfig) extends Bundle {
    val vppn = Bits(config.valen-13 bits)
    val ps = Bits(6 bits)
    val g = Bool()
    val asid = Bits(10 bits)
    val e = Bool()
    val pp0 = TLBPhyPageInfo(config)
    val pp1 = TLBPhyPageInfo(config)
    def resetVal: TLBEntry = {
        val value = TLBEntry(config)
        value.vppn := B(config.valen-13 bits, default -> False)
        value.ps := B(6 bits, default -> False)
        value.g := False
        value.asid := B(10 bits, default -> False)
        value.e := False
        value.pp0 := TLBPhyPageInfo(config).resetVal
        value.pp1 := TLBPhyPageInfo(config).resetVal
        return value
    }
}