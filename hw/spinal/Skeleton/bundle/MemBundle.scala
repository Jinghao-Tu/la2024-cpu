package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class TLBRequestBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Cache
    // Slave: TLB
    val hit = Bool()
    val pageInfo = TLBPhyPageInfo(config)
    val virtPageNumber = Bits(config.valen-12 bits)
    val valid = Bool()

    override def asMaster(): Unit = {
        in(hit, pageInfo)
        out(virtPageNumber, valid)
    }
}

case class TLBCSRInfo(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: TLB
    // Slave: CSR
    val asid = Bits(10 bits)
    val plv = CSRBundle().crmd.plv
    val da = CSRBundle().crmd.da
    val pg = CSRBundle().crmd.pg
    val datf = CSRBundle().crmd.datf
    val datm = CSRBundle().crmd.datm
    val dmw0 = CSRBundle().dmw
    val dmw1 = CSRBundle().dmw
    
    override def asMaster(): Unit = {
        in(asid, plv, da, pg, datf, datm, dmw0, dmw1)
    }
}

case class TLBPhyPageInfo(config: CPUConfig) extends Bundle {
    val ppn = Bits(config.palen-12 bits)
    val plv = Bits(2 bits)
    val mat = Bits(2 bits)
    val d = Bool()
    val v = Bool()
    def resetVal(): TLBPhyPageInfo = {
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
    def resetVal(): TLBEntry = {
        val value = TLBEntry(config)
        value.vppn := B(config.valen-13 bits, default -> False)
        value.ps := B(6 bits, default -> False)
        value.g := False
        value.asid := B(10 bits, default -> False)
        value.e := False
        value.pp0 := TLBPhyPageInfo(config).resetVal()
        value.pp1 := TLBPhyPageInfo(config).resetVal()
        return value
    }
}