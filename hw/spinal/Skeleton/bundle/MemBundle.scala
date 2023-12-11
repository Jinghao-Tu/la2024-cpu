package Skeleton.bundle

import spinal.core._
import spinal.lib.IMasterSlave

import Skeleton.config._

case class TLBRequestBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: TLB
    // Slave: Cache
    val hit = Bool()
    val pageInfo = TLBPhyPageInfo(config)
    val virtPageNumber = Bits(config.valen-12 bits)

    override def asMaster(): Unit = {
        out(hit, pageInfo)
        in(virtPageNumber)
    }
}

case class TLBPhyPageInfo(config: CPUConfig) extends Bundle {
    val ppn = Bits(config.palen-12 bits)
    val plv = Bits(2 bits)
    val mat = Bits(2 bits)
    val d = Bool()
    val v = Bool()
}

case class TLBEntry(config: CPUConfig) extends Bundle {
    val vppn = Bits(config.valen-13 bits)
    val ps = Bits(6 bits)
    val g = Bool()
    val asid = Bits(10 bits)
    val e = Bool()
    val pp0 = TLBPhyPageInfo(config)
    val pp1 = TLBPhyPageInfo(config)
}