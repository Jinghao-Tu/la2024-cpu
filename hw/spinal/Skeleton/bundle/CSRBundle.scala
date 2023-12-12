package Skeleton.bundle

import spinal.core._
import spinal.lib._

case class CSRBundle() extends Bundle {
    val crmd = new Bundle {
        val plv = Bits(2 bits)
        val ie = Bool()
        val da = Bool()
        val pg = Bool()
        val datf = Bits(2 bits)
        val datm = Bits(2 bits)
        val rsv = Bits(23 bits)
    }
    val dmw = new Bundle {
        val plv0 = Bool()
        val rsv0 = Bits(2 bits)
        val plv3 = Bool()
        val mat = Bits(2 bits)
        val rsv1 = Bits(19 bits)
        val pseg = Bits(3 bits)
        val rsv2 = Bool()
        val vseg = Bits(3 bits)
    }
}