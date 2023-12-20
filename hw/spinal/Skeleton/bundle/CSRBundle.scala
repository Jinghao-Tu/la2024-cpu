package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class CounterReadBundle(config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Operand read logic
    // Slave: CSR
    val id = UInt(config.wordLength bits)
    val value = UInt(config.counterWidth bits)

    def asMaster(): Unit = {
        in(id, value)
    }
}

case class CSRSwIOBundle(isWrite: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: Operand read logic / Retire logic
    // Slave: CSR
    val value = Bits(config.wordLength bits)
    val address = Bits(config.csrAddrLength bits)
    val wen = isWrite generate Bool()

    def asMaster(): Unit = {
        out(address, wen)
        if (isWrite) {
            out(value)
        } else {
            in(value)
        }
    }
}

case class CSRBundle(config: CPUConfig) extends Bundle {
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