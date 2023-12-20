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
    val prmd = new Bundle {
        val pplv = Bits(2 bits)
        val pie = Bool()
        val rsv = Bits(29 bits)
    }
    val euen = new Bundle {
        val fpe = Bool()
        val rsv = Bits(31 bits)
    }
    val ecfg = new Bundle {
        val lieLo = Bits(10 bits)
        val rsv0 = Bool()
        val lieHi = Bits(2 bits)
        val rsv1 = Bits(19 bits)
    }
    val estat = new Bundle {
        val isSw = Bits(2 bits)
        val isHw = Bits(8 bits)
        val rsv0 = Bool()
        val isTI = Bool()
        val isIPI = Bool()
        val rsv1 = Bits(3 bits)
        val ecode = Bits(6 bits)
        val esubcode = Bits(9 bits)
        val rsv2 = Bool()
    }
    val era = new Bundle {
        val pc = Bits(32 bits)
    }
    val badv = new Bundle {
        val vaddr = Bits(32 bits)
    }
    val eentry = new Bundle {
        val rsv = Bits(6 bits)
        val va = Bits(26 bits)
    }
    val tlbidx = new Bundle {
        val index = Bits(config.tlbSizeWidth bits)
        val rsv0 = Bits(24-config.tlbSizeWidth bits)
        val ps = Bits(6 bits)
        val rsv1 = Bool()
        val ne = Bool()
    }
    val tlbehi = new Bundle {
        val rsv = Bits(13 bits)
        val vppn = Bits(19 bits)
    }
    val tlbelo = new Bundle {
        val v = Bool()
        val d = Bool()
        val plv = Bits(2 bits)
        val mat = Bits(2 bits)
        val g = Bool()
        val rsv0 = Bool()
        val ppn = Bits(config.palen-12 bits)
        val rsv1 = (config.palen!=36) generate(Bits(36-config.palen bits))
    }
    val asid = new Bundle {
        val asid = Bits(10 bits)
        val rsv0 = Bits(6 bits)
        val asidbits = Bits(8 bits)
        val rsv1 = Bits(8 bits)
    }
    val pgd = new Bundle {
        val rsv = Bits(12 bits)
        val base = Bits(20 bits)
    }
    val cpuid = new Bundle {
        val coreid = Bits(9 bits)
        val rsv = Bits(23 bits)
    }
    val save = new Bundle {
        val data = Bits(32 bits)
    }
    val tid = new Bundle {
        val tid = Bits(32 bits)
    }
    val tcfg = new Bundle {
        val en = Bool()
        val periodic = Bool()
        val initval = Bits(config.timerWidth-2 bits)
        val rsv = (config.timerWidth!=32) generate(Bits(32-config.timerWidth bits))
    }
    val tval = new Bundle {
        val timeval = Bits(config.timerWidth bits)
        val rsv = (config.timerWidth!=32) generate(Bits(32-config.timerWidth bits))
    }
    val ticlr = new Bundle {
        val clr = Bool()
        val rsv = Bits(31 bits)
    }
    val llbctl = new Bundle {
        val rollb = Bool()
        val wcllb = Bool()
        val klo = Bool()
        val rsv = Bits(29 bits)
    }
    val tlbrentry = new Bundle {
        val rsv = Bits(6 bits)
        val pa = Bits(26 bits)
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