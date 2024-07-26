package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class BTBBundle (config: CPUConfig) extends Bundle {
    val valid = Bool()
    val tag = UInt(config.btbTagWidth bits)
    val target = UInt(config.valen bits)

    def resetVal: BTBBundle = {
        val value = BTBBundle(config)

        value.valid := False
        value.tag := 0
        value.target := 0

        value
    }

    def setVal(valid: Bool, tag: UInt, target: UInt): BTBBundle = {
        val value = BTBBundle(config)

        value.valid := valid
        value.tag := tag
        value.target := target

        value
    }
}

case class PHTBundle (config: CPUConfig) extends Bundle {
    val counter = UInt(config.phtCounterWidth bits)
    val tag = UInt(config.phtTagWidth bits)
    val useful = UInt(config.phtUsefulWidth bits)
    
    def resetVal: PHTBundle = {
        val value = PHTBundle(config)

        value.counter := 0
        value.tag := 0
        value.useful := 0

        value
    }

    def setVal(counter: UInt, tag: UInt, useful: UInt): PHTBundle = {
        val value = PHTBundle(config)

        value.counter := counter
        value.tag := tag
        value.useful := useful

        value
    }
}

case class RasTBundle (config: CPUConfig) extends Bundle {
    val valid = Bool()
    val tag = UInt(config.rasTagWidth bits)
    
    def resetVal: RasTBundle = {
        val value = RasTBundle(config)

        value.valid := False
        value.tag := 0

        value
    }

    def setVal(valid: Bool, tag: UInt): RasTBundle = {
        val value = RasTBundle(config)

        value.valid := valid
        value.tag := tag

        value
    }
}

case class BTBBundle_1(config: CPUConfig) extends Bundle {
    val valid = Bool()
    val tag = UInt(config.btbTagWidth bits)

    def resetVal: BTBBundle_1 = {
        val value = BTBBundle_1(config)

        value.valid := False
        value.tag := 0

        value
    }

    def setVal(valid: Bool, tag: UInt): BTBBundle_1 = {
        val value = BTBBundle_1(config)

        value.valid := valid
        value.tag := tag

        value
    }
}

case class RasStackBundle(config: CPUConfig) extends  Bundle {
    val target = UInt(config.valen bits)
    val counter = UInt(config.rasStackCounterWidth bits)
    
    val setVal = (target: UInt, counter: UInt) => {
        val value = RasStackBundle(config)

        value.target := target
        value.counter := counter

        value
    }
}
