package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class LSDebugBundle(config: CPUConfig) extends Bundle {
    val wb_pc = Bits(config.valen bits)
    val wb_rf_wen = Bits(4 bits)
    val wb_rf_wnum = Bits(5 bits)
    val wb_rf_wdata = Bits(config.wordLength bits)

    def defaultVal: LSDebugBundle = {
        val value = LSDebugBundle(config)
        value.wb_pc := B(0).resized
        value.wb_rf_wen := B(0).resized
        value.wb_rf_wnum := B(0).resized
        value.wb_rf_wdata := B(0).resized
        return value
    }
}