package Skeleton.bundle

import spinal.core._
import spinal.lib._

import Skeleton.config._

case class PRFIOBundle(isWrite: Boolean, config: CPUConfig) extends Bundle with IMasterSlave {
    // Master: FUs
    // Slave: PRF
    val idx = Bits(config.prfIdxWidth bits)
    val data = UInt(config.wordLength bits)
    val wen = isWrite generate (Bool())

    def asMaster(): Unit = {
        if (isWrite) {
            out(data)
        } else {
            in(data)
        }
        out(idx, wen)
    }
}