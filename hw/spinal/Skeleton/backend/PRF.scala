package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class PRF(config: CPUConfig) extends Component {
    val io = new Bundle {
        val read = Vec.fill(config.readPairNum)(Vec.fill(2)(slave(PRFIOBundle(false, config))))
        val write = Vec.fill(config.writebackWidth)(slave(PRFIOBundle(true, config)))
        val debugRegs = out(Vec.fill(config.prfSize)(UInt(config.wordLength bits)))
    }
    // No reset function implemented
    val regFile = Vec.fill(config.prfSize)(Reg(UInt(config.wordLength bits)))
    regFile.foreach(_ init(U(0)))
    io.read.foreach(portPair => {
        portPair.foreach(port => {
            when (port.idx =/= B"1'b0".resized) {
                port.data := regFile(port.idx.asUInt)
            } otherwise {
                port.data := U"1'b0".resized
            }
        })
    })
    io.write.foreach(port => {
        when (port.idx =/= B"1'b0".resized) {
            regFile(port.idx.asUInt) := port.data
        }
    })

    io.debugRegs := Vec(regFile.map(_.resized))
}