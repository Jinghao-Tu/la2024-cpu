package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class SRAT(config: CPUConfig) extends Component {
    val io = new Bundle {
        // Note that multi update of the same architecture register will result in the last taking effect due to scala feature
        val writePort = Vec.fill(config.decodeWidth)(slave(RATIOBundle(true, config)))
        val updatePort = Vec.fill(config.writeBackWidth)(slave(RATIOBundle(true, config)))
        val srcReadPort = Vec.fill(config.decodeWidth)(Vec.fill(2)(slave(RATIOBundle(false, config))))
        val prevPRDReadPort = Vec.fill(config.decodeWidth)(slave(RATIOBundle(false, config)))
        val recovery = in(Bool())
        val recoveryPort = in(Vec.fill(config.arfSize)(Bits(config.prfIdxWidth bits)))
    }
    val rat = Vec.fill(config.arfSize)(Reg(SRATEntry(config)))
    rat.foreach(_ init(SRATEntry(config).resetVal()))
    when (io.recovery) {
        (0 until config.arfSize).map(i => {
            rat(i).prfIdx := io.recoveryPort(i)
            rat(i).valid := True
        })
    } otherwise {
        // Sequence of port assign here is important because write ports have higher priority over update ports
        io.updatePort.foreach(port => {
            when (port.wen && rat(port.ard.asUInt).prfIdx === port.prd) {
                rat(port.ard.asUInt).valid := True
            }
        })
        io.writePort.foreach(port => {
            when (port.wen) {
                rat(port.ard.asUInt).prfIdx := port.prd
                rat(port.ard.asUInt).valid := False
            }
        })
    }
    io.srcReadPort.foreach(portPair => {
        portPair.foreach(port => {
            port.prd := rat(port.ard.asUInt).prfIdx
            port.valid := rat(port.ard.asUInt).valid
        })
    })
    io.prevPRDReadPort.foreach(port => {
        port.prd := rat(port.ard.asUInt).prfIdx
        port.valid := rat(port.ard.asUInt).valid
    })
}

case class ARAT(config: CPUConfig) extends Component {
    val io = new Bundle {
        // Note that multi retire of the same architecture register will result in the last taking effect due to scala feature
        val retirePort = Vec.fill(config.retireWidth)(slave(RATIOBundle(true, config)))
        val recoveryPort = out(Vec.fill(config.arfSize)(Bits(config.prfIdxWidth bits)))
    }
    val rat = Vec.fill(config.arfSize)(Reg(Bits(config.prfIdxWidth bits)))
    rat.foreach(_ init(B"1'b0".resized))
    io.retirePort.foreach(port => {
        when (port.wen) {
            rat(port.ard.asUInt) := port.prd
        }
    })
    (0 until config.arfSize).map(i => {
        io.recoveryPort(i) := rat(i)
    })
}

case class FreeList() extends Component {

}