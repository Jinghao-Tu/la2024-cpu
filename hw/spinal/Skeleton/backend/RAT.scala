package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class SRAT(config: CPUConfig) extends Component {
    val io = new Bundle {
        // Note that multi update of the same architecture register will result in the last taking effect due to scala feature
        val writePort = Vec.fill(config.decodeWidth)(slave(RATIOBundle(true, false, config)))
        val updatePort = Vec.fill(config.writebackWidth)(slave(RATIOBundle(true, true, config)))
        val srcReadPort = Vec.fill(config.decodeWidth)(Vec.fill(2)(slave(RATIOBundle(false, false, config))))
        val prevPRDReadPort = Vec.fill(config.decodeWidth)(slave(RATIOBundle(false, false, config)))
        val delayedRecovery = in(Bool())
        val recoveryPort = in(Vec.fill(config.arfSize)(Bits(config.prfIdxWidth bits)))
    }
    val rat = Vec.fill(config.arfSize)(Reg(SRATEntry(config)))
    (0 until config.arfSize).map(i => {
        rat(i).init(SRATEntry(config).resetVal(i))
    })
    when (io.delayedRecovery) {
        (0 until config.arfSize).map(i => {
            rat(i).prfIdx := io.recoveryPort(i)
            rat(i).valid := True
        })
    } otherwise {
        // Sequence of port assign here is important because write ports have higher priority over update ports
        rat.foreach(entry => {
            io.updatePort.foreach(port => {
                when (port.wen && entry.prfIdx === port.prd) {
                    entry.valid := True
                }
            })
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
        val retirePort = Vec.fill(config.retireWidth)(slave(RATIOBundle(true, false, config)))
        val recoveryPort = out(Vec.fill(config.arfSize)(Bits(config.prfIdxWidth bits)))
    }
    val rat = Vec.fill(config.arfSize)(Reg(Bits(config.prfIdxWidth bits)))
    io.retirePort.foreach(port => {
        when (port.wen) {
            rat(port.ard.asUInt) := port.prd
        }
    })
    (0 until config.arfSize).map(i => {
        rat(i).init(B(i)).resized
        io.recoveryPort(i) := rat(i)
    })
}

case class FreeList(config: CPUConfig) extends Component {
    val io = new Bundle {
        val dispatch = slave(FreeListDispatchIOBundle(config))
        val retire = slave(FreeListRetireIOBundle(config))
    }
    def freeListPtrWidth = log2Up(config.freeListSize)
    val freeList = Vec.fill(config.freeListSize)(Reg(Bits(config.prfIdxWidth bits)))
    (0 until config.freeListSize).map(i => { freeList(i).init(B(i)) })
    val freePtr = Vec.fill(config.retireWidth)(Reg(UInt(freeListPtrWidth bits)))
    (0 until config.retireWidth).map(i => { freePtr(i).init(U(i)) })
    val allocPtr = Vec.fill(config.decodeWidth)(Reg(UInt(freeListPtrWidth bits)))
    (0 until config.decodeWidth).map(i => { allocPtr(i).init(U(i+config.arfSize)) }) // PRF #0 is reserved for architecture register r0!
    val retirePtr = RegInit(U(config.arfSize).resize(freeListPtrWidth))

    val availMask = Bits(config.decodeWidth bits)
    (0 until config.decodeWidth).map(i => {
        availMask(i) := allocPtr(i) =/= freePtr(0)
        io.dispatch.availMask(i) := availMask(i downto 0).andR
    })
    val dispatchNum = io.dispatch.disPatchNum
    (0 until config.decodeWidth).map(i => {
        io.dispatch.prfIdx(i) := freeList(allocPtr(i))
        allocPtr(i) := allocPtr(i) + dispatchNum
    })
    (0 until config.retireWidth).map(i => {
        when (io.retire.validMask(i)) {
            freeList(freePtr(i)) := io.retire.prfIdx(i)
        }
        freePtr(i) := freePtr(i) + CountOne(io.retire.validMask)
    })
    retirePtr := retirePtr + CountOne(io.retire.validMask)
    when (io.retire.delayedFlush) { // Note that flush has priority over dispatch pointer movement, do not swap code here
        (0 until config.decodeWidth).map(i => {
            allocPtr(i) := retirePtr + U(i)
        })
    }
}