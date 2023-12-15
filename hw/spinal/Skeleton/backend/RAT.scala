package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class SRAT(config: CPUConfig) extends Component {
    val io = new Bundle {
        // Note that multi update of the same architecture register will result in the last taking effect due to scala feature
        val writePort = Vec.fill(config.decodeWidth)(slave(RATIOBundle(true, false, config)))
        val updatePort = Vec.fill(config.writeBackWidth)(slave(RATIOBundle(true, true, config)))
        val srcReadPort = Vec.fill(config.decodeWidth)(Vec.fill(2)(slave(RATIOBundle(false, false, config))))
        val prevPRDReadPort = Vec.fill(config.decodeWidth)(slave(RATIOBundle(false, false, config)))
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
    (0 until config.decodeWidth).map(i => { allocPtr(i).init(U(i+1)) }) // PRF #0 is reserved for architecture register r0!
    val retirePtr = RegInit(U"1'b1".resize(freeListPtrWidth))

    val availMask = Bits(config.decodeWidth bits)
    (0 until config.decodeWidth).map(i => {
        availMask(i) := allocPtr(i) =/= freePtr(0)
        io.dispatch.availMask(i) := availMask(i downto 0).andR
    })
    val dispatchNum = CountOne(io.dispatch.allowMask)
    (0 until config.decodeWidth).map(i => {
        io.dispatch.prfIdx(i) := freeList(allocPtr(i))
        allocPtr(i) := allocPtr(i) + dispatchNum
    })
    val retireEnableMask = io.retire.writeNum.muxListDc(
        for (i <- 0 until config.retireWidth+1) 
            yield (i, B((1<<i)-1).resize(config.retireWidth))
    )
    (0 until config.retireWidth).map(i => {
        when (retireEnableMask(i)) {
            freeList(freePtr(i)) := io.retire.prfIdx(i)
        }
        freePtr(i) := freePtr(i) + io.retire.writeNum
    })
    retirePtr := retirePtr + io.retire.writeNum
    when (io.retire.flush) { // Note that flush has priority over dispatch pointer movement, do not swap code here
        (0 until config.decodeWidth).map(i => {
            allocPtr(i) := retirePtr + U(i)
        })
    }
}