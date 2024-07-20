package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

/** 预测: 根据其他部件预测的下一指令块的首地址去预测接下来的指令, 直到填满或者结尾是一条预测会跳转的指令.
  * 更新: 记录跳转指令和跳转历史.
  * 组成: btb, bht
  */
// TODO: 考虑添加 ghr
case class FTB(config: CPUConfig) extends Component {
    val io = new Bundle {
        val nextBase = slave Flow (UInt(config.valen bits))
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits)))
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config)))
    }

    val btb = Mem(BTBBundle_1(config), wordCount = config.btbSize) init (Seq.fill(config.btbSize)(
      BTBBundle_1(config).resetVal
    ))
    val bht = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) init (Seq.fill(config.bhtSize)(
      U(0, config.bhtWidth bits)
    ))

    def hash_tag(pc: UInt): UInt = {
        var hash = U(0, 8 bits)
        for (i <- 0 until 4) {
            hash = hash ^ pc(i * 8 + 7 downto i * 8)
        }
        hash
    }

// ------------------------------- predict -------------------------------

    val npc = Vec.fill(config.fetchWidth)(UInt(config.valen bits))
    (0 until config.fetchWidth).map(i => {
        npc(i) := io.nextBase.payload + i |<< config.instLength / 8
    })

    val pred_jump = Vec.fill(config.fetchWidth)(Bool())
    val pred_hit = Vec.fill(config.fetchWidth)(Bool())

    (0 until config.fetchWidth).map(i => {
        val index = npc(i)(log2Up(config.btbSize) + 1 downto 2)
        val tag = hash_tag(npc(i))
        val btb_item = btb.readAsync(index)
        pred_hit(i) := btb_item.valid && btb_item.tag === tag
        val bht_item = bht.readAsync(index)
        pred_jump(i) := bht_item === 3 || bht_item === 2
    })

    val npcValid = Vec.fill(config.fetchWidth)(Bool())
    (0 until config.fetchWidth).map(i => {
        npcValid(i) := pred_hit(i) && pred_jump(i)
    })
    val lastValid = OHToUInt(OHMasking.first(npcValid.asBits))

    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := (U(i) <= lastValid)
        io.npc(i).payload := npc(i)
    })

// ------------------------------- update -------------------------------

    val updateMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        updateMask(i) := io.updateInfo(i).valid
    })

    (0 until config.retireWidth).map(i => {
        val index = io.updateInfo(i).payload.pc(log2Up(config.btbSize) + 1 downto 2)
        val tag = hash_tag(io.updateInfo(i).payload.pc)

        val isJumpInst = io.updateInfo(i).payload.isJumpInst || io.updateInfo(i).payload.isCallInst || io
            .updateInfo(i)
            .payload
            .isRetInst
        val taken = io.updateInfo(i).payload.taken

        when(updateMask(i)) {
            when(taken) {
                btb.write(index, BTBBundle_1(config).setVal(True, tag))
                bht.write(index, bht.readAsync(index) +| 1)
            }.otherwise {
                btb.write(index, BTBBundle_1(config).setVal(True, tag))
                bht.write(index, bht.readAsync(index) -| 1)
            }
        }.otherwise {
            btb.write(index, BTBBundle_1(config).resetVal)
            bht.write(index, U(0))
        }
    })

}
