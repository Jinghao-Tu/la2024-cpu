package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

/** 预测: 根据其他部件预测的下一指令块的首地址去预测接下来的指令, 直到填满或者结尾是一条跳转指令 (或者预测会跳转的指令).
  * 更新: 记录跳转指令和跳转历史.
  * 组成: btb
  */

case class FTB(config: CPUConfig) extends Component {
    val io = new Bundle {
        val nextBase = slave Flow (UInt(config.valen bits))
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits)))
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config)))

        val GHR = in UInt(config.ghrWidth bits)
    }

    val btb = Array.fill(config.fetchWidth)(Mem(BTBBundle_1(config), wordCount = config.btbSize))
    val validList = Vec.fill(config.btbSize)(RegInit(False))

    def hash_tag(pc: UInt): UInt = {
        (0 until 4).map(i => {
            pc(((i + 1) * config.valen/4 - 1) downto (i * config.valen/4))
        }).reduce(_ ^ _)
    }

// ------------------------------- predict -------------------------------

    val npc = Vec.fill(config.fetchWidth)(UInt(config.valen bits))
    (0 until config.fetchWidth).map(i => {
        npc(i) := io.nextBase.payload + i * (config.instLength / 8)
    })

    // val pred_jump = Bits(config.fetchWidth bits)
    val pred_hit = Bits(config.fetchWidth bits)
    val btb_index = Vec.fill(config.fetchWidth)(UInt(log2Up(config.btbSize) bits))
    val tag = Vec.fill(config.fetchWidth)(UInt(config.valen/4 bits))
    val valid = Bits(config.fetchWidth bits)
    val btb_item = Vec.fill(config.fetchWidth)(BTBBundle_1(config))

    (0 until config.fetchWidth).map(i => {
        btb_index(i) := npc(i)(log2Up(config.btbSize) + 1 downto 2) 
        tag(i) := hash_tag(npc(i))
        valid(i) := io.nextBase.valid & validList(btb_index(i))
        // btb_item(i) := btb(i).readSync(btb_index(i), valid(i))
        btb_item(i) := btb(i).readAsync(btb_index(i)) // 必须异步读, 延迟很大
        pred_hit(i) := valid(i) & (btb_item(i).tag === tag(i)) // avoid X.
    })

    val npcValid = pred_hit // 保守做法, 到预测是跳转指令为止.
    val hit = pred_hit.orR
    val lastValid = OHToUInt(OHMasking.first(npcValid.asBits))

    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := io.nextBase.valid & (!hit | (U(i) <= lastValid))
        io.npc(i).payload := npc(i)
    })

// ------------------------------- update -------------------------------
    val updateMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        updateMask(i) := io.updateInfo(i).valid
    })

    (0 until config.retireWidth).map(i => {
        // 2-cycle: read, write

        // stage 1: read
        val updatePC = io.updateInfo(i).payload.pc
        val updateIsJumpInst = io.updateInfo(i).payload.isJumpInst
        val updateTaken = io.updateInfo(i).payload.taken
        val updateTarget = io.updateInfo(i).payload.targetPC
        val updateGHR = io.updateInfo(i).payload.GHR
        val updatePredictFail = io.updateInfo(i).payload.predictFail

        val updBtbIdx = updatePC(log2Up(config.btbSize)+1 downto 2)
        val updTag = hash_tag(updatePC)
        val updValid = validList(updBtbIdx) && updateMask(i)
        
        // data from 1 to 2
        val updateMaskReg = RegNext(updateMask)
        val updateIsJumpInstReg = RegNext(updateIsJumpInst)
        val updateTakenReg = RegNext(updateTaken)
        val updateTargetReg = RegNext(updateTarget)
        val updatePredictFailReg = RegNext(updatePredictFail)
        val updBtbIdxReg = RegNext(updBtbIdx)
        val updTagReg = RegNext(updTag)
        val updValidReg = RegNext(updValid)
        
        // stage 2: write
        val wdataBTB = BTBBundle_1(config).resetVal
        when(updValidReg) {
            when(updateIsJumpInstReg) {
                // update btb
                wdataBTB := BTBBundle_1(config).setVal(updTagReg)
            } .otherwise {
                // update valid
                validList(updBtbIdxReg) := False
            }
        } .otherwise {
            when (updateTakenReg) {
                // update btb
                wdataBTB := BTBBundle_1(config).setVal(updTagReg)
                // update valid
                validList(updBtbIdxReg) := True
            }
        }
        (0 until config.fetchWidth).map(i => {
            btb(i).write(updBtbIdxReg, wdataBTB, updateMaskReg(i) && (updValidReg && updateIsJumpInstReg || !updValidReg && updateTakenReg))
        })
    })

}
