package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

/** 预测: 根据其他部件预测的下一指令块的首地址去预测接下来的指令, 直到填满或者结尾是一条跳转指令.
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

    val tagList = Vec.fill(config.ftbSize)(RegInit(U(0, config.valen/4 bits)))
    val validList = Vec.fill(config.ftbSize)(RegInit(False))

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

    val hit = Bits(config.fetchWidth bits)
    val idx = Vec.fill(config.fetchWidth)(UInt(log2Up(config.ftbSize) bits))
    val tag = Vec.fill(config.fetchWidth)(UInt(config.valen/4 bits))
    val valid = Bits(config.fetchWidth bits)

    (0 until config.fetchWidth).map(i => {
        idx(i) := npc(i)(log2Up(config.ftbSize)+1 downto 2)
        tag(i) := hash_tag(npc(i))
        valid(i) := io.nextBase.valid & validList(idx(i))
        hit(i) := valid(i) & (tag(i) === tagList(idx(i)))
    })

    // 保守做法, 到预测是跳转指令为止.
    val ohit = hit.orR
    val lastValid = OHToUInt(OHMasking.first(hit.asBits))

    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := io.nextBase.valid & (!ohit | (U(i) <= lastValid))
        io.npc(i).payload := npc(i)
    })

// ------------------------------- update -------------------------------

    val updateMask = (0 until config.retireWidth).map(i => io.updateInfo(i).valid).asBits
    val updatePC = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updateIsJumpInst = (0 until config.retireWidth).map(i => io.updateInfo(i).isJumpInst).asBits
    val updateIdx = Vec.fill(config.retireWidth)(UInt(log2Up(config.ftbSize) bits))
    val updateTag = Vec.fill(config.retireWidth)(UInt(config.valen/4 bits))
    val updateValid = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        updatePC(i) := io.updateInfo(i).pc
        updateIdx(i) := updatePC(i)(log2Up(config.ftbSize)+1 downto 2)
        updateTag(i) := hash_tag(updatePC(i))
        updateValid(i) := updateMask(i) & updateIsJumpInst(i)
        tagList(updateIdx(i)) := updateTag(i)
        validList(updateIdx(i)) := updateValid(i)
    })
    
    
    
}