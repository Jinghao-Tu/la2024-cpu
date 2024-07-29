package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class NextLinePredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val lastPC = slave Flow(UInt(config.valen bits)) // time base
        val nextBase = master Flow(UInt(config.valen bits)) // 0-latency
        val branchInfo = out(BranchInfo(config)) // 0-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config)))
        
        val GHR = in(UInt(config.ghrWidth bits))
    }

    val nextBase = UInt(config.valen bits)
    val lastPC = UInt(config.valen bits)
    lastPC := io.lastPC.payload

    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR

    // must bram, not reg
    val BTB = Mem(BTBBundle(config), wordCount = config.btbSize) // branch target buffer
    val pBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) // branch history table for prediction read.
    val uBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) // branch history table for update read.
    val validList = RegInit(B(0, config.btbSize bits))
    
    def hash_tag(pc: UInt): UInt = {
        (0 until 4).map(i => {
            pc(((i + 1) * config.valen/4 - 1) downto (i * config.valen/4))
        }).reduce(_ ^ _)
    }

// --------------------------------------------------------------------------------------------------------------------------------
    // TODO: 也许考虑换成 1 延迟预测, 0 延迟预测使用 pc+4 替代.
    val predictTarget = UInt(config.valen bits)
    val predictTaken = Bool()
    val predictJumpInst = Bool()

    // val index = hash_index(lastPC, GHR, 4)
    val bhtIdx = lastPC(log2Up(config.bhtSize)+1 downto 2) ^ GHR(log2Up(config.bhtSize)-1 downto 0)
    val btbIdx = lastPC(log2Up(config.btbSize)+1 downto 2)
    val tag = hash_tag(lastPC)
    val valid = io.lastPC.valid & validList(btbIdx)  // reg reading is fast.
    // val bht_item = pBHT.readSync(bhtIdx, valid)
    // val btb_item = BTB.readSync(btbIdx, valid)
    val bht_item = pBHT.readAsync(bhtIdx) // 异步读, 延迟很大
    val btb_item = BTB.readAsync(btbIdx) // 异步读, 延迟很大
    // predictTaken := CountOne(bht_item.asBools) > 1
    predictTaken := valid & bht_item.orR // avoid X.
    predictJumpInst := valid & (btb_item.tag === tag) // avoid X.
    switch(predictJumpInst & predictTaken) {
        is(True) {
            predictTarget := lastPC(31 downto 20) @@ btb_item.target @@ U(0, 2 bits)
        }
        default {
            predictTarget := lastPC + 4
        }
    }
    
    nextBase := predictTarget
    
    io.nextBase.valid := io.lastPC.valid // 0-latency
    io.nextBase.payload := nextBase
    if (config.debug) io.branchInfo.pc := lastPC
    io.branchInfo.predictTarget := predictTarget
    io.branchInfo.predictTaken := predictTaken & predictJumpInst
    io.branchInfo.predictJumpInst := predictJumpInst
    io.branchInfo.GHR := U(0).resized
// --------------------------------------------------------------------------------------------------------------------------------

    // 将更新信息存入队列中, 每周期写 ram.
    // 关键点在于 跳转指令并没有那么多, 所以每周期可以更新若干个 valid 和至多一次 btb 和 bht.
    
    val queueLength = 64
    val writeQueue = Vec.fill(queueLength)(Reg(writeBundle(config)))
    val head = Reg(UInt(log2Up(queueLength) bits)) init(0)
    val tail = Reg(UInt(log2Up(queueLength) bits)) init(queueLength - 1)
    val empty = (tail + U(1) === head) && !(writeQueue(tail).writeValid || writeQueue(tail).writeRAM)
    val reverse = (head > tail) && (writeQueue(head).writeValid || writeQueue(head).writeRAM)

    (0 until config.retireWidth).map(i => {
        // 2-cycle: read, write

        // stage 1: read
        val updateMask = io.updateInfo(i).valid
        val updatePC = io.updateInfo(i).payload.pc
        val updateIsJumpInst = io.updateInfo(i).payload.isJumpInst
        val updateTaken = io.updateInfo(i).payload.taken
        val updateTarget = io.updateInfo(i).payload.targetPC
        val updateGHR = io.updateInfo(i).payload.GHR
        val updatePredictFail = io.updateInfo(i).payload.predictFail

        val updBhtIdx = updatePC(log2Up(config.bhtSize)+1 downto 2) ^ updateGHR(log2Up(config.bhtSize)-1 downto 0)
        val updBtbIdx = updatePC(log2Up(config.btbSize)+1 downto 2)
        val updTag = hash_tag(updatePC)
        val updValid = validList(updBtbIdx) & updateMask
        
        // data from 1 to 2
        val updateMaskReg = RegNext(updateMask)
        val updateIsJumpInstReg = RegNext(updateIsJumpInst)
        val updateTakenReg = RegNext(updateTaken)
        val updateTargetReg = RegNext(updateTarget)
        val updatePredictFailReg = RegNext(updatePredictFail)
        val updBhtIdxReg = RegNext(updBhtIdx)
        val updBtbIdxReg = RegNext(updBtbIdx)
        val updTagReg = RegNext(updTag)
        val updValidReg = RegNext(updValid)
        // val uBHTItem = uBHT.readSync(updBhtIdx, updValid)
        val uBHTItem = RegNext(uBHT.readSync(updBhtIdx, updValid))
        
        // stage 2: store write data into queue
        val writeRAM = False
        val wdataBHT = U(0, config.bhtWidth bits)
        val wdataBTB = BTBBundle(config).resetVal
        val writeValid = False
        val wdataValid = False
        when (updateMaskReg) {
            when(updValidReg) {
                when(updateIsJumpInstReg) {
                    writeRAM := True
                    // update bht
                    wdataBHT := uBHTItem |<< U(1) + updateTakenReg.asUInt
                    // update btb
                    wdataBTB := BTBBundle(config).setVal(updTagReg, updateTargetReg(config.predictInstWidth + 1 downto 2))
                    
                } .otherwise {
                    // update valid
                    writeValid := True
                    wdataValid := False
                }
            } .otherwise {
                when (updateTakenReg) {
                    writeRAM := True
                    // update bht
                    wdataBHT := U(1, config.bhtWidth bits)
                    // update btb
                    wdataBTB := BTBBundle(config).setVal(updTagReg, updateTargetReg(config.predictInstWidth + 1 downto 2))
                    // update valid
                    writeValid := True
                    wdataValid := True
                }
            }
        }
        // pBHT.write(updBhtIdxReg, wdataBHT, updateMaskReg(i) & (updValidReg & updateIsJumpInstReg || !updValidReg & updateTakenReg))
        // uBHT.write(updBhtIdxReg, wdataBHT, updateMaskReg(i) & (updValidReg & updateIsJumpInstReg || !updValidReg & updateTakenReg))
        // BTB.write(updBtbIdxReg, wdataBTB, updateMaskReg(i) & (updValidReg & updateIsJumpInstReg || !updValidReg & updateTakenReg))
        // 即便无效也存入.
        writeQueue(tail + U(i + 1)) := writeQueue(tail).setVal(updBhtIdxReg, updBtbIdxReg, writeRAM, wdataBHT, wdataBTB, writeValid, wdataValid)
    })
    tail := tail + U(config.retireWidth)
    
    // 写 valid, bht, btb. 一直写 valid, 直到遇到第二次需要写 bht 或 btb 的情况, 或为空.
    val writeMask = Bits(queueLength bits)
    writeMask := B((0 until queueLength).map(i => writeQueue(i).writeRAM)).rotateLeft(head)
    
}

case class writeBundle(config: CPUConfig) extends Bundle {
    val bhtIdx = UInt(log2Up(config.bhtSize) bits)
    val btbIdx = UInt(log2Up(config.btbSize) bits)
    val writeRAM = Bool()
    val wdataBHT = UInt(config.bhtWidth bits)
    val wdataBTB = BTBBundle(config)
    val writeValid = Bool()
    val wdataValid = Bool()
    
    def resetVal: writeBundle = {
        val value = writeBundle(config)
        value.bhtIdx := U(0, log2Up(config.bhtSize) bits)
        value.btbIdx := U(0, log2Up(config.btbSize) bits)
        value.writeRAM := False
        value.wdataBHT := U(0, config.bhtWidth bits)
        value.wdataBTB := BTBBundle(config).resetVal
        value.writeValid := False
        value.wdataValid := False
        value
    }

    def setVal(bhtIdx: UInt, btbIdx: UInt, writeRAM: Bool, wdataBHT: UInt, wdataBTB: BTBBundle, writeValid: Bool, wdataValid: Bool): writeBundle = {
        val value = writeBundle(config)
        value.bhtIdx := bhtIdx
        value.btbIdx := btbIdx
        value.writeRAM := writeRAM
        value.wdataBHT := wdataBHT
        value.wdataBTB := wdataBTB
        value.writeValid := writeValid
        value.wdataValid := wdataValid
        value
    }
}