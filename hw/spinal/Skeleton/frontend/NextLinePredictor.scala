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
    val pBTB = Mem(BTBBundle(config), wordCount = config.btbSize).init(Seq.fill(config.btbSize)(BTBBundle(config).resetVal)) // branch target buffer for prediction read.
    val uBTB = Array.fill(config.retireWidth)(Mem(BTBBundle(config), wordCount = config.btbSize).init(Seq.fill(config.btbSize)(BTBBundle(config).resetVal))) // branch target buffer for update write.
    val pBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Seq.fill(config.bhtSize)(U(1, config.bhtWidth bits))) // branch history table for prediction read.
    val uBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Seq.fill(config.bhtSize)(U(1, config.bhtWidth bits))) // branch history table for update write.
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
    val bht_item = pBHT.readAsync(bhtIdx) // 异步读, 延迟很大
    val btb_item = pBTB.readAsync(btbIdx) // 异步读, 延迟很大
    predictTaken := valid & bht_item.orR
    // predictTaken := valid & CountOne(bht_item) > U(1)
    // predictTaken := valid
    predictJumpInst := valid & (btb_item.tag === tag)
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
// ---------------------------------------------------------------------------------------------------------------

    // stage 1: read
    val updateMask = (0 until config.retireWidth).map(i => io.updateInfo(i).valid).asBits
    val updatePC = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updateIsJumpInst = (0 until config.retireWidth).map(i => io.updateInfo(i).isJumpInst).asBits
    val updateTaken = (0 until config.retireWidth).map(i => io.updateInfo(i).taken).asBits
    val updateTargetPC = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updateGHR = Vec.fill(config.retireWidth)(UInt(config.ghrWidth bits))
    (0 until config.retireWidth).map(i => {
        updatePC(i) := io.updateInfo(i).pc
        updateTargetPC(i) := io.updateInfo(i).targetPC
        updateGHR(i) := io.updateInfo(i).GHR
    })
    
    val updateBhtIdx = Vec.fill(config.retireWidth)(UInt(log2Up(config.bhtSize) bits))
    val updateBtbIdx = Vec.fill(config.retireWidth)(UInt(log2Up(config.btbSize) bits))
    val updateBtbTag = Vec.fill(config.retireWidth)(UInt(config.valen/4 bits))
    (0 until config.retireWidth).map(i => {
        updateBhtIdx(i) := updatePC(i)(log2Up(config.bhtSize)+1 downto 2) ^ updateGHR(i)(log2Up(config.bhtSize)-1 downto 0)
        updateBtbIdx(i) := updatePC(i)(log2Up(config.btbSize)+1 downto 2)
        updateBtbTag(i) := hash_tag(updatePC(i))
    })
    
    val firstWriteIdx = OHToUInt(OHMasking.first(updateMask & updateIsJumpInst))
    val firstRen = (updateMask & updateIsJumpInst).orR
    
    // 1 to 2
    val updateMaskReg = RegNext(updateMask)
    val updatePCReg = RegNext(updatePC)
    val updateIsJumpInstReg = RegNext(updateIsJumpInst)
    val updateTakenReg = RegNext(updateTaken)
    val updateTargetPCReg = RegNext(updateTargetPC)
    val updateGHRReg = RegNext(updateGHR)
    val updateBhtIdxReg = RegNext(updateBhtIdx)
    val updateBtbIdxReg = RegNext(updateBtbIdx)
    val updateBtbTagReg = RegNext(updateBtbTag)
    val updateBhtItem = (0 until config.retireWidth).map(i => {uBHT.readSync(updateBhtIdx(firstWriteIdx), firstRen)})
    val updateBtbItem = (0 until config.retireWidth).map(i => {uBTB(i).readSync(updateBtbIdx(firstWriteIdx), firstRen)})
    val firstWriteIdxReg = RegNext(firstWriteIdx)
    
    // stage 2: write
    val updateValid = (0 until config.retireWidth).map(i => {validList(updateBtbIdxReg(i)) & (updateBtbTagReg(i) === updateBtbItem(i).tag)})
    (0 until config.retireWidth).map(i => {
        when(updateMask(i) & updateValid(i) & !updateIsJumpInst(i)) {
                validList(updateBtbIdx(i)) := False
        }
    })
    val firstWdataBht = U(1, config.bhtWidth bits)
    val firstWdataBtb = BTBBundle(config).resetVal
    val firstWenBtb = False
    when(updateValid(firstWriteIdxReg) & updateIsJumpInstReg(firstWriteIdxReg)){
        firstWdataBht := updateBhtItem(firstWriteIdxReg) |<< 1 +| updateTakenReg(firstWriteIdxReg).asUInt
    } .elsewhen(!updateValid(firstWriteIdxReg) & updateIsJumpInstReg(firstWriteIdxReg) & updateTakenReg(firstWriteIdxReg)){
        firstWdataBtb := BTBBundle(config).setVal(updateBtbTagReg(firstWriteIdxReg), updatePCReg(firstWriteIdxReg)(19 downto 2))
        firstWenBtb := True
    }
    pBTB.write(updateBtbIdx(firstWriteIdxReg), firstWdataBtb, firstWenBtb)
    (0 until config.retireWidth).map(i => {
        uBTB(i).write(updateBtbIdx(firstWriteIdxReg), firstWdataBtb, firstWenBtb)
    })
    when (firstWenBtb) {
        validList(updateBtbIdx(firstWriteIdxReg)) := True
    }
    pBHT.write(updateBhtIdx(firstWriteIdxReg), firstWdataBht)
    uBHT.write(updateBhtIdx(firstWriteIdxReg), firstWdataBht)
    
}