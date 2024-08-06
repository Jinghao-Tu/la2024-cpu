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

    val btbValidList = RegInit(B(0, config.btbSize bits))
    val pBTB = Mem(BTBBundle(config), wordCount = config.btbSize).init(Array.fill(config.btbSize)(BTBBundle(config).resetVal)) // branch target buffer for prediction read.
    val uBTB = Array.fill(config.retireWidth)(Mem(BTBBundle(config), wordCount = config.btbSize).init(Array.fill(config.btbSize)(BTBBundle(config).resetVal))) // branch target buffer for update write.

    val bhtValidList = RegInit(B(0, config.bhtSize bits))
    val pBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Array.fill(config.bhtSize)(U(0, config.bhtWidth bits))) // branch history table for prediction read.
    val uBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Array.fill(config.bhtSize)(U(0, config.bhtWidth bits))) // branch history table for update write.
    
    def hash_tag(pc: UInt): UInt = {
        val num = (config.valen-1) / config.btbTagWidth + 1
        val extPC = pc.resize(num * config.btbTagWidth)
        // (0 until num).map(i => {
        //     extPC(i * config.btbTagWidth + config.btbTagWidth - 1 downto i * config.btbTagWidth)
        // }).reduce(_ ^ _)
        extPC(config.btbTagWidth + 1 downto 2)
    }

// --------------------------------------------------------------------------------------------------------------------------------
    val predictTarget   = UInt(config.valen bits)
    val predictTaken    = Bool()
    val predictJumpInst = Bool()

    val bhtIdx           = lastPC(log2Up(config.bhtSize)+1 downto 2) ^ GHR(log2Up(config.bhtSize)-1 downto 0)
    val btbIdx           = lastPC(log2Up(config.btbSize)+1 downto 2)
    val tag              = hash_tag(lastPC)
    val bhtValid         = io.lastPC.valid & bhtValidList(bhtIdx)
    val btbValid         = io.lastPC.valid & btbValidList(btbIdx)
    val bht_item         = pBHT.readAsync(bhtIdx)
    val btb_item         = pBTB.readAsync(btbIdx)
        predictTaken    := bhtValid & bht_item.msb
        predictJumpInst := btbValid & btb_item.tag === tag
    switch(predictJumpInst & predictTaken) {
        is(True) {
            predictTarget := lastPC(31 downto 20) @@ btb_item.target @@ U(0, 2 bits)
        }
        default {
            predictTarget := lastPC + 4
        }
    }
    
    nextBase := predictTarget
    
    io.nextBase.valid             := io.lastPC.valid
    io.nextBase.payload           := nextBase
    io.branchInfo.predictTarget   := predictTarget
    io.branchInfo.predictTaken    := predictTaken & predictJumpInst
    io.branchInfo.predictJumpInst := predictJumpInst
    io.branchInfo.GHR             := GHR

    if (config.debug) io.branchInfo.pc := lastPC
// ---------------------------------------------------------------------------------------------------------------

    // stage 1: read
    val updateMaskStage1       = (0 until config.retireWidth).map(i => io.updateInfo(i).valid).asBits
    val updatePCStage1         = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updateIsJumpInstStage1 = (0 until config.retireWidth).map(i => io.updateInfo(i).isJumpInst).asBits
    val updateTakenStage1      = (0 until config.retireWidth).map(i => io.updateInfo(i).taken).asBits
    val updateTargetPCStage1   = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updateGHRStage1        = Vec.fill(config.retireWidth)(UInt(config.ghrWidth bits))
    (0 until config.retireWidth).map(i => {
        updatePCStage1(i)       := io.updateInfo(i).pc
        updateTargetPCStage1(i) := io.updateInfo(i).targetPC
        updateGHRStage1(i)      := io.updateInfo(i).GHR
    })
    
    val firstWriteIdxStage1 = OHToUInt(OHMasking.first(updateMaskStage1 & updateIsJumpInstStage1))
    val firstRenStage1      = (updateMaskStage1 & updateIsJumpInstStage1).orR
    
    val updateBhtIdxStage1 = UInt(log2Up(config.bhtSize) bits)
    val updateBtbIdxStage1 = Vec.fill(config.retireWidth)(UInt(log2Up(config.btbSize) bits))
    val updateBtbTagStage1 = Vec.fill(config.retireWidth)(UInt(config.valen/4 bits))
    updateBhtIdxStage1 := updatePCStage1(firstWriteIdxStage1)(log2Up(config.bhtSize)+1 downto 2) ^ updateGHRStage1(firstWriteIdxStage1)(log2Up(config.bhtSize)-1 downto 0)
    (0 until config.retireWidth).map(i => {
        updateBtbIdxStage1(i) := updatePCStage1(i)(log2Up(config.btbSize)+1 downto 2)
        updateBtbTagStage1(i) := hash_tag(updatePCStage1(i))
    })
    
    // 1 to 2
    val updateMaskSatge2       = RegNext(updateMaskStage1)
    val updatePCSatge2         = RegNext(updatePCStage1)
    val updateIsJumpInstStage2 = RegNext(updateIsJumpInstStage1)
    val updateTakenStage2      = RegNext(updateTakenStage1)
    val updateTargetPCStage2   = RegNext(updateTargetPCStage1)
    val updateGHRStage2        = RegNext(updateGHRStage1)
    val updateBhtIdxStage2     = RegNext(updateBhtIdxStage1)
    val updateBtbIdxStage2     = RegNext(updateBtbIdxStage1)
    val updateBtbTagStage2     = RegNext(updateBtbTagStage1)
    val firstWriteIdxStage2    = RegNext(firstWriteIdxStage1)
    val updateBhtItem          = pBHT.readSync(updateBhtIdxStage1, firstRenStage1 & bhtValidList(updateBhtIdxStage1))
    val updateBtbItem          = (0 until config.retireWidth).map(i => {pBTB.readSync(updateBtbIdxStage1(i), firstRenStage1 & btbValidList(updateBtbIdxStage1(i)))})
    
    // stage 2: write
    val updateBtbHit = (0 until config.retireWidth).map(i => {btbValidList(updateBtbIdxStage2(i)) & (updateBtbTagStage2(i) === updateBtbItem(i).tag)})
    (0 until config.retireWidth).map(i => {
        when(updateMaskSatge2(i) & updateBtbHit(i) & !updateIsJumpInstStage2(i)) {
                btbValidList(updateBtbIdxStage2(i)) := False
        }
    })
    val updateBhtHit = bhtValidList(updateBhtIdxStage2)

    val firstWdataBht = U(1, config.bhtWidth bits).rotateRight(1)
    val firstWdataBtb = BTBBundle(config).resetVal
    val firstWenBtb   = False
    when (updateMaskSatge2(firstWriteIdxStage2) & updateIsJumpInstStage2(firstWriteIdxStage2)) {
        when(updateBtbHit(firstWriteIdxStage2) & updateBhtHit){
            when (updateTakenStage2(firstWriteIdxStage2)) {
                firstWdataBht := updateBhtItem +| U(1, config.bhtWidth bits)
            } .otherwise {
                firstWdataBht := updateBhtItem -| U(1, config.bhtWidth bits)
            }
        } .elsewhen(!updateBtbHit(firstWriteIdxStage2)& updateTakenStage2(firstWriteIdxStage2)){
            // first write
            firstWdataBtb := BTBBundle(config).setVal(updateBtbTagStage2(firstWriteIdxStage2), updateTargetPCStage2(firstWriteIdxStage2)(19 downto 2))
            firstWenBtb   := True
            btbValidList(updateBtbIdxStage2(firstWriteIdxStage2)) := True
        }
    }
    pBTB.write(updateBtbIdxStage2(firstWriteIdxStage2), firstWdataBtb, firstWenBtb)
    (0 until config.retireWidth).map(i => {
        uBTB(i).write(updateBtbIdxStage2(firstWriteIdxStage2), firstWdataBtb, firstWenBtb)
    })
    pBHT.write(updateBhtIdxStage2, firstWdataBht)
    uBHT.write(updateBhtIdxStage2, firstWdataBht)
    bhtValidList(updateBhtIdxStage2) := True

}