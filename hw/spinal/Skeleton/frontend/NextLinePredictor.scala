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

    val btbValidList = Vec.fill(config.btbSize)(RegInit(False))
    val pBTB = Mem(BTBBundle(config), wordCount = config.btbSize).init(Array.fill(config.btbSize)(BTBBundle(config).resetVal)) // branch target buffer for prediction read.
    val uBTB = Array.fill(config.retireWidth)(Mem(BTBBundle(config), wordCount = config.btbSize).init(Array.fill(config.btbSize)(BTBBundle(config).resetVal))) // branch target buffer for update write.

    val bhtValidList = Vec.fill(config.bhtSize)(RegInit(False))
    val pBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Array.fill(config.bhtSize)(U(0, config.bhtWidth bits))) // branch history table for prediction read.
    val uBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Array.fill(config.bhtSize)(U(0, config.bhtWidth bits))) // branch history table for update write.
    
    def hash_tag(pc: UInt): UInt = {
        // val num = (config.valen-1) / config.btbTagWidth + 1
        // val extPC = pc.resize(num * config.btbTagWidth)
        // extPC(config.btbTagWidth + 1 downto 2)
        pc(config.btbTagWidth + 1 downto 2)
    }

// --------------------------------------------------------------------------------------------------------------------------------
    val predictTarget   = UInt(config.valen bits)
    val predictTaken    = Bool()
    val predictJumpInst = Bool()

    // stage 1
    val stage1 = new Bundle {
        val lastPC      = io.lastPC.payload
        val lastPCValid = io.lastPC.valid
        val GHR         = io.GHR
        val bhtIdx      = lastPC(log2Up(config.bhtSize)+1 downto 2) ^ GHR(log2Up(config.bhtSize)-1 downto 0)
        val btbIdx      = lastPC(log2Up(config.btbSize)+1 downto 2)
        val tag         = hash_tag(lastPC)
        val bhtValid    = lastPCValid & bhtValidList(bhtIdx)
        val btbValid    = lastPCValid & btbValidList(btbIdx)
    }
    
    // 1 to 2
    val stage2 = new Bundle {
        val lastPC      = RegNext(stage1.lastPC)
        val lastPCValid = RegNext(stage1.lastPCValid)
        val GHR         = RegNext(stage1.GHR)
        val tag         = RegNext(stage1.tag)
        val bhtValid    = RegNext(stage1.bhtValid)
        val btbValid    = RegNext(stage1.btbValid)
        // val bht_item    = pBHT.readSync(stage1.bhtIdx, stage1.bhtValid)
        // val btb_item    = pBTB.readSync(stage1.btbIdx, stage1.btbValid)
        val bht_item    = Delay(pBHT(stage1.bhtIdx), 1)
        val btb_item    = Delay(pBTB(stage1.btbIdx), 1)
    }
    
    // stage 2
    predictTaken    := stage2.bhtValid & stage2.bht_item.msb
    predictJumpInst := stage2.btbValid & (stage2.btb_item.tag === stage2.tag)
    predictTarget := Mux(predictJumpInst & predictTaken, stage2.lastPC(31 downto 20) @@ stage2.btb_item.target @@ U(0, 2 bits), (stage2.lastPC + 4))
    
    nextBase := predictTarget
    
    io.nextBase.valid             := stage2.lastPCValid
    io.nextBase.payload           := nextBase
    io.branchInfo.predictTarget   := predictTarget
    io.branchInfo.predictTaken    := predictTaken & predictJumpInst
    io.branchInfo.predictJumpInst := predictJumpInst
    io.branchInfo.GHR             := stage2.GHR

    if (config.debug) io.branchInfo.pc := stage2.lastPC
// ---------------------------------------------------------------------------------------------------------------

    // stage 1: read
    val updateStage1 = new Bundle {
        val mask          = (0 until config.retireWidth).map(i => io.updateInfo(i).valid).asBits
        val pc            = Vec.fill(config.retireWidth)(UInt(config.valen bits))
        val isJumpInst    = (0 until config.retireWidth).map(i => io.updateInfo(i).isJumpInst).asBits
        val taken         = (0 until config.retireWidth).map(i => io.updateInfo(i).taken).asBits
        val targetPC      = Vec.fill(config.retireWidth)(UInt(config.valen bits))
        val GHR           = Vec.fill(config.retireWidth)(UInt(config.ghrWidth bits))
        val firstWriteIdx = OHToUInt(OHMasking.first(mask & isJumpInst))
        val firstRen      = (mask & isJumpInst).orR
        val bhtIdx        = pc(firstWriteIdx)(log2Up(config.bhtSize)+1 downto 2) ^ GHR(firstWriteIdx)(log2Up(config.bhtSize)-1 downto 0)
        val btbIdx        = Vec.fill(config.retireWidth)(UInt(log2Up(config.btbSize) bits))
        val btbTag        = Vec.fill(config.retireWidth)(UInt(config.btbTagWidth bits))
    }
    (0 until config.retireWidth).map(i => {
        updateStage1.pc(i)       := io.updateInfo(i).pc
        updateStage1.targetPC(i) := io.updateInfo(i).targetPC
        updateStage1.GHR(i)      := io.updateInfo(i).GHR
        updateStage1.btbIdx(i)   := io.updateInfo(i).pc(log2Up(config.btbSize)+1 downto 2)
        updateStage1.btbTag(i)   := hash_tag(io.updateInfo(i).pc)
    })
    
    // 1 to 2
    val updateStage2 = new Bundle {
        val mask          = RegNext(updateStage1.mask)
        val isJumpInst    = RegNext(updateStage1.isJumpInst)
        val taken         = RegNext(updateStage1.taken(updateStage1.firstWriteIdx))
        val targetPC      = RegNext(updateStage1.targetPC(updateStage1.firstWriteIdx))
        val firstWriteIdx = RegNext(updateStage1.firstWriteIdx)
        val bhtIdx        = RegNext(updateStage1.bhtIdx)
        val btbIdx        = RegNext(updateStage1.btbIdx)
        val btbTag        = RegNext(updateStage1.btbTag)
        val bhtItem       = uBHT.readSync(updateStage1.bhtIdx, updateStage1.firstRen & bhtValidList(updateStage1.bhtIdx))
        val btbItem       = (0 until config.retireWidth).map(i => {uBTB(i).readSync(updateStage1.btbIdx(i), updateStage1.firstRen & btbValidList(updateStage1.btbIdx(i)))})
    }
    
    // stage 2: write
    val updateBtbHit = (0 until config.retireWidth).map(i => {btbValidList(updateStage2.btbIdx(i)) & (updateStage2.btbTag(i) === updateStage2.btbItem(i).tag)})
    (0 until config.retireWidth).map(i => {
        when(updateStage2.mask(i) & updateBtbHit(i) & !updateStage2.isJumpInst(i)) {
                btbValidList(updateStage2.btbIdx(i)) := False
        }
    })
    val updateBhtHit = bhtValidList(updateStage2.bhtIdx)

    val firstWdataBht = U(1, config.bhtWidth bits).rotateRight(1)
    val firstWdataBtb = BTBBundle(config).resetVal
    val firstWenBtb   = False
    when (updateStage2.mask(updateStage2.firstWriteIdx) & updateStage2.isJumpInst(updateStage2.firstWriteIdx)) {
        when(updateBtbHit(updateStage2.firstWriteIdx) & updateBhtHit){
            when (updateStage2.taken) {
                firstWdataBht := updateStage2.bhtItem +| U(1, config.bhtWidth bits)
            } .otherwise {
                firstWdataBht := updateStage2.bhtItem -| U(1, config.bhtWidth bits)
            }
        } .elsewhen(!updateBtbHit(updateStage2.firstWriteIdx) & updateStage2.taken) {
            // first write
            firstWdataBtb := BTBBundle(config).setVal(updateStage2.btbTag(updateStage2.firstWriteIdx), updateStage2.targetPC(19 downto 2))
            firstWenBtb   := True
            btbValidList(updateStage2.btbIdx(updateStage2.firstWriteIdx)) := True
        }
    }
    pBTB.write(updateStage2.btbIdx(updateStage2.firstWriteIdx), firstWdataBtb, firstWenBtb)
    (0 until config.retireWidth).map(i => {
        uBTB(i).write(updateStage2.btbIdx(updateStage2.firstWriteIdx), firstWdataBtb, firstWenBtb)
    })
    pBHT.write(updateStage2.bhtIdx, firstWdataBht)
    uBHT.write(updateStage2.bhtIdx, firstWdataBht)
    bhtValidList(updateStage2.bhtIdx) := True

}