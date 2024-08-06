package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._
import Skeleton.bundle.CRUROOp.lo

case class FullPredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val lastPC = slave Flow(UInt(config.valen bits)) // time base
        val nextBase = master Flow(UInt(config.valen bits)) // 1-latency
        val branchInfo = out(BranchInfo(config)) // 1-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config)))

        val GHR = in(UInt(config.ghrWidth bits))
    }

    val nextBase = UInt(config.valen bits)
    val lastPC = UInt(config.valen bits)
    lastPC := io.lastPC.payload
    val pred_valid = Reg(Bool())
    pred_valid := io.lastPC.valid // 1-latency

    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR

// ------------------------------------------------------------------------------------------
    val btbValidList = RegInit(B(0, config.btbSize bits))
    val pBTB = Mem(BTBBundle(config), wordCount = config.btbSize).init(Array.fill(config.btbSize)(BTBBundle(config).resetVal))
    val uBTB = Array.fill(config.retireWidth)(Mem(BTBBundle(config), wordCount = config.btbSize).init(Array.fill(config.btbSize)(BTBBundle(config).resetVal)))

    val bhtValidList = RegInit(B(0, config.bhtSize bits))
    val pBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Array.fill(config.bhtSize)(U(0, config.bhtWidth bits)))
    val uBHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize).init(Array.fill(config.bhtSize)(U(0, config.bhtWidth bits)))

    val phtValidLists = Vec.fill(config.phtNum)(RegInit(B(0, config.phtSize bits)))
    val pPHT = Array.fill(config.phtNum)(
      Mem(PHTBundle(config), wordCount = config.phtSize).init(Array.fill(config.phtSize)(PHTBundle(config).resetVal))
    )
    val uPHT = Array.fill(config.phtNum)(
      Mem(PHTBundle(config), wordCount = config.phtSize).init(Array.fill(config.phtSize)(PHTBundle(config).resetVal))
    )

    def hash_index(pc: UInt, GHR: UInt, level: Int): UInt = {
        if (level == 0) {
            // bht index
            pc(log2Up(config.bhtSize) + 1 downto 2) ^ GHR(log2Up(config.bhtSize) - 1 downto 0)
        } else {
            // pht index
            val phtLenWidth = log2Up(config.phtSize)
            val numPC = (config.valen - 1) / phtLenWidth + 1
            val numGHR = 1 << (level - 1)
            val extPC = pc.resize(numPC * phtLenWidth)
            val extGHR = GHR.resize(numGHR * phtLenWidth)
            val hashPC = (0 until numPC).map(i => {
                extPC(i * phtLenWidth + phtLenWidth - 1 downto i * phtLenWidth)
            }).reduce(_ ^ _)
            val hashGHR = (0 until numGHR).map(i => {
                extGHR(i * phtLenWidth + phtLenWidth - 1 downto i * phtLenWidth)
            }).reduce(_ ^ _)
            hashPC ^ hashGHR
        }
    }

    def hash_tag(pc: UInt, GHR: UInt, level: Int): UInt = {
        if (level == 0) {
            // bht tag
            val num = (config.valen - 1) / config.btbTagWidth + 1
            val extPC = pc.resize(num * config.btbTagWidth)
            // (0 until num).map(i => {
            //     extPC(i * config.btbTagWidth + config.btbTagWidth - 1 downto i * config.btbTagWidth)
            // }).reduce(_ ^ _)
            extPC(config.btbTagWidth + 1 downto 2)
        } else {
            // pht tag
            val numPC = (config.valen - 1) / config.phtTagWidth + 1
            val numGHR = 1 << (level - 1)
            val extPC = pc.resize(numPC * config.phtTagWidth)
            val extGHR = GHR.resize(numGHR * config.phtTagWidth)
            val hashPC = (0 until numPC).map(i => {
                extPC(i * config.phtTagWidth + config.phtTagWidth - 1 downto i * config.phtTagWidth)
            }).reduce(_ ^ _)
            val hashGHR = (0 until numGHR).map(i => {
                extGHR(i * config.phtTagWidth + config.phtTagWidth - 1 downto i * config.phtTagWidth)
            }).reduce(_ ^ _)
            // hashPC ^ hashGHR
            extPC(config.phtTagWidth + 1 downto 2) ^ extGHR(config.phtTagWidth - 1 downto 0)
        }
    }

// ------------------------------------------------------------------------------------------
    val lastPCReg = RegNext(lastPC)
    val GHRReg = RegNext(GHR)

    val predictTarget = UInt(config.valen bits)
    val predictTaken = Bool()
    val predictJumpInst = Bool()

    // stage 1
    val bhtIdx = hash_index(lastPC, GHR, 0)
    val phtIdx = Vec.fill(config.phtNum)(UInt(log2Up(config.phtSize) bits))
    (0 until config.phtNum).map(j => {
        phtIdx(j) := hash_index(lastPC, GHR, j+1)
    })
    val btbIdx = lastPC(log2Up(config.btbSize) + 1 downto 2)
    val bhtRen = io.lastPC.valid && bhtValidList(bhtIdx)
    val phtRen = Bits(config.phtNum bits)
    (0 until config.phtNum).map(j => {
        phtRen(j) := io.lastPC.valid && phtValidLists(j)(phtIdx(j))
    })
    val btbRen = io.lastPC.valid && btbValidList(btbIdx)
    
    // 1 to 2
    val valid = RegNext(io.lastPC.valid)
    val bhtRen2 = RegNext(bhtRen)
    val phtRen2 = RegNext(phtRen)
    val btbRen2 = RegNext(btbRen)
    val bhtItem = pBHT.readSync(bhtIdx, bhtRen)
    val phtItem = Vec.fill(config.phtNum)(PHTBundle(config))
    val phtTag  = Reg(Vec.fill(config.phtNum)(UInt(config.phtTagWidth bits)))
    (0 until config.phtNum).map(j => {
        phtItem(j) := pPHT(j).readSync(phtIdx(j), phtRen(j))
        phtTag(j) := hash_tag(lastPC, GHR, j+1)
    })
    val btbItem = pBTB.readSync(btbIdx, btbRen)
    val btbTag  = RegNext(hash_tag(lastPC, GHR, 0))
    
    // stage 2
    val bhtPred = bhtRen2 & bhtItem.msb
    val phtHit  = Bits(config.phtNum bits)
    val phtPred = Bits(config.phtNum bits)
    (0 until config.phtNum).map(j => {
        val counter = phtItem(j).counter
        phtHit(j) := phtRen2(j) & phtItem(j).tag === phtTag(j) & phtItem(j).useful =/= U(0)
        phtPred(j) := phtRen2(j) & counter.msb
    })
    val predIdx = OHToUInt(OHMasking.last(phtHit))
    val predHit = phtHit.orR
    val btbHit  = btbRen2 & btbItem.tag === btbTag
    
    predictTaken    := (predHit & phtPred(predIdx)) | (!predHit & bhtPred)
    predictJumpInst := btbHit
    switch(predictJumpInst & predictTaken) {
        is(True) {
            predictTarget := lastPCReg(31 downto 20) @@ btbItem.target @@ U(0, 2 bits)
        }
        default {
            predictTarget := lastPCReg + 4
        }
    }
    
    nextBase := predictTarget

    io.nextBase.valid             := valid
    io.nextBase.payload           := nextBase
    io.branchInfo.predictTarget   := predictTarget
    io.branchInfo.predictTaken    := predictTaken & predictJumpInst
    io.branchInfo.predictJumpInst := predictJumpInst
    io.branchInfo.GHR             := GHRReg
    
    if (config.debug) {
        io.branchInfo.pc := lastPCReg
    }

// ------------------------------------------------------------------------------------------

    // stage 1
    val updateMaskStage1        = (0 until config.retireWidth).map(i => io.updateInfo(i).valid).asBits
    val updatePCStage1          = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updateIsJumpInstStage1  = (0 until config.retireWidth).map(i => io.updateInfo(i).isJumpInst).asBits
    val updateTakenStage1       = (0 until config.retireWidth).map(i => io.updateInfo(i).taken).asBits
    val updateTargetPCStage1    = Vec.fill(config.retireWidth)(UInt(config.valen bits))
    val updatePredictFailStage1 = (0 until config.retireWidth).map(i => io.updateInfo(i).predictFail).asBits
    val updateGHRStage1         = Vec.fill(config.retireWidth)(UInt(config.ghrWidth bits))
    (0 until config.retireWidth).map(i => {
        updatePCStage1(i)       := io.updateInfo(i).pc
        updateTargetPCStage1(i) := io.updateInfo(i).targetPC
        updateGHRStage1(i)      := io.updateInfo(i).GHR
    })
    
    val firstWriteIdxStage1 = OHToUInt(OHMasking.first(updateMaskStage1 & updateIsJumpInstStage1))
    val firstRenStage1      = (updateMaskStage1 & updateIsJumpInstStage1).orR
    
    val updateBhtIdxStage1 = UInt(log2Up(config.bhtSize) bits)
    val updatePhtIdxStage1 = Vec.fill(config.phtNum)(UInt(log2Up(config.phtSize) bits))
    val updatePhtTagStage1 = Vec.fill(config.phtNum)(UInt(config.phtTagWidth bits))
    val updateBtbIdxStage1 = Vec.fill(config.retireWidth)(UInt(log2Up(config.btbSize) bits))
    val updateBtbTagStage1 = Vec.fill(config.retireWidth)(UInt(config.btbTagWidth bits))
    updateBhtIdxStage1 := hash_index(updatePCStage1(firstWriteIdxStage1), updateGHRStage1(firstWriteIdxStage1), 0)
    (0 until config.phtNum).map(j => {
        updatePhtIdxStage1(j) := hash_index(updatePCStage1(firstWriteIdxStage1), updateGHRStage1(firstWriteIdxStage1), j+1)
        updatePhtTagStage1(j) := hash_tag(updatePCStage1(firstWriteIdxStage1), updateGHRStage1(firstWriteIdxStage1), j+1)
    }) 
    (0 until config.retireWidth).map(i => {
        updateBtbIdxStage1(i) := updatePCStage1(i)(log2Up(config.btbSize)+1 downto 2)
        updateBtbTagStage1(i) := hash_tag(updatePCStage1(i), updateGHRStage1(i), 0)
    })
    
    // 1 to 2
    val updateMaskSatge2        = RegNext(updateMaskStage1)
    val updatePCSatge2          = RegNext(updatePCStage1)
    val updateIsJumpInstSatge2  = RegNext(updateIsJumpInstStage1)
    val updateTakenSatge2       = RegNext(updateTakenStage1)
    val updateTargetPCSatge2    = RegNext(updateTargetPCStage1)
    val updatePredictFailStage2 = RegNext(updatePredictFailStage1)
    val updateGHRSatge2         = RegNext(updateGHRStage1)
    val updateBhtIdxSatge2      = RegNext(updateBhtIdxStage1)
    val updatePhtIdxSatge2      = RegNext(updatePhtIdxStage1)
    val updatePhtTagSatge2      = RegNext(updatePhtTagStage1)
    val updateBtbIdxSatge2      = RegNext(updateBtbIdxStage1)
    val updateBtbTagSatge2      = RegNext(updateBtbTagStage1)
    val firstWriteIdxStage2     = RegNext(firstWriteIdxStage1)
    val updateBhtItemStage2     = pBHT.readSync(updateBhtIdxStage1, firstRenStage1 & bhtValidList(updateBhtIdxStage1))
    val updatePhtItemStage2     = Vec.fill(config.phtNum)(PHTBundle(config))
    val updateBtbItemStage2     = Vec.fill(config.retireWidth)(BTBBundle(config))
    (0 until config.phtNum).map(j => {
        updatePhtItemStage2(j) := pPHT(j).readSync(updatePhtIdxStage1(j), firstRenStage1 & phtValidLists(j)(updatePhtIdxStage1(j)))
    })
    (0 until config.retireWidth).map(i => {
        updateBtbItemStage2(i) := pBTB.readSync(updateBtbIdxStage1(i), firstRenStage1 & btbValidList(updateBtbIdxStage1(i)))
    })

    // stage 2
    val updateBtbHit = (0 until config.retireWidth).map(i => {btbValidList(updateBtbIdxSatge2(i)) & (updateBtbTagSatge2(i) === updateBtbItemStage2(i).tag)})
    (0 until config.retireWidth).map(i => {
        when(updateMaskSatge2(i) & updateBtbHit(i) & !updateIsJumpInstSatge2(i)) {
            btbValidList(updateBtbIdxSatge2(i)) := False
        }
    })
    
    val updateBhtHit = bhtValidList(updateBhtIdxSatge2)
    
    val updatePhtPreMask = Bits(config.phtNum bits)
    (0 until config.phtNum).map(j => {
        updatePhtPreMask(j) := phtValidLists(j)(updatePhtIdxSatge2(j)) ? (updatePhtTagSatge2(j) === updatePhtItemStage2(j).tag & updatePhtItemStage2(j).useful =/= U(0) & updatePhtItemStage2(j).counter.msb) | False
    })
    val updatePreIdx = OHToUInt(OHMasking.last(updatePhtPreMask))
    val updatePreHit = updatePhtPreMask.orR

    val updatePhtAltMask = Bits(config.phtNum bits)
    (0 until config.phtNum).map(j => {
        updatePhtAltMask(j) := phtValidLists(j)(updatePhtIdxSatge2(j)) ? (updatePhtPreMask(j) & (U(j) < updatePreIdx)) | False
    })
    val updateAltIdx = OHToUInt(OHMasking.last(updatePhtAltMask))
    val updateAltHit = updatePhtAltMask.orR
    
    val updatePhtNexMask = Bits(config.phtNum bits)
    (0 until config.phtNum).map(j => {
        updatePhtNexMask(j) := phtValidLists(j)(updatePhtIdxSatge2(j)) ? (updatePhtItemStage2(j).useful <= U(1) & (U(j) > updatePreIdx)) | True
    })
    val updateNexIdx = OHToUInt(OHMasking.first(updatePhtNexMask))
    val updateNexHit = updatePhtNexMask.orR
    
    val updatePreTaken = updatePreHit ? updatePhtItemStage2(updatePreIdx).counter.msb | (updateBhtHit & updateBhtItemStage2.msb)
    val updateAltTaken = (updatePreHit && updateAltHit) ? updatePhtItemStage2(updateAltIdx).counter.msb | (updateBhtHit & updateBhtItemStage2.msb)

    val updateWdataPhtUseful  = Vec.fill(config.phtNum)(U(0, config.phtUsefulWidth bits))
    val updateWdataPhtCounter = Vec.fill(config.phtNum)(U(0, config.phtCounterWidth bits))
    val updateWdataPhtTag     = Vec.fill(config.phtNum)(U(0, config.phtTagWidth bits))
    val updateWenPhtUseful    = Vec.fill(config.phtNum)(False)
    val updateWenPhtCounter   = Vec.fill(config.phtNum)(False)
    val updateWenPhtTag       = Vec.fill(config.phtNum)(False)
    val updateWdataBht        = U(1, config.bhtWidth bits).rotateRight(1)
    val updateWenBht          = True
    val updateWdataBtb        = BTBBundle(config).resetVal
    val updateWenBtb          = False
    
    when (updateMaskSatge2(firstWriteIdxStage2) & updateIsJumpInstSatge2(firstWriteIdxStage2)) {
        when (updatePreTaken =/= updateAltTaken) {
            when (updatePreTaken === updateTakenSatge2(firstWriteIdxStage2)) {
                updateWdataPhtUseful(updatePreIdx) := updatePhtItemStage2(updatePreIdx).useful +| U(1, config.phtUsefulWidth bits)
            } .otherwise {
                updateWdataPhtUseful(updatePreIdx) := updatePhtItemStage2(updatePreIdx).useful -| U(1, config.phtUsefulWidth bits)
            }
            updateWenPhtUseful(updatePreIdx) := True
        }
        
        when (!updatePredictFailStage2(firstWriteIdxStage2)) {
            when (!updatePreHit) {
                // update BHT
                when (updateTakenSatge2(firstWriteIdxStage2)) {
                    updateWdataBht := updateBhtItemStage2 +| U(1, config.bhtWidth bits)
                } .otherwise {
                    updateWdataBht := updateBhtItemStage2 -| U(1, config.bhtWidth bits)
                }
            } .elsewhen(!updateAltHit) {
                // update pred and BHT
                when (updateTakenSatge2(firstWriteIdxStage2)) {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter +| U(1, config.phtCounterWidth bits)
                    updateWdataBht := updateBhtItemStage2 +| U(1, config.bhtWidth bits)
                } .otherwise {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter -| U(1, config.phtCounterWidth bits)
                    updateWdataBht := updateBhtItemStage2 -| U(1, config.bhtWidth bits)
                }
            } .otherwise {
                // update pred and alt
                when (updateTakenSatge2(firstWriteIdxStage2)) {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter +| U(1, config.phtCounterWidth bits)
                    updateWdataPhtCounter(updateAltIdx) := updatePhtItemStage2(updateAltIdx).counter -| U(1, config.phtCounterWidth bits)
                } .otherwise {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter -| U(1, config.phtCounterWidth bits)
                    updateWdataPhtCounter(updateAltIdx) := updatePhtItemStage2(updateAltIdx).counter +| U(1, config.phtCounterWidth bits)
                }
            }
        } .otherwise {
            // update counter
            when (!updatePreHit) {
                // update BHT
                when (updateTakenSatge2(firstWriteIdxStage2)) {
                    updateWdataBht := updateBhtItemStage2 +| U(1, config.bhtWidth bits)
                } .otherwise {
                    updateWdataBht := updateBhtItemStage2 -| U(1, config.bhtWidth bits)
                }
            } .elsewhen(!updateAltHit) {
                // update pred and BHT
                when (updateTakenSatge2(firstWriteIdxStage2)) {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter +| U(1, config.phtCounterWidth bits)
                    updateWdataBht := updateBhtItemStage2 +| U(1, config.bhtWidth bits)
                } .otherwise {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter -| U(1, config.phtCounterWidth bits)
                    updateWdataBht := updateBhtItemStage2 -| U(1, config.bhtWidth bits)
                }
                updateWenPhtCounter(updatePreIdx) := True
            } .otherwise {
                // update pred and alt and BHT
                when (updateTakenSatge2(firstWriteIdxStage2)) {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter +| U(1, config.phtCounterWidth bits)
                    updateWdataPhtCounter(updateAltIdx) := updatePhtItemStage2(updateAltIdx).counter -| U(1, config.phtCounterWidth bits)
                    updateWdataBht := updateBhtItemStage2 +| U(1, config.bhtWidth bits)
                } .otherwise {
                    updateWdataPhtCounter(updatePreIdx) := updatePhtItemStage2(updatePreIdx).counter -| U(1, config.phtCounterWidth bits)
                    updateWdataPhtCounter(updateAltIdx) := updatePhtItemStage2(updateAltIdx).counter +| U(1, config.phtCounterWidth bits)
                    updateWdataBht := updateBhtItemStage2 -| U(1, config.bhtWidth bits)
                }
                updateWenPhtCounter(updatePreIdx) := True
                updateWenPhtCounter(updateAltIdx) := True
            }
            
            // allocate next entry
            when (updateNexHit) {
                // update tag, counter, useful
                updateWdataPhtTag(updateNexIdx)     := updatePhtTagSatge2(updateNexIdx)
                updateWdataPhtCounter(updateNexIdx) := updateTakenSatge2(firstWriteIdxStage2) ? U(1, config.phtCounterWidth bits).rotateRight(1) | (U(1, config.phtCounterWidth bits).rotateRight(1) - U(1, config.phtCounterWidth bits))
                updateWdataPhtUseful(updateNexIdx)  := U(1, config.phtUsefulWidth bits)
                updateWenPhtTag(updateNexIdx)       := True
                updateWenPhtCounter(updateNexIdx)   := True
                updateWenPhtUseful(updateNexIdx)    := True
                
                phtValidLists(updateNexIdx)(updatePhtIdxSatge2(updateNexIdx)) := True
            } .otherwise {
                // all useful - 1
                (0 until config.phtNum).map(j => {
                    updateWdataPhtUseful(j) := updatePhtItemStage2(j).useful -| U(1, config.phtUsefulWidth bits)
                    updateWenPhtUseful(j) := phtValidLists(j)(updatePhtIdxSatge2(j))
                })
            }
            
            // update btbValidList and BTB
            updateWdataBtb := BTBBundle(config).setVal(updateBtbTagSatge2(firstWriteIdxStage2), updateTargetPCSatge2(firstWriteIdxStage2)(19 downto 2))
            updateWenBtb := True
            btbValidList(updateBtbIdxSatge2(firstWriteIdxStage2)) := True
        }
    }
    
    pBHT.write(updateBhtIdxSatge2, updateWdataBht, updateWenBht)
    uBHT.write(updateBhtIdxSatge2, updateWdataBht, updateWenBht)
    bhtValidList(updateBhtIdxSatge2) := True
    (0 until config.phtNum).map(j => {
        pPHT(j).write(
            address = updatePhtIdxSatge2(j),
            data = PHTBundle(config).setVal(updateWdataPhtCounter(j), updatePhtTagSatge2(j), updateWdataPhtUseful(j)),
            enable = updateWenPhtCounter(j) | updateWenPhtTag(j) | updateWenPhtUseful(j), 
            mask = Bits(config.phtCounterWidth bits).setAllTo(updateWenPhtCounter(j)) ## Bits(config.phtTagWidth bits).setAllTo(updateWenPhtTag(j)) ## Bits(config.phtUsefulWidth bits).setAllTo(updateWenPhtUseful(j))
        )
        uPHT(j).write(
            address = updatePhtIdxSatge2(j),
            data = PHTBundle(config).setVal(updateWdataPhtCounter(j), updatePhtTagSatge2(j), updateWdataPhtUseful(j)),
            enable = updateWenPhtCounter(j) | updateWenPhtTag(j) | updateWenPhtUseful(j), 
            mask = Bits(config.phtCounterWidth bits).setAllTo(updateWenPhtCounter(j)) ## Bits(config.phtTagWidth bits).setAllTo(updateWenPhtTag(j)) ## Bits(config.phtUsefulWidth bits).setAllTo(updateWenPhtUseful(j))
        )
    })
    pBTB.write(updateBtbIdxSatge2(firstWriteIdxStage2), updateWdataBtb, updateWenBtb)
    (0 until config.retireWidth).map(i => {
        uBTB(i).write(updateBtbIdxSatge2(i), updateWdataBtb, updateWenBtb)
    })

}
