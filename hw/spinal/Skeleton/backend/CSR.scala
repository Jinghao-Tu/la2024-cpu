package Skeleton.backend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class CSR(config: CPUConfig) extends Component {
    val io = new Bundle {
        val extInt = in(Bits(Defs.extInterruptNum bits))
        val interrupt = out(Bool())
        val plv = out(CSRBundle(config).crmd.plv)
        val counter = slave(CounterReadBundle(config))
        val swRead = slave(CSRSwIOBundle(false, config))
        val swWrite = slave(CSRSwIOBundle(true, config))
        val llBitComm = slave(LLBitBundle(config))
        val badvICache = slave(BADVBundle(false, config))
        val badvDCache = slave(BADVBundle(true, config))
        val tlbCSRInfo = slave(TLBCSRInfo(config))
        val tlbCSRWrite = slave(TLBCSRWrite(config))
        val ctrl = slave(ROBCSRBundle(config))
        val flush = in(Bool())
    }

    val crmd = Reg(CSRBundle(config).crmd)
    val prmd = Reg(CSRBundle(config).prmd)
    val ecfg = Reg(CSRBundle(config).ecfg)
    val estat = Reg(CSRBundle(config).estat)
    val era = Reg(CSRBundle(config).era)
    val badv = Reg(CSRBundle(config).badv)
    val eentry = Reg(CSRBundle(config).eentry)
    val tlbidx = Reg(CSRBundle(config).tlbidx)
    val tlbehi = Reg(CSRBundle(config).tlbehi)
    val tlbelo0 = Reg(CSRBundle(config).tlbelo)
    val tlbelo1 = Reg(CSRBundle(config).tlbelo)
    val asid = Reg(CSRBundle(config).asid)
    val pgdl = Reg(CSRBundle(config).pgd)
    val pgdh = Reg(CSRBundle(config).pgd)
    val save0 = Reg(CSRBundle(config).save)
    val save1 = Reg(CSRBundle(config).save)
    val save2 = Reg(CSRBundle(config).save)
    val save3 = Reg(CSRBundle(config).save)
    val tid = Reg(CSRBundle(config).tid)
    val tcfg = Reg(CSRBundle(config).tcfg)
    val tval = Reg(CSRBundle(config).tval)
    val ticlr = Bool()
    val llbctl = Reg(CSRBundle(config).llbctl)
    val tlbrentry = Reg(CSRBundle(config).tlbrentry)
    val dmw0 = Reg(CSRBundle(config).dmw)
    val dmw1 = Reg(CSRBundle(config).dmw)
    crmd.init(CSRBundle(config).resetVal.crmd).allowUnsetRegToAvoidLatch
    prmd.init(CSRBundle(config).resetVal.prmd).allowUnsetRegToAvoidLatch
    ecfg.init(CSRBundle(config).resetVal.ecfg).allowUnsetRegToAvoidLatch
    estat.init(CSRBundle(config).resetVal.estat).allowUnsetRegToAvoidLatch
    era.init(CSRBundle(config).resetVal.era).allowUnsetRegToAvoidLatch
    badv.init(CSRBundle(config).resetVal.badv).allowUnsetRegToAvoidLatch
    eentry.init(CSRBundle(config).resetVal.eentry).allowUnsetRegToAvoidLatch
    tlbidx.init(CSRBundle(config).resetVal.tlbidx).allowUnsetRegToAvoidLatch
    tlbehi.init(CSRBundle(config).resetVal.tlbehi).allowUnsetRegToAvoidLatch
    tlbelo0.init(CSRBundle(config).resetVal.tlbelo).allowUnsetRegToAvoidLatch
    tlbelo1.init(CSRBundle(config).resetVal.tlbelo).allowUnsetRegToAvoidLatch
    asid.init(CSRBundle(config).resetVal.asid).allowUnsetRegToAvoidLatch
    pgdl.init(CSRBundle(config).resetVal.pgd).allowUnsetRegToAvoidLatch
    pgdh.init(CSRBundle(config).resetVal.pgd).allowUnsetRegToAvoidLatch
    save0.init(CSRBundle(config).resetVal.save).allowUnsetRegToAvoidLatch
    save1.init(CSRBundle(config).resetVal.save).allowUnsetRegToAvoidLatch
    save2.init(CSRBundle(config).resetVal.save).allowUnsetRegToAvoidLatch
    save3.init(CSRBundle(config).resetVal.save).allowUnsetRegToAvoidLatch
    tid.init(CSRBundle(config).resetVal.tid).allowUnsetRegToAvoidLatch
    tcfg.init(CSRBundle(config).resetVal.tcfg).allowUnsetRegToAvoidLatch
    tval.init(CSRBundle(config).resetVal.tval).allowUnsetRegToAvoidLatch
    llbctl.init(CSRBundle(config).resetVal.llbctl).allowUnsetRegToAvoidLatch
    tlbrentry.init(CSRBundle(config).resetVal.tlbrentry).allowUnsetRegToAvoidLatch
    dmw0.init(CSRBundle(config).resetVal.dmw).allowUnsetRegToAvoidLatch
    dmw1.init(CSRBundle(config).resetVal.dmw).allowUnsetRegToAvoidLatch

    val stableCounter = Counter(64 bits)
    stableCounter.increment()
    ticlr := False // Unless written 1 to TICLR.CLR, which is handled below
    
    val csrWriteBuffer = Reg(CSRWriteBufferBundle(config))
    val csrWriteBufferLock = Reg(Bool())
    csrWriteBuffer.init(CSRWriteBufferBundle(config).resetVal)
    csrWriteBufferLock.init(False)

    val timerNext = CSRBundle(config).tval.timeval
    val timeUp = tval.timeval === 0 && tcfg.en
    when (io.ctrl.writeCSR && csrWriteBuffer.address === CSRCoding.TCFG) {
        timerNext := csrWriteBuffer.value.as(CSRBundle(config).tcfg).initval ## B(0, 2 bits) // Not mentioned in LA32R handbook, but OpenLA500 uses this approach
    } otherwise {
        when (timeUp) {
            when (tcfg.periodic) {
                timerNext := tcfg.initval ## B(0, 2 bits)
            } otherwise {
                timerNext.setAll
                tcfg.en := False // This will be overrided when CSR write to TCFG happened to be happened in the same cycle
            }
        } otherwise {
            timerNext := (tval.timeval.asUInt - tcfg.en.asUInt.resized).asBits
        }
    }

    tval.timeval := timerNext

    estat.isHw := io.extInt
    estat.isTI := (timeUp || estat.isTI) && ~ticlr

    val intVec = Bits(Defs.interruptNum bits)
    intVec := estat.asBits(15 downto 0) & ecfg.asBits(15 downto 0)

    when (io.swWrite.wen && ~csrWriteBufferLock) {
        csrWriteBuffer.value := io.swWrite.value
        csrWriteBuffer.address := io.swWrite.address
    }
    when (io.swWrite.wen && ~io.flush) {
        csrWriteBufferLock := True
    }
    when (io.flush) {
        csrWriteBufferLock := False
    }

    val llAddr = Reg(Bits(config.palen bits))
    val llbUpdateBuffer = Reg(Bits(config.palen bits))
    val llbUpdateBufferLock = Reg(Bool())
    llAddr.init(B(0).resized)
    llbUpdateBuffer.init(B(0).resized)
    llbUpdateBufferLock.init(False)
    when (io.llBitComm.wen && ~llbUpdateBufferLock) {
        llbUpdateBuffer := io.llBitComm.toUpdateAddr
    }
    when (io.llBitComm.wen && ~io.flush) {
        llbUpdateBufferLock := True
    }
    when (io.flush) {
        llbUpdateBufferLock := False
    }
    when (io.ctrl.llBitUpdate) {
        llAddr := llbUpdateBuffer
    }

    val badvICacheBuffer = Reg(Bits(config.valen bits))
    val badvICacheBufferLock = Reg(Bool())
    badvICacheBuffer.init(B(0).resized)
    badvICacheBufferLock.init(False)
    when (io.badvICache.wen && ~badvICacheBufferLock) {
        badvICacheBuffer := io.badvICache.vaddr
    }
    when (io.badvICache.wen && ~io.flush) {
        badvICacheBufferLock := True
    }
    when (io.flush) {
        badvICacheBufferLock := False
    }

    val badvDCacheROBIdx = Reg(Bits(config.robIdxWidth bits))
    val badvDCacheBuffer = Reg(Bits(config.valen bits))
    val badvDCacheBufferLock = Reg(Bool())
    badvDCacheROBIdx.init(B(0).resized)
    badvDCacheBuffer.init(B(0).resized)
    badvDCacheBufferLock.init(False)
    when (io.badvDCache.wen && ~badvDCacheBufferLock) {
        badvDCacheROBIdx := io.badvDCache.robIdx
        badvDCacheBuffer := io.badvDCache.vaddr
    }
    when (io.badvDCache.wen && ~io.flush) {
        badvDCacheBufferLock := True
    }
    when (io.flush) {
        badvDCacheBufferLock := False
    }

    switch(io.swRead.address) {
        is(CSRCoding.CRMD) { io.swRead.value := crmd.asBits }
        is(CSRCoding.PRMD) { io.swRead.value := prmd.asBits }
        is(CSRCoding.ECFG) { io.swRead.value := ecfg.asBits }
        is(CSRCoding.ESTAT) { io.swRead.value := estat.asBits }
        is(CSRCoding.ERA) { io.swRead.value := era.asBits }
        is(CSRCoding.BADV) { io.swRead.value := badv.asBits }
        is(CSRCoding.EENTRY) { io.swRead.value := eentry.asBits }
        is(CSRCoding.TLBIDX) { io.swRead.value := tlbidx.asBits }
        is(CSRCoding.TLBEHI) { io.swRead.value := tlbehi.asBits }
        is(CSRCoding.TLBELO0) { io.swRead.value := tlbelo0.asBits }
        is(CSRCoding.TLBELO1) { io.swRead.value := tlbelo1.asBits }
        is(CSRCoding.ASID) { io.swRead.value := asid.asBits }
        is(CSRCoding.PGDL) { io.swRead.value := pgdl.asBits }
        is(CSRCoding.PGDH) { io.swRead.value := pgdh.asBits }
        is(CSRCoding.PGD) { io.swRead.value := Mux(badv.asBits(config.wordLength-1), pgdh.asBits, pgdl.asBits) }
        is(CSRCoding.CPUID) { io.swRead.value := B(0).resized }
        is(CSRCoding.SAVE0) { io.swRead.value := save0.asBits }
        is(CSRCoding.SAVE1) { io.swRead.value := save1.asBits }
        is(CSRCoding.SAVE2) { io.swRead.value := save2.asBits }
        is(CSRCoding.SAVE3) { io.swRead.value := save3.asBits }
        is(CSRCoding.TID) { io.swRead.value := tid.asBits }
        is(CSRCoding.TCFG) { io.swRead.value := tcfg.asBits }
        is(CSRCoding.TVAL) { if (config.timerWidth!=32) {
                                io.swRead.value := (B(0).resized ## tval.timeval.asBits) 
                             } else {
                                io.swRead.value := tval.timeval.asBits
                             } }
        is(CSRCoding.TICLR) { io.swRead.value := B(0).resized }
        is(CSRCoding.LLBCTL) { io.swRead.value := llbctl.asBits }
        is(CSRCoding.TLBRENTRY) { io.swRead.value := tlbrentry.asBits }
        is(CSRCoding.DMW0) { io.swRead.value := dmw0.asBits }
        is(CSRCoding.DMW1) { io.swRead.value := dmw1.asBits }
        default { io.swRead.value := B(0).resized }
    }
    when (io.ctrl.writeCSR) {
        switch(csrWriteBuffer.address) {
            is(CSRCoding.CRMD) {
                crmd.plv  := csrWriteBuffer.value.as(CSRBundle(config).crmd).plv
                crmd.ie   := csrWriteBuffer.value.as(CSRBundle(config).crmd).ie
                crmd.da   := csrWriteBuffer.value.as(CSRBundle(config).crmd).da
                crmd.pg   := csrWriteBuffer.value.as(CSRBundle(config).crmd).pg
                crmd.datf := csrWriteBuffer.value.as(CSRBundle(config).crmd).datf
                crmd.datm := csrWriteBuffer.value.as(CSRBundle(config).crmd).datm
            }
            is(CSRCoding.PRMD) {
                prmd.pplv := csrWriteBuffer.value.as(CSRBundle(config).prmd).pplv
                prmd.pie := csrWriteBuffer.value.as(CSRBundle(config).prmd).pie
            }
            is(CSRCoding.ECFG) {
                ecfg.lieLo := csrWriteBuffer.value.as(CSRBundle(config).ecfg).lieLo
                ecfg.lieHi := csrWriteBuffer.value.as(CSRBundle(config).ecfg).lieHi
            }
            is(CSRCoding.ESTAT) {
                estat.isSw := csrWriteBuffer.value.as(CSRBundle(config).estat).isSw
            }
            is(CSRCoding.ERA) {
                era.pc := csrWriteBuffer.value.as(CSRBundle(config).era).pc
            }
            is(CSRCoding.BADV) {
                badv.vaddr := csrWriteBuffer.value.as(CSRBundle(config).badv).vaddr
            }
            is(CSRCoding.EENTRY) {
                eentry.va := csrWriteBuffer.value.as(CSRBundle(config).eentry).va
            }
            is(CSRCoding.TLBIDX) {
                tlbidx.index := csrWriteBuffer.value.as(CSRBundle(config).tlbidx).index
                tlbidx.ps := csrWriteBuffer.value.as(CSRBundle(config).tlbidx).ps
                tlbidx.ne := csrWriteBuffer.value.as(CSRBundle(config).tlbidx).ne
            }
            is(CSRCoding.TLBEHI) {
                tlbehi.vppn := csrWriteBuffer.value.as(CSRBundle(config).tlbehi).vppn
            }
            is(CSRCoding.TLBELO0) {
                tlbelo0.v := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).v
                tlbelo0.d := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).d
                tlbelo0.plv := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).plv
                tlbelo0.mat := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).mat
                tlbelo0.g := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).g
                tlbelo0.ppn := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).ppn
            }
            is(CSRCoding.TLBELO1) {
                tlbelo1.v := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).v
                tlbelo1.d := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).d
                tlbelo1.plv := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).plv
                tlbelo1.mat := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).mat
                tlbelo1.g := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).g
                tlbelo1.ppn := csrWriteBuffer.value.as(CSRBundle(config).tlbelo).ppn
            }
            is(CSRCoding.ASID) {
                asid.asid := csrWriteBuffer.value.as(CSRBundle(config).asid).asid
            }
            is(CSRCoding.PGDL) {
                pgdl.base := csrWriteBuffer.value.as(CSRBundle(config).pgd).base
            }
            is(CSRCoding.PGDH) {
                pgdh.base := csrWriteBuffer.value.as(CSRBundle(config).pgd).base
            }
            is(CSRCoding.SAVE0) {
                save0.data := csrWriteBuffer.value.as(CSRBundle(config).save).data
            }
            is(CSRCoding.SAVE1) {
                save1.data := csrWriteBuffer.value.as(CSRBundle(config).save).data
            }
            is(CSRCoding.SAVE2) {
                save2.data := csrWriteBuffer.value.as(CSRBundle(config).save).data
            }
            is(CSRCoding.SAVE3) {
                save3.data := csrWriteBuffer.value.as(CSRBundle(config).save).data
            }
            is(CSRCoding.TID) {
                tid.tid := csrWriteBuffer.value.as(CSRBundle(config).tid).tid
            }
            is(CSRCoding.TCFG) {
                tcfg.en       := csrWriteBuffer.value.as(CSRBundle(config).tcfg).en
                tcfg.periodic := csrWriteBuffer.value.as(CSRBundle(config).tcfg).periodic
                tcfg.initval  := csrWriteBuffer.value.as(CSRBundle(config).tcfg).initval
            }
            is(CSRCoding.TICLR) {
                ticlr := csrWriteBuffer.value(0)
            }
            is(CSRCoding.LLBCTL) {
                llbctl.klo := csrWriteBuffer.value.as(CSRBundle(config).llbctl).klo
                when (csrWriteBuffer.value(1)) {
                    llbctl.rollb := False
                }
            }
            is(CSRCoding.TLBRENTRY) {
                tlbrentry.pa := csrWriteBuffer.value.as(CSRBundle(config).tlbrentry).pa
            }
            is(CSRCoding.DMW0) {
                dmw0.plv0 := csrWriteBuffer.value.as(CSRBundle(config).dmw).plv0
                dmw0.plv3 := csrWriteBuffer.value.as(CSRBundle(config).dmw).plv3
                dmw0.mat  := csrWriteBuffer.value.as(CSRBundle(config).dmw).mat
                dmw0.pseg := csrWriteBuffer.value.as(CSRBundle(config).dmw).pseg
                dmw0.vseg := csrWriteBuffer.value.as(CSRBundle(config).dmw).vseg
            }
            is(CSRCoding.DMW1) {
                dmw1.plv0 := csrWriteBuffer.value.as(CSRBundle(config).dmw).plv0
                dmw1.plv3 := csrWriteBuffer.value.as(CSRBundle(config).dmw).plv3
                dmw1.mat  := csrWriteBuffer.value.as(CSRBundle(config).dmw).mat
                dmw1.pseg := csrWriteBuffer.value.as(CSRBundle(config).dmw).pseg
                dmw1.vseg := csrWriteBuffer.value.as(CSRBundle(config).dmw).vseg
            }
        }
    }
    when (io.ctrl.ertn) {
        crmd.plv := prmd.pplv
        crmd.ie := prmd.pie
        crmd.da := (estat.ecode === ECode.TLBR.eCode) ? False | crmd.da
        crmd.pg := (estat.ecode === ECode.TLBR.eCode) ? True | crmd.pg
        llbctl.rollb := llbctl.klo ? llbctl.rollb | False
        llbctl.klo := False
    }
    when (io.ctrl.normalException || io.ctrl.tlbrException) {
        prmd.pplv := crmd.plv
        prmd.pie := crmd.ie
        crmd.plv := B(0).resized
        crmd.ie := False
        era.pc := io.ctrl.epc.asBits
        estat.ecode := io.ctrl.eCode
        estat.esubcode := io.ctrl.eSubCode.resized

        when ((io.ctrl.eCode === ECode.TLBR.eCode && io.ctrl.eSubCode === ECode.TLBR.eSubCode) ||
              (io.ctrl.eCode ===  ECode.PIL.eCode && io.ctrl.eSubCode ===  ECode.PIL.eSubCode) ||
              (io.ctrl.eCode ===  ECode.PIS.eCode && io.ctrl.eSubCode ===  ECode.PIS.eSubCode) ||
              (io.ctrl.eCode ===  ECode.PIF.eCode && io.ctrl.eSubCode ===  ECode.PIF.eSubCode) ||
              (io.ctrl.eCode ===  ECode.PME.eCode && io.ctrl.eSubCode ===  ECode.PME.eSubCode) ||
              (io.ctrl.eCode ===  ECode.PPI.eCode && io.ctrl.eSubCode ===  ECode.PPI.eSubCode)) {
                // BADV AND TLBEHI
                when (io.ctrl.eROBIdx === badvDCacheROBIdx && badvDCacheBufferLock) {
                    badv.vaddr := badvDCacheBuffer
                    tlbehi.vppn := badvDCacheBuffer(13, config.valen-13 bits)
                } otherwise {
                    badv.vaddr := badvICacheBuffer
                }
        }
        when ((io.ctrl.eCode === ECode.ADEF.eCode && io.ctrl.eSubCode === ECode.ADEF.eSubCode) ||
              (io.ctrl.eCode ===  ECode.ALE.eCode && io.ctrl.eSubCode ===  ECode.ALE.eSubCode)) {
                when (io.ctrl.eROBIdx === badvDCacheROBIdx && badvDCacheBufferLock) {
                    badv.vaddr := badvDCacheBuffer
                } otherwise {
                    badv.vaddr := badvICacheBuffer
                }
              }

        when (io.ctrl.tlbrException) {
            crmd.da := True
            crmd.pg := False
        }
    }

    when (io.tlbCSRWrite.idxWen) {
        tlbidx := io.tlbCSRWrite.tlbidx
    }
    when (io.tlbCSRWrite.entryWen) {
        tlbehi := io.tlbCSRWrite.tlbehi
        tlbelo0 := io.tlbCSRWrite.tlbelo0
        tlbelo1 := io.tlbCSRWrite.tlbelo1
        asid.asid := io.tlbCSRWrite.asid
    }

    io.interrupt := crmd.ie && intVec.orR
    io.plv := crmd.plv
    io.counter.id := tid.tid.asUInt
    io.counter.value := stableCounter.value
    io.llBitComm.actualAddr := llAddr
    io.llBitComm.llBit := llbctl.rollb
    io.tlbCSRInfo.asid := asid.asid
    io.tlbCSRInfo.plv := crmd.plv
    io.tlbCSRInfo.da := crmd.da
    io.tlbCSRInfo.pg := crmd.pg
    io.tlbCSRInfo.datf := crmd.datf
    io.tlbCSRInfo.datm := crmd.datm
    io.tlbCSRInfo.dmw0 := dmw0
    io.tlbCSRInfo.dmw1 := dmw1
    io.tlbCSRInfo.ecode := estat.ecode
    io.tlbCSRInfo.tlbidx := tlbidx
    io.tlbCSRInfo.tlbehi := tlbehi
    io.tlbCSRInfo.tlbelo0 := tlbelo0
    io.tlbCSRInfo.tlbelo1 := tlbelo1
    io.ctrl.era := era.pc.asUInt
    io.ctrl.eentry := eentry.asBits.asUInt
    io.ctrl.tlbrentry := tlbrentry.asBits.asUInt
}

case class CSRWriteBufferBundle(config: CPUConfig) extends Bundle {
    val value = Bits(config.wordLength bits)
    val address = Bits(config.csrAddrLength bits)

    def resetVal: CSRWriteBufferBundle = {
        val value = CSRWriteBufferBundle(config)
        value.value := B(0).resized
        value.address := B(0).resized
        return value
    }
}

object CSRCoding {
    def CRMD      = B("000".asHex)
    def PRMD      = B("001".asHex)
    def EUEN      = B("002".asHex) // Not implemented
    def ECFG      = B("004".asHex)
    def ESTAT     = B("005".asHex)
    def ERA       = B("006".asHex)
    def BADV      = B("007".asHex)
    def EENTRY    = B("00C".asHex)
    def TLBIDX    = B("010".asHex)
    def TLBEHI    = B("011".asHex)
    def TLBELO0   = B("012".asHex)
    def TLBELO1   = B("013".asHex)
    def ASID      = B("018".asHex)
    def PGDL      = B("019".asHex)
    def PGDH      = B("01A".asHex)
    def PGD       = B("01B".asHex)
    def CPUID     = B("020".asHex)
    def SAVE0     = B("030".asHex)
    def SAVE1     = B("031".asHex)
    def SAVE2     = B("032".asHex)
    def SAVE3     = B("033".asHex)
    def TID       = B("040".asHex)
    def TCFG      = B("041".asHex)
    def TVAL      = B("042".asHex)
    def TICLR     = B("044".asHex)
    def LLBCTL    = B("060".asHex)
    def TLBRENTRY = B("088".asHex)
    def CTAG      = B("098".asHex) // Not implemented
    def DMW0      = B("180".asHex)
    def DMW1      = B("181".asHex)
}