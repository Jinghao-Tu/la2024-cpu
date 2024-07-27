package Skeleton.fu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import Skeleton.bundle._
import Skeleton.config._

case class DCache(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(FUType.lsu, config)) // 1-latency
        val output = master Stream(FUWBBundle(config))
        val wakeOut = Vec.fill(2)(master(Flow(Bits(config.prfIdxWidth bits)))) // 0-latency!
        val retireComm = master(LSUROBBundle(config))
        val tlb = master(TLBRequestBundle(config))
        val plv = in(CSRBundle(config).crmd.plv)
        val llBitComm = master(LLBitBundle(config))
        val ctrl = slave(CacheCtrlBundle(config))
        val specialOpBufferUpdate = master Flow(SpecialOpBufferUpdateBundle(config)) // 0-latency!
        val flush = in(Bool())
        val badv = master(BADVBundle(true, config))
        val axi = master(AXIBundle(true, config))
    }
    // AGU
    val address = io.input.payload.src1 + io.input.payload.src2
    // D-Cache
    val data = Array.fill(config.dCacheWaySize)(Mem(Bits(config.axiDataWidth bits), config.dCacheSizePerWay))
    val tag = Array.fill(config.dCacheWaySize)(Mem(Bits(config.dCacheTagWidth bits), config.dCacheLineSize))
    /*(0 until config.dCacheWaySize).map(i => {
        data(i).generateAsBlackBox()
        tag(i).generateAsBlackBox()
    })*/
    val valid = Array.fill(config.dCacheWaySize)(Vec.fill(config.dCacheLineSize)(Reg(Bool())))
    val dirty = Array.fill(config.dCacheWaySize)(Vec.fill(config.dCacheLineSize)(Reg(Bool())))
    valid.foreach(_.foreach(_ init(False)))
    dirty.foreach(_.foreach(_ init(False)))
    val dataRead = Vec.fill(config.dCacheWaySize)(Bits(config.axiDataWidth bits))
    val tagRead = Vec.fill(config.dCacheWaySize)(Bits(config.dCacheTagWidth bits))
    val hit = Bits(config.dCacheWaySize bits)
    val miss = Bool()
    val missBufferAvail = Bool()
    val wayToReplace = Bits(config.dCacheWaySize bits)
    val realLSMask = Bits(config.axiDataWidth / 8 bits)
    val stall = Bool()
    val cacopActive = Bool()
    val cacopEn = io.ctrl.cacopStoreTag | io.ctrl.cacopIndexInvalidate | io.ctrl.cacopHitInvalidate
    val rollingBack = Bool()
    val writeBufferAvail = Bool()
    val writeBufferAppend = Bool()
    val writeBufferUpdate = Bool()
    val refilling = Bool()
    val exceptionInfo = ExceptionInfo()
    val exceptionInfo1 = ExceptionInfo()
    val exceptionInfo2 = ExceptionInfo()
    
    val wayDirty = Bits(config.dCacheWaySize bits)
    val wayDirtyBypass = Reg(Bits(config.dCacheWaySize bits))
    wayDirtyBypass.init(0)
    
    val stage1Out = Stream(DCachePipelineBundle(config))
    val stage2In = Stream(DCachePipelineBundle(config))
    stage2In <-< stage1Out.throwWhen(io.flush)
    stage1Out.valid := (io.input.valid & ~stall) | cacopEn
    val preShiftSize = address(log2Up(config.wordLength / 8)-1 downto 0)
    io.tlb.virtPageNumber := Mux(io.ctrl.cacopHitInvalidate, io.ctrl.cacopVA(config.valen-1 downto 12).asBits, address(config.valen-1 downto 12).asBits)

    io.input.ready := (stage1Out.ready & ~stall) || io.flush
    stage1Out.payload.tlb.hit := io.tlb.hit
    stage1Out.payload.tlb.pageInfo := io.tlb.pageInfo
    stage1Out.payload.isStoreTag := io.ctrl.cacopStoreTag
    stage1Out.payload.isIndexInvalidate := io.ctrl.cacopIndexInvalidate
    stage1Out.payload.isHitInvalidate := io.ctrl.cacopHitInvalidate
    stage1Out.payload.robIdx := io.input.payload.robIdx
    stage1Out.payload.prd := io.input.payload.prd
    stage1Out.payload.branchInfo := io.input.payload.branchInfo
    stage1Out.payload.branchResult := io.input.payload.branchResult
    stage1Out.payload.exceptionInfo := Mux(io.input.exceptionInfo.exception, io.input.payload.exceptionInfo, exceptionInfo1)
    stage1Out.payload.storeData := (io.input.payload.src3 |<< (preShiftSize @@ U(0, 3 bits))).asBits
    stage1Out.payload.vaddr := Mux(cacopEn, io.ctrl.cacopVA, address)

    // Normal Load/Store decode
    stage1Out.payload.lsCtrlBundle.load := io.input.payload.uop.lsuOp === LSUOp.ld || io.input.payload.uop.lsuOp === LSUOp.ldu || io.input.payload.uop.lsuOp === LSUOp.ll
    stage1Out.payload.lsCtrlBundle.store := io.input.payload.uop.lsuOp === LSUOp.st || io.input.payload.uop.lsuOp === LSUOp.sc
    stage1Out.payload.lsCtrlBundle.signed := io.input.payload.uop.lsuOp === LSUOp.ld
    stage1Out.payload.lsCtrlBundle.ll := io.input.payload.uop.lsuOp === LSUOp.ll
    stage1Out.payload.lsCtrlBundle.sc := io.input.payload.uop.lsuOp === LSUOp.sc
    stage1Out.payload.lsCtrlBundle.lsMask := io.input.payload.uop.lsuCoOp((config.wordLength / 8) - 1 downto 0) |<< preShiftSize
    stage1Out.payload.lsCtrlBundle.normalMemOp := (stage1Out.payload.lsCtrlBundle.load || stage1Out.payload.lsCtrlBundle.store) && ~io.ctrl.stall // Inhibit most of stage 2 actions
    switch (io.input.payload.uop.lsuCoOp((config.wordLength / 8) - 1 downto 0)) {
        is (LSUSizeOp.byte.asBits)     { stage1Out.payload.lsCtrlBundle.size := B(0).resized }
        is (LSUSizeOp.halfword.asBits) { stage1Out.payload.lsCtrlBundle.size := B(1).resized }
        is (LSUSizeOp.word.asBits)     { stage1Out.payload.lsCtrlBundle.size := B(2).resized }
        default                        { stage1Out.payload.lsCtrlBundle.size := B(2).resized }
    }
    val normalMemOp = stage2In.payload.lsCtrlBundle.normalMemOp
    stage1Out.payload.checkTLBException := stage1Out.payload.lsCtrlBundle.normalMemOp || (io.input.payload.uop.lsuOp === LSUOp.cacop && io.input.payload.uop.lsuCoOp(4 downto 3) === B(2).resized)
    stage1Out.payload.lsException := ~io.input.exceptionInfo.exception

    val transferRAddrHi = Reg(Bits(config.palen-config.dCacheOffsetWidth bits))
    val transferRAddrMid = Reg(Bits(config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits))
    val transferRAddrLo = Reg(Bits(config.dCacheBlockOffsetWidth bits))
    val transferRAddr = transferRAddrHi ## transferRAddrMid ## transferRAddrLo
    val transferWAddrHi = Reg(Bits(config.palen-config.dCacheOffsetWidth bits))
    val transferWAddrMid = Reg(Bits(config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits))
    val transferWAddrLo = Reg(Bits(config.dCacheBlockOffsetWidth bits))
    val transferWAddr = transferWAddrHi ## transferWAddrMid ## transferWAddrLo
    val transferUncached = Reg(Bool())
    val transferCACOP = Reg(Bool())
    val transferWriteBufferIdx = Reg(UInt(log2Up(config.dCacheWriteBufferSize) bits)) // Reserved for shortening possible critical path
    val transferWData = Reg(Bits(config.axiDataWidth bits)) // This is needed for a quick continue on uncached store
    val transferLSMask = Reg(Bits(config.axiDataWidth / 8 bits))
    val transferWaySelect = Reg(Bits(config.dCacheWaySize bits))
    val axiFillPriority = io.axi.rFire & ~transferUncached
    val axiWritebackPriority = (io.axi.awFire || io.axi.wFire) && ~transferUncached && ~io.axi.wlast
    transferRAddrHi.init(B(0).resized)
    transferRAddrMid.init(B(0).resized)
    transferRAddrLo.init(B(0).resized)
    transferWAddrHi.init(B(0).resized)
    transferWAddrMid.init(B(0).resized)
    transferWAddrLo.init(B(0).resized)
    transferUncached.init(False)
    transferCACOP.init(False)
    transferWriteBufferIdx.init(U(0).resized)
    transferWData.init(B(0).resized)
    transferLSMask.init(B(0).resized)
    transferWaySelect.init(B(0).resized)

    // Port 0: Stage 1 Load, rollback(high priority)
    // Port 1: Stage 2 Store, refill(high priority)
    val portAddr0 = UInt(log2Up(config.dCacheSizePerWay) bits)
    val portRData0 = Vec.fill(config.dCacheWaySize)(Bits(config.axiDataWidth bits))
    val portRData0Raw = Vec.fill(config.dCacheWaySize)(Bits(config.axiDataWidth bits))
    val portWData0 = Bits(config.axiDataWidth bits)
    val portWMask0 = Bits(config.axiDataWidth/8 bits)
    val portRen0 = Vec.fill(config.dCacheWaySize)(Bool())
    val portWen0 = Vec.fill(config.dCacheWaySize)(Bool())
    val portAddr1 = UInt(log2Up(config.dCacheSizePerWay) bits)
    val portRData1 = Vec.fill(config.dCacheWaySize)(Bits(config.axiDataWidth bits))
    val portWData1 = Bits(config.axiDataWidth bits)
    val portWMask1 = Bits(config.axiDataWidth/8 bits)
    val portRen1 = Vec.fill(config.dCacheWaySize)(Bool())
    val portWen1 = Vec.fill(config.dCacheWaySize)(Bool())
    val portWData1Bypass = Vec.fill(config.dCacheWaySize)(Reg(Bits(config.axiDataWidth bits)))
    val portWMask1Bypass = Vec.fill(config.dCacheWaySize)(Reg(Bits(config.axiDataWidth/8 bits)))
    val writebackBufferAvail = Reg(Bool())
    val writebackDataAvail = Reg(Bool())
    writebackBufferAvail.init(False)
    writebackDataAvail.init(False)
    when (axiWritebackPriority) {
        writebackDataAvail := True
    } elsewhen (writebackDataAvail) { // Priority matters here
        writebackDataAvail := False
    }
    when (io.axi.wFire) {
        writebackBufferAvail := False
    } elsewhen (writebackDataAvail) { // Priority matters here
        writebackBufferAvail := True
    }

    val missingEntry = DCacheMissBufferEntryBundle(config)
    val latestWrite = DCacheWriteBufferEntryBundle(config)
    val mergedWrite = Bits(config.axiDataWidth bits)
    (0 until config.axiDataWidth/8).map(i => {
        mergedWrite(i*8+7 downto i*8) := Mux(transferLSMask(i) && writeBufferUpdate, transferWData(i*8+7 downto i*8), io.axi.rdata(i*8+7 downto i*8))
    })

    portAddr0 := Mux(rollingBack, latestWrite.index, getDataIdx(Mux(io.ctrl.cacopHitInvalidate, io.ctrl.cacopVA, address)))
    portWData0 := latestWrite.prevData
    portWMask0 := B"4'b1111"
    portRen0 := (stage1Out.ready #* config.dCacheWaySize).asBools | portWen0
    portWen0 := latestWrite.waySelect.asBools & (rollingBack #* config.dCacheWaySize).asBools & (~latestWrite.miss #* config.dCacheWaySize).asBools
    portAddr1 := Mux(axiFillPriority || axiWritebackPriority, Mux(axiFillPriority, getDataIdx(transferRAddr.asUInt), getDataIdx(transferWAddr.asUInt)), getDataIdx(stage2In.payload.vaddr))
    portWData1 := Mux(axiFillPriority, mergedWrite, stage2In.payload.storeData)
    portWMask1 := Mux(axiFillPriority, B"4'b1111", realLSMask)
    portRen1 := (axiWritebackPriority #* config.dCacheWaySize).asBools | portWen1
    portWen1 := (transferWaySelect.asBools & (axiFillPriority #* config.dCacheWaySize).asBools) | (hit.asBools & (~miss #* config.dCacheWaySize).asBools & (writeBufferAppend #* config.dCacheWaySize).asBools & (~io.flush #* config.dCacheWaySize).asBools)

    (0 until config.dCacheWaySize).map(i => {
        portWData1Bypass(i).init(B(0).resized)
        portWMask1Bypass(i).init(B(0).resized)
        (0 until config.axiDataWidth/8).map(j => {
            portRData0(i)(j*8+7 downto j*8) := Mux(portWMask1Bypass(i)(j), portWData1Bypass(i)(j*8+7 downto j*8), portRData0Raw(i)(j*8+7 downto j*8))
        })
        when (portRen0(i)) {
            when (portWen1(i) && (portAddr0 === portAddr1)) {
                portWData1Bypass(i) := portWData1
                portWMask1Bypass(i) := portWMask1
            } otherwise {
                portWData1Bypass(i) := B(0).resized
                portWMask1Bypass(i) := B(0).resized
            }
        }
    })

    (0 until config.dCacheWaySize).map(i => {
        stage1Out.payload.wayValid(i) := valid(i)(getBlockIdx(Mux(cacopEn, io.ctrl.cacopVA, address)))
        stage1Out.payload.wayDirty(i) := dirty(i)(getBlockIdx(Mux(cacopEn, io.ctrl.cacopVA, address)))
        dataRead(i) := portRData0(i)
        portRData0Raw(i) := data(i).readWriteSync(portAddr0, portWData0, portRen0(i), portWen0(i), portWMask0)
        portRData1(i) := data(i).readWriteSync(portAddr1, portWData1, portRen1(i), portWen1(i), portWMask1)
        tagRead(i) := tag(i).readSync(getBlockIdx(Mux(cacopEn, io.ctrl.cacopVA, address)), stage1Out.fire)
        hit(i) := stage2In.payload.wayValid(i) & (tagRead(i) === getTag(stage2In.payload.vaddr).asBits) & (stage2In.payload.tlb.pageInfo.mat === B(1).resized)
    })
    miss := stage2In.valid & normalMemOp & ~(hit.orR | exceptionInfo.exception) // Handles miss only when missed request has no exception
    
    exceptionInfo := Mux(stage2In.payload.exceptionInfo.exception || ~stage2In.payload.checkTLBException, stage2In.payload.exceptionInfo, exceptionInfo2)
    val hasException = Bool()
    val axiLoad = Bool()
    val axiFinish = Bool()
    val noStructuralHazard = ~axiLoad && ~((axiFillPriority || axiWritebackPriority) && stage2In.payload.lsCtrlBundle.store) && (~miss || missBufferAvail) && (~stage2In.payload.lsCtrlBundle.store || writeBufferAvail) // Inst in stage 2 is blocked when axi is sending data for a previously missed/uncached load, or when axi is sending data to fill the cache line with a store instruction in stage 2, or when a miss/uncached L/S meets full miss buffer, or when a cached store meets full write buffer
    axiFinish := False // Default to FALSE
    stage2In.ready := noStructuralHazard || io.flush
    when ((stage1Out.payload.lsCtrlBundle.load || stage1Out.payload.lsCtrlBundle.store) && 
          (io.input.payload.uop.lsuCoOp(config.wordLength / 8 - 1 downto 0) === LSUSizeOp.word.asBits && address(1 downto 0).orR) || (io.input.payload.uop.lsuCoOp(config.wordLength / 8 - 1 downto 0) === LSUSizeOp.halfword.asBits && address(0))) {
        exceptionInfo1.exception := True
        exceptionInfo1.eCode := ECode.ALE.eCode
        exceptionInfo1.eSubCode := ECode.ALE.eSubCode
    } otherwise {
        exceptionInfo1.exception := False
        exceptionInfo1.eCode := ECode.ALE.eCode
        exceptionInfo1.eSubCode := ECode.ALE.eSubCode
    }
    when (stage2In.payload.tlb.hit) {
        when (~stage2In.payload.tlb.pageInfo.v) {
            exceptionInfo2.exception := True
            exceptionInfo2.eCode := Mux(stage2In.payload.lsCtrlBundle.load, ECode.PIL.eCode, ECode.PIS.eCode)
            exceptionInfo2.eSubCode := Mux(stage2In.payload.lsCtrlBundle.load, ECode.PIL.eSubCode, ECode.PIS.eSubCode)
        } elsewhen (io.plv.asUInt > stage2In.payload.tlb.pageInfo.plv.asUInt) {
            exceptionInfo2.exception := True
            exceptionInfo2.eCode := ECode.PPI.eCode
            exceptionInfo2.eSubCode := ECode.PPI.eSubCode
        } elsewhen (stage2In.payload.lsCtrlBundle.store && ~stage2In.payload.tlb.pageInfo.d) {
            exceptionInfo2.exception := True
            exceptionInfo2.eCode := ECode.PME.eCode
            exceptionInfo2.eSubCode := ECode.PME.eSubCode
        } otherwise {
            exceptionInfo2.exception := False
            exceptionInfo2.eCode := ECode.PPI.eCode // Just to avoid latch detection
            exceptionInfo2.eSubCode := ECode.PPI.eSubCode // Just to avoid latch detection
        }
    } otherwise {
        exceptionInfo2.exception := True
        exceptionInfo2.eCode := ECode.TLBR.eCode
        exceptionInfo2.eSubCode := ECode.TLBR.eSubCode
    }

    // LL/SC related
    val scMatchHit = stage2In.payload.lsCtrlBundle.sc && (getTranslatedAddr(stage2In.payload.vaddr).asBits === io.llBitComm.actualAddr) && io.llBitComm.llBit
    val scMatchAXI = missingEntry.sc && (transferRAddr.asBits === io.llBitComm.actualAddr) && io.llBitComm.llBit // Note that this requires critical word FIRST, or transferRAddr won't match the actual missed addr
    val scResHit = UInt(config.wordLength bits)
    val scResAXI = UInt(config.wordLength bits)
    scResHit := (U(0, config.wordLength-1 bits) @@ (scMatchHit.asUInt))
    scResAXI := (U(0, config.wordLength-1 bits) @@ (scMatchAXI.asUInt))
    realLSMask := Mux(stage2In.payload.lsCtrlBundle.sc && ~scMatchHit, B(0).resized, stage2In.payload.lsCtrlBundle.lsMask)
    io.llBitComm.toUpdateAddr := getTranslatedAddr(stage2In.payload.vaddr).asBits
    io.llBitComm.wen := stage2In.payload.lsCtrlBundle.ll && stage2In.valid // New LL address could be write to LL buffer before inst commit when LL misses, but actual address update happens when LL retires, so it's legal

    // Write Buffer, for cached store
    val writeBufferRetireMask = Bits(config.dCacheWriteBufferSize bits)
    val writeBuffer = Vec.fill(config.dCacheWriteBufferSize)(Reg(DCacheWriteBufferEntryBundle(config)))
    val writeBufferHead = Reg(UInt(log2Up(config.dCacheWriteBufferSize) bits))
    val writeBufferTail = Reg(UInt(log2Up(config.dCacheWriteBufferSize) bits))
    val writeBufferHeadNext = writeBufferHead + CountOne(writeBufferRetireMask).resize(log2Up(config.dCacheWriteBufferSize) bits)
    writeBufferHead.init(U(0).resized)
    writeBufferTail.init(U(0).resized)
    latestWrite := writeBuffer(writeBufferTail)
    writeBuffer.foreach(_ init(DCacheWriteBufferEntryBundle(config).resetVal))
    writeBufferAvail := ~writeBuffer(writeBufferTail).valid  // Tail-chaining not supported
    (0 until config.dCacheWriteBufferSize).map(i => {
        val idxMatchMask = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(j => {
            idxMatchMask(j) := writeBuffer(i).robIdx === io.retireComm.robIdx(j) && io.retireComm.allowRetire(j)
        })
        writeBufferRetireMask(i) := idxMatchMask.orR && writeBuffer(i).valid
    })
    writeBufferHead := writeBufferHeadNext
    writeBufferAppend := stage2In.payload.lsCtrlBundle.store && (stage2In.payload.tlb.pageInfo.mat === B(1).resized) && stage2In.valid && ~exceptionInfo.exception && noStructuralHazard && ~cacopActive
    when (writeBufferAppend && ~io.flush) {
        writeBuffer(writeBufferTail).robIdx := stage2In.payload.robIdx
        writeBuffer(writeBufferTail).waySelect := Mux(miss, wayToReplace, hit)
        writeBuffer(writeBufferTail).prevData := MuxOH(hit, dataRead) // Missed store will be bypassed directly to writebuffer from axi
        writeBuffer(writeBufferTail).prevDirty := Mux(miss, False, MuxOH(hit, wayDirty.asBools))
        writeBuffer(writeBufferTail).index := getDataIdx(stage2In.vaddr)
        writeBuffer(writeBufferTail).miss := miss
    }
    when (writeBufferUpdate) {
        writeBuffer(missingEntry.writeBufferIdx).prevData := io.axi.rdata
        writeBuffer(missingEntry.writeBufferIdx).miss := False
    }
    (0 until config.dCacheWriteBufferSize).map(i => {
        writeBuffer(i).valid := (writeBufferRetireMask(i) || (rollingBack && writeBufferTail === i)) ? False | (writeBuffer(i).valid || (writeBufferAppend && (writeBufferTail === i) && ~io.flush))
    })

    // Let's handle cache miss/uncached now
    val missBuffer = Vec.fill(config.dCacheMissBufferSize)(Reg(DCacheMissBufferEntryBundle(config)))
    val missBufferHead = Reg(UInt(log2Up(config.dCacheMissBufferSize) bits))
    val missBufferTail = Reg(UInt(log2Up(config.dCacheMissBufferSize) bits))
    missBufferHead.init(U(0).resized)
    missBufferTail.init(U(0).resized)
    missingEntry := missBuffer(missBufferHead)
    missBuffer.foreach(_ init(DCacheMissBufferEntryBundle(config).resetVal))
    missBufferAvail := ~missBuffer(missBufferTail).valid // Tail-chaining not supported
    when (miss && stage2In.ready && ~io.flush) { // Miss or uncached, no structural hazard
        missBuffer(missBufferTail).robIdx := stage2In.payload.robIdx
        missBuffer(missBufferTail).prd := stage2In.payload.prd
        missBuffer(missBufferTail).branchInfo := stage2In.payload.branchInfo
        missBuffer(missBufferTail).branchResult := stage2In.payload.branchResult
        missBuffer(missBufferTail).exceptionInfo := exceptionInfo
        missBuffer(missBufferTail).uncached := stage2In.payload.tlb.pageInfo.mat === B(0).resized
        missBuffer(missBufferTail).load := stage2In.payload.lsCtrlBundle.load
        missBuffer(missBufferTail).store := stage2In.payload.lsCtrlBundle.store
        missBuffer(missBufferTail).signed := stage2In.payload.lsCtrlBundle.signed
        missBuffer(missBufferTail).ll := stage2In.payload.lsCtrlBundle.ll
        missBuffer(missBufferTail).sc := stage2In.payload.lsCtrlBundle.sc
        missBuffer(missBufferTail).writeBufferIdx := writeBufferTail
        missBuffer(missBufferTail).waySelect := wayToReplace
        missBuffer(missBufferTail).writeBack := MuxOH(wayToReplace, wayDirty.asBools)
        missBuffer(missBufferTail).storeData := stage2In.payload.storeData
        missBuffer(missBufferTail).lsMask := realLSMask
        missBuffer(missBufferTail).size := stage2In.payload.lsCtrlBundle.size
        missBuffer(missBufferTail).vaddr := stage2In.payload.vaddr
        missBuffer(missBufferTail).paddr := getTranslatedAddr(stage2In.payload.vaddr)
        missBuffer(missBufferTail).prevPaddr := getPrevAddr(stage2In.payload.vaddr)

        if (config.dCacheMissBufferSize > 1) missBufferTail := missBufferTail + 1
    }
    if (config.dCacheMissBufferSize > 1) {
        when (axiFinish) {
            missBufferHead := missBufferHead + 1
        }
    }
    when (io.flush) {
        if (config.dCacheMissBufferSize > 1) {
            missBufferTail := axiFinish ? (missBufferHead + 1) | missBufferHead // missBufferHead will increment by 1 on axiFinish, making missBufferTail sync with that
        } else {
            missBufferTail := missBufferHead
        }
        
    }
    val sameBlockMask = Bits(config.dCacheMissBufferSize+1 bits)
    (0 until config.dCacheMissBufferSize).map(i => {
        sameBlockMask(i) := missBuffer(i).valid && ~missBuffer(i).uncached && (getBlockIdx(missBuffer(i).vaddr) === getBlockIdx(address))
        missBuffer(i).valid := ((io.flush && ((missBufferHead =/= i) || ~refilling)) || (axiFinish && missBufferHead === i)) ? False | (missBuffer(i).valid || (miss && stage2In.ready && missBufferTail === i)) // This requires that missBuffer entries are served in order
    })
    sameBlockMask(config.dCacheMissBufferSize) := miss && ~(stage2In.payload.tlb.pageInfo.mat === B(0).resized) && (getBlockIdx(stage2In.payload.vaddr) === getBlockIdx(address))
    val sameBlock = sameBlockMask.orR && io.input.valid
    stall := io.ctrl.stall | sameBlock | rollingBack | io.axi.bready

    val lruBit = Vec.fill(config.dCacheLineSize)(Vec.fill(config.dCacheWaySize-1)(Reg(Bool()))) // Trick to make LRU array parameterizable
    lruBit.foreach(_.foreach(_ init(False))) // Initial to way 0

    when (io.output.fire && Mux(axiLoad, ~transferUncached, normalMemOp && stage2In.payload.tlb.pageInfo.mat === B(1).resized) && ~exceptionInfo.exception) {
        setLRUUpdate(Mux(axiLoad, getBlockIdx(missingEntry.vaddr), getBlockIdx(stage2In.payload.vaddr)), config.dCacheWaySize-1, 0)
    }
    val dirtyUpdate = io.output.fire && Mux(axiLoad, ~transferUncached && missingEntry.store, stage2In.payload.lsCtrlBundle.store && stage2In.payload.tlb.pageInfo.mat === B(1).resized) && ~exceptionInfo.exception
    when (dirtyUpdate) {
        (0 until config.dCacheWaySize).map(i => {
            when(Mux(axiLoad, transferWaySelect(i), hit(i))) {
                dirty(i)(Mux(axiLoad, getBlockIdx(transferRAddr.asUInt), getBlockIdx(stage2In.payload.vaddr))) := True
            }
        })
    }
    when (rollingBack) {
        (0 until config.dCacheWaySize).map(i => {
            when(latestWrite.waySelect(i)) {
                dirty(i)(latestWrite.index(config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth, config.dCacheIdxWidth bits)) := latestWrite.prevDirty
            }
        })
    }

    
    wayDirty := stage2In.payload.wayDirty | wayDirtyBypass
    when (stage1Out.fire) {
        when (dirtyUpdate && (Mux(axiLoad, getBlockIdx(transferRAddr.asUInt), getBlockIdx(stage2In.payload.vaddr)) === getBlockIdx(Mux(cacopEn, io.ctrl.cacopVA, address)))) {
            wayDirtyBypass := Mux(axiLoad, transferWaySelect, hit)
        } otherwise {
            wayDirtyBypass := B(0).resized
        }
    }

    io.axi.arid := B(1).resized
    io.axi.araddr := Mux(transferUncached, transferRAddr, transferRAddr(config.dCacheBlockOffsetWidth, config.palen - config.dCacheBlockOffsetWidth bits) ## B(0, config.dCacheBlockOffsetWidth bits))
    io.axi.arlen := Mux(transferUncached, B(0).resize(8 bits), B(config.axiBlockBurstLength).resize(8 bits))
    io.axi.arsize := Mux(transferUncached, missingEntry.size, B(2).resized)
    io.axi.arburst := Mux(transferUncached, B"2'b01", B"2'b10") // WRAP, to support critical word first
    io.axi.arlock := B(0).resized // Lock not used
    io.axi.arcache := B(0).resized // Not used
    io.axi.arprot := B(0).resized // Not used
    io.axi.arvalid := False // To get rid of fake LATCH detection

    io.axi.rready := False // To get rid of fake LATCH detection

    io.axi.awid := B(1).resized
    io.axi.awaddr := transferWAddr
    // io.axi.awlen := Mux(transferUncached, B(0).resized, B(config.axiBlockBurstLength).resized)
    // io.axi.awlen := B(config.axiBlockBurstLength).resized
    switch (transferUncached) {
        is (True) {
            io.axi.awlen := B(0).resized
        }
        default {
            io.axi.awlen := B(config.axiBlockBurstLength).resized
        }
    }
    io.axi.awsize := Mux(transferUncached, missingEntry.size, B(2).resized)
    io.axi.awburst := B(1).resized // Always INCR
    io.axi.awlock := B(0).resized // Lock not used
    io.axi.awcache := B(0).resized // Not used
    io.axi.awprot := B(0).resized // Not used
    io.axi.awvalid := False // To get rid of fake LATCH detection

    io.axi.wid := B(1).resized
    io.axi.wdata := Mux(writebackBufferAvail || transferUncached, transferWData, MuxOH(transferWaySelect, portRData1))
    io.axi.wstrb := Mux(transferUncached, transferLSMask, B"4'b1111")
    io.axi.wlast := (~transferWAddrMid.orR || transferUncached) && io.axi.wvalid
    io.axi.wvalid := False // To get rid of fake LATCH detection

    io.axi.bready := False // We must care about write response

    axiLoad := False // To get rid of fake LATCH detection
    refilling := False // To get rid of fake LATCH detection
    rollingBack := False // To get rid of fake LATCH detection
    writeBufferUpdate := False // To get rid of fake LATCH detection

    val missBufferAllowMask = Bits(config.dCacheMissBufferSize bits) // Asserts when the inst is NON-speculative. Only missing entry bit is used now, other bits are reserved for more aggressive refilling
    val missBufferPreAllowMask = Bits(config.dCacheMissBufferSize bits) // Asserts when the cache line is SPECULATIVE. Only missing entry bit is used now, other bits are reserved for more aggressive refilling
    (0 until config.dCacheMissBufferSize).map(i => {
        val robIdxMatchMask = Bits(config.retireWidth bits)
        (0 until config.retireWidth).map(j => {
            if (j == 0) {
                robIdxMatchMask(j) := missBuffer(i).robIdx === io.retireComm.robIdx(j)
            } else {
                robIdxMatchMask(j) := missBuffer(i).robIdx === io.retireComm.robIdx(j) && io.retireComm.allowRetire.asBits(j-1 downto 0).andR
            }
        })
        missBufferAllowMask(i) := robIdxMatchMask.orR && missBuffer(i).valid && ~io.flush
        val writeBufferIdxMatchMask = Bits(config.dCacheWriteBufferSize bits)
        (0 until config.dCacheWriteBufferSize).map(j => {
            writeBufferIdxMatchMask(j) := getBlockIdx(missBuffer(i).vaddr) === writeBuffer(j).index && writeBuffer(j).valid
        })
        missBufferPreAllowMask(i) := writeBufferIdxMatchMask.orR && missBuffer(i).valid && ~io.flush // Don't push it too hard, insts retiring this cycle are not considered
    })

    val cacopPAddr = Mux(stage2In.payload.isHitInvalidate, getTranslatedAddr(stage2In.payload.vaddr).asBits, tagRead(stage2In.payload.vaddr(log2Up(config.dCacheWaySize)-1 downto 0)) ## getBlockIdx(stage2In.payload.vaddr) ## U(0, config.dCacheOffsetWidth bits))

    val axiCtrl = new StateMachine {
        val idle = new State with EntryPoint
        val readReq = new State
        val readFirst = new State
        val read = new State
        val writeReq = new State
        val write = new State
        val writeWait = new State

        idle
            .whenIsActive {
                refilling := False
                writeBufferUpdate := False
                io.axi.arvalid := False
                io.axi.rready := False
                io.axi.awvalid := False
                io.axi.wvalid := False
                io.axi.bready := False
                when (~io.flush && (missBufferAllowMask(missBufferHead) || (missBufferPreAllowMask(missBufferHead) && ~(missingEntry.uncached)))) { // AXI requests are allowed when the inst is non-speculative or when cached L/S can safely refill the cache
                    transferRAddrHi := missingEntry.paddr(config.dCacheOffsetWidth, config.palen-config.dCacheOffsetWidth bits).asBits
                    transferRAddrMid := missingEntry.paddr(config.dCacheBlockOffsetWidth, config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits).asBits
                    transferRAddrLo := missingEntry.paddr(config.dCacheBlockOffsetWidth-1 downto 0).asBits
                    transferWAddrHi := Mux(missingEntry.uncached, missingEntry.paddr(config.dCacheOffsetWidth, config.palen-config.dCacheOffsetWidth bits), missingEntry.prevPaddr(config.dCacheOffsetWidth, config.palen-config.dCacheOffsetWidth bits)).asBits
                    transferWAddrMid := Mux(missingEntry.uncached, missingEntry.paddr(config.dCacheBlockOffsetWidth, config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits), U(0, config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits)).asBits // TODO: awlen, wlast and this have a problem which happens in n14. A simple solution is adding a write counter, just for writing.
                    // transferWAddrMid := U(0, config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits).asBits
                    transferWAddrLo := Mux(missingEntry.uncached, missingEntry.paddr(config.dCacheBlockOffsetWidth-1 downto 0), U(0, config.dCacheBlockOffsetWidth bits)).asBits
                    transferUncached := missingEntry.uncached
                    transferWaySelect := missingEntry.waySelect
                    transferWData := missingEntry.storeData
                    transferLSMask := missingEntry.lsMask
                    when ((missingEntry.uncached && missingEntry.store) || (~missingEntry.uncached && missingEntry.writeBack)) {
                        goto(writeReq)
                    } otherwise {
                        goto(readReq)
                    }
                }
                when (cacopSetInvalid && cacopWriteBack && stage2In.valid) { // No need to check for exception, this has been checked before retiring 
                    transferWAddrHi := cacopPAddr(config.dCacheOffsetWidth, config.palen-config.dCacheOffsetWidth bits).asBits
                    transferWAddrMid := cacopPAddr(config.dCacheBlockOffsetWidth, config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits).asBits // TODO: maybe, here is also a problem? I'm not sure.
                    // transferWAddrMid := U(0, config.dCacheOffsetWidth - config.dCacheBlockOffsetWidth bits).asBits
                    transferWAddrLo := cacopPAddr(config.dCacheBlockOffsetWidth-1 downto 0).asBits
                    transferCACOP := True
                    transferUncached := False
                    transferWaySelect := cacopWay
                    goto(writeReq)
                }
            }
        readReq
            .whenIsActive {
                refilling := True
                writeBufferUpdate := False
                io.axi.arvalid := True
                io.axi.rready := False
                io.axi.awvalid := False
                io.axi.wvalid := False
                io.axi.bready := False
                when (~transferUncached) {
                    (0 until config.dCacheWaySize).map(i => {
                        when(transferWaySelect(i)) {
                            tag(i)(getBlockIdx(transferRAddr.asUInt)) := transferRAddrHi.resizeLeft(config.dCacheTagWidth)
                            valid(i)(getBlockIdx(transferRAddr.asUInt)) := True
                            dirty(i)(getBlockIdx(transferRAddr.asUInt)) := False
                        }
                    })
                }
                when (io.axi.arFire) {
                    transferWData := missingEntry.storeData
                    when (missingEntry.valid && ~io.flush) { // When missing entry hasn't been flushed
                        goto(readFirst)
                    } otherwise {
                        goto(read)
                    }
                }
            }
        readFirst
            .whenIsActive {
                refilling := True
                writeBufferUpdate := missingEntry.store && io.axi.rFire // No flush is ensured when in this state, no need to check it again
                io.axi.arvalid := False
                io.axi.rready := True
                io.axi.awvalid := False
                io.axi.wvalid := False
                io.axi.bready := False
                when (io.axi.rFire) {
                    axiLoad := True
                    transferRAddrMid := (transferRAddrMid.asUInt + 1).asBits
                    when (io.axi.rlast) {
                        axiFinish := True
                        goto(idle)
                    } otherwise {
                        goto(read)
                    }
                } elsewhen (io.flush) {
                    goto(read)
                }
            }
        read
            .whenIsActive {
                refilling := True
                writeBufferUpdate := False
                io.axi.arvalid := False
                io.axi.rready := True
                io.axi.awvalid := False
                io.axi.wvalid := False
                io.axi.bready := False
                when (io.axi.rFire) {
                    transferRAddrMid := (transferRAddrMid.asUInt + 1).asBits
                    when (io.axi.rlast) {
                        axiFinish := True
                        goto(idle)
                    }
                }
            }
        writeReq
            .whenIsActive {
                refilling := True
                writeBufferUpdate := False
                io.axi.arvalid := False
                io.axi.rready := False
                io.axi.awvalid := True
                io.axi.wvalid := False
                io.axi.bready := False
                when (io.axi.awFire) {
                    transferWAddrMid := (transferWAddrMid.asUInt + 1).asBits
                    goto(write)
                }
            }
        write
            .whenIsActive {
                refilling := True
                writeBufferUpdate := False
                io.axi.arvalid := False
                io.axi.rready := False
                io.axi.awvalid := False
                io.axi.wvalid := True
                io.axi.bready := False
                when (writebackDataAvail) {
                    transferWData := MuxOH(transferWaySelect, portRData1)
                }
                when (io.axi.wFire) {
                    transferWAddrMid := (transferWAddrMid.asUInt + 1).asBits
                    when (io.axi.wlast) {
                        // when (transferUncached) { // Uncached store
                        //     axiLoad := True
                        //     axiFinish := True
                        //     goto(idle)
                        // } elsewhen (transferCACOP) {
                        //     transferCACOP := False
                        //     goto(idle)
                        // } otherwise {
                        //     goto(readReq)
                        // }
                       goto(writeWait)
                    }
                }
            }
        writeWait
            .whenIsActive {
                refilling := True
                writeBufferUpdate := False
                io.axi.arvalid := False
                io.axi.rready := False
                io.axi.awvalid := False
                io.axi.wvalid := False
                io.axi.bready := True
                when (io.axi.bFire) {
                    when (transferUncached) { // Uncached store
                        axiLoad := True
                        axiFinish := True
                        goto(idle)
                    } elsewhen (transferCACOP) {
                        transferCACOP := False
                        goto(idle)
                    } otherwise {
                        goto(readReq)
                    }
                }
            }
    }
    val rollbackCtrl = new StateMachine {
        val idle = new State with EntryPoint
        val rollback = new State

        idle
            .whenIsActive {
                rollingBack := False
                when (writeBufferAppend && ~io.flush) {
                    writeBufferTail := writeBufferTail + 1
                }
                when (io.flush && writeBuffer(writeBufferHeadNext).valid) { // The buffer is empty otherwise
                    writeBufferTail := writeBufferTail - 1
                    goto(rollback)
                }
            }
        rollback
            .whenIsActive {
                rollingBack := True
                when (writeBufferHead === writeBufferTail) {
                    goto(idle)
                } otherwise {
                    writeBufferTail := writeBufferTail - 1
                }
            }
    }

    (0 until config.dCacheWaySize).map(i => {
        val conditionMask = Bits(log2Up(config.dCacheWaySize) bits)
        (0 until log2Up(config.dCacheWaySize)).map(j => {
            val groupWidth = config.dCacheWaySize/(1<<j)
            var offset = (1<<j)-1 + i/groupWidth
            if ((i % groupWidth)/(groupWidth/2) == 1) { conditionMask(j) := lruBit(getBlockIdx(stage2In.payload.vaddr))(offset) }
            else { conditionMask(j) := ~lruBit(getBlockIdx(stage2In.payload.vaddr))(offset) }
        })
        wayToReplace(i) := conditionMask.andR
    })
    
    // Stage 2
    val dataShuffle = Vec.fill(config.dCacheWaySize)(Bits(config.axiDataWidth bits))
    val axiShuffle = Bits(config.axiDataWidth bits)
    (0 until config.dCacheWaySize).map(i => {
        val shiftedData = dataRead(i) |>> (stage2In.payload.vaddr(log2Up(config.wordLength/8)-1 downto 0) @@ U(0, 3 bits))
        if (config.wordLength == 32) {
            switch(stage2In.payload.lsCtrlBundle.size) {
                is(0)   { dataShuffle(i) := (shiftedData( 7) & stage2In.payload.lsCtrlBundle.signed) #* (config.wordLength- 8) ## shiftedData( 7 downto 0) }
                is(1)   { dataShuffle(i) := (shiftedData(15) & stage2In.payload.lsCtrlBundle.signed) #* (config.wordLength-16) ## shiftedData(15 downto 0) }
                is(2)   { dataShuffle(i) := shiftedData }
                default { dataShuffle(i) := shiftedData }
            }
        } else {
            switch(stage2In.payload.lsCtrlBundle.size) {
                is(0)   { dataShuffle(i) := (shiftedData( 7) & stage2In.payload.lsCtrlBundle.signed) #* (config.wordLength- 8) ## shiftedData( 7 downto 0) }
                is(1)   { dataShuffle(i) := (shiftedData(15) & stage2In.payload.lsCtrlBundle.signed) #* (config.wordLength-16) ## shiftedData(15 downto 0) }
                is(2)   { dataShuffle(i) := (shiftedData(31) & stage2In.payload.lsCtrlBundle.signed) #* (config.wordLength-32) ## shiftedData(31 downto 0) }
                is(3)   { dataShuffle(i) := shiftedData }
                default { dataShuffle(i) := shiftedData }
            }
        }
    })
    val axiShiftedData = io.axi.rdata |>> (missingEntry.vaddr(log2Up(config.wordLength/8)-1 downto 0) @@ U(0, 3 bits))
    if (config.wordLength == 32) {
        switch(missingEntry.size) {
            is(0)   { axiShuffle := (axiShiftedData( 7) & missingEntry.signed) #* (config.wordLength- 8) ## axiShiftedData( 7 downto 0) }
            is(1)   { axiShuffle := (axiShiftedData(15) & missingEntry.signed) #* (config.wordLength-16) ## axiShiftedData(15 downto 0) }
            is(2)   { axiShuffle := axiShiftedData }
            default { axiShuffle := axiShiftedData }
        }
    } else {
        switch(missingEntry.size) {
            is(0)   { axiShuffle := (axiShiftedData( 7) & missingEntry.signed) #* (config.wordLength- 8) ## axiShiftedData( 7 downto 0) }
            is(1)   { axiShuffle := (axiShiftedData(15) & missingEntry.signed) #* (config.wordLength-16) ## axiShiftedData(15 downto 0) }
            is(2)   { axiShuffle := (axiShiftedData(31) & missingEntry.signed) #* (config.wordLength-32) ## axiShiftedData(31 downto 0) }
            is(3)   { axiShuffle := axiShiftedData }
            default { axiShuffle := axiShiftedData }
        }
    }

    io.output.payload.robIdx := Mux(axiLoad, missingEntry.robIdx, stage2In.payload.robIdx)
    io.output.payload.data := Mux(axiLoad, Mux(missingEntry.sc, scResAXI, axiShuffle.asUInt), Mux(stage2In.payload.lsCtrlBundle.sc, scResHit, MuxOH(hit, dataShuffle).asUInt)) // Ugly, may need timing optimization
    io.output.payload.prd := Mux(axiLoad, missingEntry.prd, stage2In.payload.prd)
    io.output.payload.branchInfo := Mux(axiLoad, missingEntry.branchInfo, stage2In.payload.branchInfo)
    io.output.payload.branchResult := Mux(axiLoad, missingEntry.branchResult, stage2In.payload.branchResult)
    io.output.payload.exceptionInfo := Mux(axiLoad, missingEntry.exceptionInfo, exceptionInfo)
    io.output.valid := axiLoad || (((hit.orR && ~((axiFillPriority || axiWritebackPriority) && stage2In.payload.lsCtrlBundle.store)) || exceptionInfo.exception || ~stage2In.payload.lsCtrlBundle.normalMemOp) && stage2In.valid && ~cacopActive)

    io.badv.robIdx := stage2In.payload.robIdx
    io.badv.vaddr := stage2In.payload.vaddr.asBits
    io.badv.wen := exceptionInfo.exception && stage2In.valid && ~cacopActive && stage2In.payload.lsException

    io.wakeOut(0).valid := False // Reserved for stage 1 waking up
    io.wakeOut(0).payload := B(0).resized

    io.wakeOut(1).valid := axiLoad || (stage2In.valid && ~axiLoad && (stage2In.payload.lsCtrlBundle.load || stage2In.payload.lsCtrlBundle.sc) && (hit.orR && ~((axiFillPriority || axiWritebackPriority) && stage2In.payload.lsCtrlBundle.store)) && ~cacopActive)
    io.wakeOut(1).payload := io.output.payload.prd
    
    io.ctrl.busy := refilling || rollingBack || (cacopActive && stage2In.valid)

    val cacopIdx = Bits(config.dCacheIdxWidth bits)
    val cacopWay = Bits(config.dCacheWaySize bits) // One-hot
    val cacopHit = Bits(config.iCacheWaySize bits) // One-hot, CACOP Hit Invalidate doesn't check for cacheability, make it happy
    val cacopWriteBack = Bool()
    val cacopSetInvalid = stage2In.payload.isHitInvalidate || stage2In.payload.isIndexInvalidate
    cacopActive := cacopSetInvalid || stage2In.payload.isStoreTag
    cacopWriteBack := False
    cacopIdx := getBlockIdx(stage2In.payload.vaddr).asBits
    cacopWay := Mux(stage2In.payload.isHitInvalidate, hit, (B(1, config.dCacheWaySize bits) |<< (stage2In.payload.vaddr(log2Up(config.dCacheWaySize)-1 downto 0))))
    (0 until config.dCacheWaySize).map(i => {
        cacopHit(i) := stage2In.payload.wayValid(i) & (tagRead(i) === getTag(stage2In.payload.vaddr).asBits)
        when (stage2In.valid && cacopActive) {
            when (cacopWay(i)) {
                valid(i)(cacopIdx.asUInt) := False
            }
        }
    })
    when (stage2In.valid && cacopActive) {
        cacopWriteBack := (wayDirty & cacopWay).orR
    }

    val specialOpBufferWrite = ~stage1Out.payload.lsCtrlBundle.normalMemOp && io.input.payload.uop.lsuOp =/= LSUOp.preld && io.input.payload.uop.lsuOp =/= LSUOp.dbar && ~io.ctrl.stall
    io.specialOpBufferUpdate.valid := io.input.valid && specialOpBufferWrite
    io.specialOpBufferUpdate.payload.uop := io.input.payload.uop
    io.specialOpBufferUpdate.payload.vaddr := Mux(io.input.payload.uop.lsuOp === LSUOp.invtlb, io.input.payload.src2, address)
    io.specialOpBufferUpdate.payload.asid := io.input.payload.src1(9 downto 0).asBits

    def getBlockIdx(addr: UInt): UInt = {
        return addr(config.dCacheIdxWidth+config.dCacheOffsetWidth-1 downto config.dCacheOffsetWidth)
    }
    def getDataIdx(addr: UInt): UInt = {
        return addr(config.dCacheIdxWidth+config.dCacheOffsetWidth-1 downto config.dCacheBlockOffsetWidth)
    }
    def getTag(addr: UInt): UInt = {
        if (config.dCacheIdxWidth+config.dCacheOffsetWidth < 12) {
            return stage2In.payload.tlb.pageInfo.ppn.asUInt @@ (addr(11 downto config.dCacheIdxWidth+config.dCacheOffsetWidth))
        } else {
            return stage2In.payload.tlb.pageInfo.ppn.asUInt
        }
    }
    def getPrevAddr(addr: UInt): UInt = {
        val ppn = MuxOH(wayToReplace, tagRead)
        if (config.dCacheIdxWidth+config.dCacheOffsetWidth < 12) {
            return ppn.asUInt @@ (addr(config.dCacheIdxWidth+config.dCacheOffsetWidth-1 downto 0))
        } else {
            return ppn.asUInt @@ addr(11 downto 0)
        }
    }
    def getTranslatedAddr(addr: UInt): UInt = {
        return stage2In.payload.tlb.pageInfo.ppn.asUInt @@ (addr(11 downto 0))
    }
    def setLRUUpdate(index: UInt, hi: Int, lo: Int): Unit = {
        val width = hi - lo + 1
        val level = log2Up(config.dCacheWaySize/width*2)-1 // Too lazy to find a log2 func, using shifted log2up
        val offset = (1<<level)-1 + lo/width
        lruBit(index)(offset) := Mux(axiLoad, transferWaySelect(width/2-1+lo downto lo).orR,  hit(width/2-1+lo downto lo).orR)
        if (width > 2) {
            setLRUUpdate(index, width/2-1+lo, lo)
            setLRUUpdate(index, hi, width/2+lo)
        }
    }
}


case class DCacheLSCtrlBundle(config: CPUConfig) extends Bundle {
    val load = Bool() // Asserted for Load and LL
    val store = Bool() // Asserted for Store and SC
    val signed = Bool() // Asserted for LD and deasserted for LDU, don't care for other insts
    val ll = Bool() // Asserted for LL and deasserted for Load or other insts
    val sc = Bool() // Asserted for SC and deasserted for Store or other insts
    val lsMask = Bits(config.wordLength / 8 bits)
    val size = Bits(AXIBundle(false, config).arsize.getWidth bits) // Same encoding as AxSIZE
    val normalMemOp = Bool() // Asserted for Load/Store, LL or SC
}

case class DCachePipelineBundle(config: CPUConfig) extends Bundle {
    val robIdx = Bits(config.robIdxWidth bits)
    val prd = Bits(config.prfIdxWidth bits)
    val branchInfo = BranchInfo(config)
    val branchResult = BranchResult(config)
    val exceptionInfo = ExceptionInfo()
    val storeData = Bits(config.wordLength bits)
    val lsCtrlBundle = DCacheLSCtrlBundle(config)
    val vaddr = UInt(config.valen bits)
    val tlb = TLBRespondBundle(config)
    val wayValid = Bits(config.dCacheWaySize bits)
    val wayDirty = Bits(config.dCacheWaySize bits)
    val isStoreTag = Bool()
    val isIndexInvalidate = Bool()
    val isHitInvalidate = Bool()
    val checkTLBException = Bool()
    val lsException = Bool()
}

case class DCacheMissBufferEntryBundle(config: CPUConfig) extends Bundle {
    val robIdx = Bits(config.robIdxWidth bits)
    val prd = Bits(config.prfIdxWidth bits)
    val branchInfo = BranchInfo(config)
    val branchResult = BranchResult(config)
    val exceptionInfo = ExceptionInfo()
    val uncached = Bool()
    val load = Bool() // Asserted for Load and LL
    val store = Bool() // Asserted for Store and SC
    val signed = Bool() // Asserted for LD and deasserted for LDU, don't care for other insts
    val ll = Bool() // Asserted for LL and deasserted for Load or other insts
    val sc = Bool() // Asserted for SC and deasserted for Store or other insts
    val writeBufferIdx = UInt(log2Up(config.dCacheWriteBufferSize) bits)
    val waySelect = Bits(config.dCacheWaySize bits)
    val writeBack = Bool()
    val storeData = Bits(config.axiDataWidth bits)
    val lsMask = Bits(config.axiDataWidth / 8 bits)
    val size = Bits(AXIBundle(false, config).arsize.getWidth bits) // Same encoding as AxSIZE
    val vaddr = UInt(config.valen bits)
    val paddr = UInt(config.palen bits)
    val prevPaddr = UInt(config.palen bits)
    val valid = Bool()
    def resetVal: DCacheMissBufferEntryBundle = {
        val value = DCacheMissBufferEntryBundle(config)
        value.robIdx := B(0).resized
        value.prd := B(0).resized
        value.branchInfo := BranchInfo(config).resetVal
        value.branchResult := BranchResult(config).resetVal
        value.exceptionInfo := ExceptionInfo().resetVal
        value.uncached := False
        value.load := False
        value.store := False
        value.signed := False
        value.ll := False
        value.sc := False
        value.writeBufferIdx := U(0).resized
        value.waySelect := B(0).resized
        value.writeBack := False
        value.storeData := B(0).resized
        value.lsMask := B(0).resized
        value.size := B(0).resized
        value.vaddr := U(0).resized
        value.paddr := U(0).resized
        value.prevPaddr := U(0).resized
        value.valid := False
        return value
    }
}

case class DCacheWriteBufferEntryBundle(config: CPUConfig) extends Bundle {
    val robIdx = Bits(config.robIdxWidth bits)
    val waySelect = Bits(config.dCacheWaySize bits)
    val prevData = Bits(config.axiDataWidth bits)
    val prevDirty = Bool()
    val index = UInt(log2Up(config.dCacheSizePerWay) bits)
    val miss = Bool()
    val valid = Bool()
    def resetVal: DCacheWriteBufferEntryBundle = {
        val value = DCacheWriteBufferEntryBundle(config)
        value.robIdx := B(0).resized
        value.waySelect := B(0).resized
        value.prevData := B(0).resized
        value.prevDirty := False
        value.index := U(0).resized
        value.miss := False
        value.valid := False
        return value
    }
}