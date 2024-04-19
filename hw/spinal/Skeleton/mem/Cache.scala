package Skeleton.mem

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import Skeleton.bundle._
import Skeleton.config._

case class ICache(config: CPUConfig) extends Component {
    val io = new Bundle {
        val input = Vec.fill(config.fetchWidth)(slave Stream(ICacheReqBundle(config))) // 0-latency!
        val output = slave(InstrQueueInBundle(config))
        val tlb = master(TLBRequestBundle(config))
        val plv = in(CSRBundle(config).crmd.plv)
        val ctrl = slave(ICacheCtrlBundle(config)) // Stall stage 1
        val flush = in(Bool())
        val badv = master(BADVBundle(config))
        val axi = master(AXIBundle(false, config))
    }
    val data = Array.fill(config.iCacheWaySize)(Mem(Bits(config.axiDataWidth bits), config.iCacheSizePerWay))
    val tag = Array.fill(config.iCacheWaySize)(Mem(Bits(config.iCacheTagWidth bits), config.iCacheLineSize))
    (0 until config.iCacheWaySize).map(i => {
        data(i).generateAsBlackBox()
        tag(i).generateAsBlackBox()
    })
    val valid = Array.fill(config.iCacheWaySize)(Vec.fill(config.iCacheLineSize)(Reg(Bool())))
    valid.foreach(_.foreach(_ init(False)))
    val dataRead = Vec.fill(config.fetchWidth)(Vec.fill(config.iCacheWaySize)(Bits(config.axiDataWidth bits)))
    val tagRead = Vec.fill(config.fetchWidth)(Vec.fill(config.iCacheWaySize)(Bits(config.iCacheTagWidth bits)))
    val hit = Vec.fill(config.fetchWidth)(Bits(config.iCacheWaySize bits))
    val miss = Bits(config.fetchWidth bits)
    val stall = Bool()
    val cacopEn = io.ctrl.cacopStoreTag | io.ctrl.cacopIndexInvalidate | io.ctrl.cacopHitInvalidate

    val stage1Out = Stream(ICachePipelineBundle(config))
    val stage2In = Stream(ICachePipelineBundle(config))
    stage2In <-< stage1Out.throwWhen(io.flush)
    stage1Out.valid := stage1Out.payload.valid.orR | cacopEn
    val acceptMask = Reg(Bits(config.fetchWidth bits)) // Used for stage 2 partial launch selection
    val fetchMask = Vec.fill(config.fetchWidth)(Reg(Bool()))
    io.tlb.virtPageNumber := Mux(io.ctrl.cacopHitInvalidate, io.ctrl.cacopVA(config.valen-1 downto 12).asBits, io.input(0).payload.address(config.valen-1 downto 12).asBits)
    stage1Out.payload.tlb.hit := io.tlb.hit
    stage1Out.payload.tlb.pageInfo := io.tlb.pageInfo
    stage1Out.payload.isStoreTag := io.ctrl.cacopStoreTag
    stage1Out.payload.isIndexInvalidate := io.ctrl.cacopIndexInvalidate
    stage1Out.payload.isHitInvalidate := io.ctrl.cacopHitInvalidate
    when (stage1Out.fire) {
        acceptMask := B(0).resized
    } otherwise {
        acceptMask := acceptMask | (io.output.allowMask |<< CountOne(acceptMask))
    }
    (0 until config.fetchWidth).map(i => {
        (0 until config.iCacheWaySize).map(j => {
            if (i == 0) {                
                stage1Out.payload.wayValid(i)(j) := valid(j)(getBlockIdx(Mux(io.ctrl.cacopHitInvalidate, io.ctrl.cacopVA, io.input(i).payload.address)))
                dataRead(i)(j) := data(j).readSync(getDataIdx(Mux(io.ctrl.cacopHitInvalidate, io.ctrl.cacopVA, io.input(i).payload.address)), stage1Out.fire)
                tagRead(i)(j) := tag(j).readSync(getBlockIdx(Mux(io.ctrl.cacopHitInvalidate, io.ctrl.cacopVA, io.input(i).payload.address)), stage1Out.fire)
                hit(i)(j) := stage2In.payload.wayValid(i)(j) & (tagRead(i)(j) === getTag(stage2In.payload.pc(i)).asBits) & (stage2In.payload.tlb.pageInfo.mat === B(1).resized)
            } else {
                stage1Out.payload.wayValid(i)(j) := valid(j)(getBlockIdx(io.input(i).payload.address))
                dataRead(i)(j) := data(j).readSync(getDataIdx(io.input(i).payload.address), stage1Out.fire)
                tagRead(i)(j) := tag(j).readSync(getBlockIdx(io.input(i).payload.address), stage1Out.fire)
                hit(i)(j) := stage2In.payload.wayValid(i)(j) & (tagRead(i)(j) === getTag(stage2In.payload.pc(i)).asBits) & (stage2In.payload.tlb.pageInfo.mat === B(1).resized)
            }
        })
        miss(i) := stage2In.payload.valid(i) & ~(hit(i).orR | io.output.info(i).exceptionInfo.exception | fetchMask(i)) // Handles miss only when missed request has no exception
    })
    
    val exceptionInfo1 = Vec.fill(config.fetchWidth)(ExceptionInfo())
    val exceptionInfo2 = ExceptionInfo()
    val availMask = Bits(config.fetchWidth bits)
    val fireMask = io.output.allowMask |<< CountOne(acceptMask)
    val portAvail = Bits(config.fetchWidth bits)
    val portData = Vec.fill(config.fetchWidth)(InstrQueueEntry(config))
    val hasException = Bits(config.fetchWidth bits)
    acceptMask.init(B(0).resized)
    stage2In.ready := ((acceptMask | fireMask) === stage2In.payload.valid) || io.flush
    when (stage2In.payload.tlb.hit) {
        when (~stage2In.payload.tlb.pageInfo.v) {
            exceptionInfo2.exception := True
            exceptionInfo2.eCode := ECode.PIF.eCode
            exceptionInfo2.eSubCode := ECode.PIF.eSubCode
        } elsewhen (io.plv.asUInt > stage2In.payload.tlb.pageInfo.plv.asUInt) {
            exceptionInfo2.exception := True
            exceptionInfo2.eCode := ECode.PPI.eCode
            exceptionInfo2.eSubCode := ECode.PPI.eSubCode
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

    // Let's handle cache miss/uncached now
    val missBuffer = Vec.fill(config.fetchWidth)(Reg(Bits(config.axiDataWidth bits)))
    val missVector = OHMasking.first(miss)
    val missAddr = PriorityMux(miss, stage2In.pc)
    val replacingWay = Reg(Bits(config.iCacheWaySize bits))
    val transferBlockOffset = Reg(UInt(config.iCacheOffsetWidth bits))
    val transferIndexOffset = Reg(UInt(12-config.iCacheOffsetWidth bits))
    val transferAddr = stage2In.payload.tlb.pageInfo.ppn.asUInt @@ transferIndexOffset @@ transferBlockOffset
    val sameBlockMask = Bits(config.fetchWidth bits)
    val bufWriteMask = Bits(config.fetchWidth bits)
    val refilling = Bool()
    (0 until config.fetchWidth).map(i => {
        sameBlockMask(i) := io.input(i).valid & (getBlockIdx(transferAddr) === getBlockIdx(io.input(i).payload.address))
        bufWriteMask(i) := io.axi.rFire & (transferAddr(config.iCacheIdxWidth+config.iCacheOffsetWidth-1 downto 0) === stage2In.payload.pc(i)(config.iCacheIdxWidth+config.iCacheOffsetWidth-1 downto 0))
        when (stage1Out.fire) {
            (0 until config.fetchWidth).map(i => { fetchMask(i) := False })
        } otherwise {
            fetchMask(i) := fetchMask(i) | bufWriteMask(i) // Note that parameterization here is not complete: we assume that buffer can be fully filled within 1 beat
        }
    })
    stall := io.ctrl.stall | (sameBlockMask.orR & refilling) // When req in stage1 share same block index with the one under refill, req should be stalled for 1 cycle to read newly written data out
    
    val lruBit = Vec.fill(config.iCacheLineSize)(Vec.fill(config.iCacheWaySize-1)(Reg(Bool()))) // Trick to make LRU array parameterizable
    lruBit.foreach(_.foreach(_ init(False))) // Initial to way 0
    val wayToReplace = Vec.fill(config.fetchWidth)(Bits(config.iCacheWaySize bits))
    val wayOfReplace = Vec.fill(config.fetchWidth)(Reg(Bits(config.iCacheWaySize bits)))

    (0 until config.fetchWidth).map(i => {
        when (stage1Out.fire) {
            wayOfReplace(i) := B(0).resized
        }
        when (fireMask(i) && stage2In.payload.tlb.pageInfo.mat === B(1).resized) {
            setLRUUpdate(getBlockIdx(stage2In.payload.pc(i)), config.iCacheWaySize-1, 0, i)
        }
    })
    
    io.axi.arid := B(0).resized
    io.axi.araddr := transferAddr.asBits
    io.axi.arlen := Mux(stage2In.payload.tlb.pageInfo.mat === B(0).resized, B(0).resize(8 bits), B(config.axiBlockBurstLength).resize(8 bits))
    io.axi.arsize := B"3'b010" // 4 bytes per beat
    io.axi.arburst := Mux(stage2In.payload.tlb.pageInfo.mat === B(0).resized, B"2'b01", B"2'b10") // WRAP, to support critical word first
    io.axi.arlock := B(0).resized // Lock not used
    io.axi.arcache := B(0).resized // Not used
    io.axi.arprot := B(0).resized // Not used
    io.axi.arvalid := False // To get rid of fake LATCH detection
    io.axi.rready := False // To get rid of fake LATCH detection
    refilling := False // To get rid of fake LATCH detection

    val fsm = new StateMachine {
        val idle = new State with EntryPoint
        val req = new State
        val read = new State

        idle
            .whenIsActive {
                refilling := False
                io.axi.arvalid := False
                io.axi.rready := False
                when (miss.orR & stage2In.valid & ~io.flush) {
                    goto(req)
                }
            }
        req
            .onEntry {
                transferIndexOffset := missAddr(11 downto config.iCacheOffsetWidth)
                transferBlockOffset := missAddr(config.iCacheOffsetWidth-1 downto 0)
                replacingWay := PriorityMux(miss, wayToReplace)
            }
            .whenIsActive {
                refilling := True
                io.axi.arvalid := True
                io.axi.rready := False
                when (io.axi.arFire) {
                    goto(read)
                }
            }
        read
            .onEntry {
                when (stage2In.payload.tlb.pageInfo.mat === B(1).resized) { // Refill
                    (0 until config.iCacheWaySize).map(i => {
                        when (replacingWay(i)) {
                            tag(i)(getBlockIdx(transferAddr)) := getTag(transferAddr).asBits
                            valid(i)(getBlockIdx(transferAddr)) := True
                        }
                    })
                    (0 until config.fetchWidth).map(i => {
                        when (getBlockIdx(transferAddr) === getBlockIdx(stage2In.payload.pc(i))) {
                            wayOfReplace(i) := replacingWay
                        }
                    })
                }
            }
            .whenIsActive {
                refilling := True
                io.axi.arvalid := False
                io.axi.rready := True
                when (stage2In.payload.tlb.pageInfo.mat === B(1).resized) { // Refill
                    (0 until config.iCacheWaySize).map(i => {
                        when (io.axi.rFire) {
                            when (replacingWay(i)) {
                                data(i)(getDataIdx(transferAddr)) := io.axi.rdata
                            }
                            transferBlockOffset := transferBlockOffset + U(config.axiDataWidth/8)
                        }
                    })
                    
                }
                when (io.axi.rlast & io.axi.rFire) {
                    when (miss.orR & stage2In.valid & ~io.flush) {
                        goto(req)
                    } otherwise {
                        goto(idle)
                    }
                }
            }
    }

    val allowMask = Bits(config.fetchWidth bits)
    (0 until config.fetchWidth).map(i => {
        // Stage 1
        when (io.input(i).payload.address(log2Up(config.instLength/8)-1 downto 0).orR) {
            exceptionInfo1(i).exception := True
            exceptionInfo1(i).eCode := ECode.ADEF.eCode
            exceptionInfo1(i).eSubCode := ECode.ADEF.eSubCode
        } otherwise {
            exceptionInfo1(i).exception := False
            exceptionInfo1(i).eCode := ECode.ADEF.eCode // Just to make mux logic simpler
            exceptionInfo1(i).eSubCode := ECode.ADEF.eSubCode // Just to make mux logic simpler
        }

        // Only continuous inst flow that meets limitation below can be served together in one cycle
        // 1. Share same TAG (to reduce TLB port cost)
        // 2. Either in the same block or in different blocks that have different indexes (to avoid WAW hazard in replacement unit)
        // The implemented blocking logic is a little bit stricter but has almost the same effect
        allowMask(i) := io.input(i).payload.address(config.valen-1 downto config.iCacheIdxWidth+config.iCacheOffsetWidth) === io.input(0).payload.address(config.valen-1 downto config.iCacheIdxWidth+config.iCacheOffsetWidth)
        io.input(i).ready := stage1Out.ready & ~stall & allowMask(i downto 0).andR & ~io.flush
        stage1Out.payload.branchInfo(i) := io.input(i).payload.branchInfo
        stage1Out.payload.exceptionInfo(i) := exceptionInfo1(i)
        if (i == 0) { // CACOP Hit Invalidate uses channel 0
            stage1Out.payload.pc(i) := Mux(cacopEn, io.ctrl.cacopVA, io.input(i).payload.address)
            stage1Out.payload.valid(i) := io.input(i).fire
        } else {
            stage1Out.payload.pc(i) := io.input(i).payload.address
            stage1Out.payload.valid(i) := io.input(i).fire & (io.input(i).payload.address(config.valen-1 downto config.iCacheIdxWidth+config.iCacheOffsetWidth) === io.input(0).payload.address(config.valen-1 downto config.iCacheIdxWidth+config.iCacheOffsetWidth))
        }
        // Stage 2
        availMask(i) := stage2In.payload.valid(i) & (hit(i).orR | io.output.info(i).exceptionInfo.exception | acceptMask(i) | fetchMask(i))
        portAvail(i) := availMask(i downto 0).andR
        portData(i).inst := Mux(fetchMask(i) | miss(i), Mux(miss(i), io.axi.rdata, missBuffer(i)), MuxOH(hit(i), dataRead(i)))
        portData(i).branchInfo := stage2In.payload.branchInfo(i)
        portData(i).exceptionInfo := Mux(stage2In.payload.exceptionInfo(i).exception, exceptionInfo2, stage2In.payload.exceptionInfo(i))
        portData(i).pc := stage2In.payload.pc(i)

        when (bufWriteMask(i)) { missBuffer(i) := io.axi.rdata }

        io.output.availMask(i) := portAvail((i+CountOne(acceptMask)).resized)
        io.output.info(i) := portData((i+CountOne(acceptMask)).resized)
        
        hasException(i) := stage2In.payload.valid(i) & io.output.info(i).exceptionInfo.exception

        (0 until config.iCacheWaySize).map(j => {
            val conditionMask = Bits(log2Up(config.iCacheWaySize) bits)
            (0 until log2Up(config.iCacheWaySize)).map(k => {
                val groupWidth = config.iCacheWaySize/(1<<k)
                var offset = (1<<k)-1 + j/groupWidth
                if ((j % groupWidth)/(groupWidth/2) == 1) { conditionMask(k) := lruBit(getBlockIdx(stage2In.payload.pc(i)))(offset) }
                else { conditionMask(k) := ~lruBit(getBlockIdx(stage2In.payload.pc(i)))(offset) }
            })
            wayToReplace(i)(j) := conditionMask.andR
        })
    })

    io.badv.vaddr := PriorityMux(hasException, stage2In.payload.pc).asBits
    io.badv.wen := hasException.orR

    io.ctrl.axiInProgress := refilling | (miss.orR & stage2In.valid) | (stage2In.valid && (stage2In.payload.isHitInvalidate || stage2In.payload.isIndexInvalidate || stage2In.payload.isStoreTag))

    val cacopIdx = Bits(config.iCacheIdxWidth bits)
    val cacopWay = Bits(config.iCacheWaySize bits) // One-hot
    cacopIdx := getBlockIdx(stage2In.payload.pc(0)).asBits
    cacopWay := Mux(stage2In.payload.isHitInvalidate, hit(0), (B(1, config.iCacheWaySize bits) |<< (stage2In.payload.pc(0)(log2Up(config.iCacheWaySize)-1 downto 0))))
    when (stage2In.valid && (stage2In.payload.isHitInvalidate || stage2In.payload.isIndexInvalidate || stage2In.payload.isStoreTag)) {
        (0 until config.iCacheWaySize).map(i => {
            when (cacopWay(i)) {
                valid(i)(cacopIdx.asUInt) := False
            }
        })
    }

    def getBlockIdx(addr: UInt): UInt = {
        return addr(config.iCacheIdxWidth+config.iCacheOffsetWidth-1 downto config.iCacheOffsetWidth)
    }
    def getDataIdx(addr: UInt): UInt = {
        return addr(config.iCacheIdxWidth+config.iCacheOffsetWidth-1 downto config.iCacheBlockOffsetWidth)
    }
    def getTag(addr: UInt): UInt = {
        if (config.iCacheIdxWidth+config.iCacheOffsetWidth < 12) {
            return stage2In.payload.tlb.pageInfo.ppn.asUInt @@ (addr(11 downto config.iCacheIdxWidth+config.iCacheOffsetWidth))
        } else {
            return stage2In.payload.tlb.pageInfo.ppn.asUInt
        }
    }
    def getTranslatedAddr(addr: UInt): UInt = {
        return stage2In.payload.tlb.pageInfo.ppn.asUInt @@ (addr(11 downto 0))
    }
    def setLRUUpdate(index: UInt, hi: Int, lo: Int, port: Int): Unit = {
        val width = hi - lo + 1
        val level = log2Up(config.iCacheWaySize/width*2)-1 // Too lazy to find a log2 func, using shifted log2up
        val offset = (1<<level)-1 + lo/width
        when (wayOfReplace(port)(hi downto lo).orR | hit(port)(hi downto lo).orR) { // wen
            lruBit(index)(offset) := wayOfReplace(port)(hi downto width/2+lo).orR | hit(port)(hi downto width/2+lo).orR
        }
        if (width > 2) {
            setLRUUpdate(index, width/2-1+lo, lo, port)
            setLRUUpdate(index, hi, width/2+lo, port)
        }
    }
}

case class ICachePipelineBundle(config: CPUConfig) extends Bundle {
    val branchInfo = Vec.fill(config.fetchWidth)(BranchInfo(config))
    val exceptionInfo = Vec.fill(config.fetchWidth)(ExceptionInfo())
    val pc = Vec.fill(config.fetchWidth)(UInt(config.wordLength bits))
    val valid = Bits(config.fetchWidth bits)
    val tlb = TLBRespondBundle(config)
    val wayValid = Vec.fill(config.fetchWidth)(Bits(config.iCacheWaySize bits))
    val isStoreTag = Bool()
    val isIndexInvalidate = Bool()
    val isHitInvalidate = Bool()
}