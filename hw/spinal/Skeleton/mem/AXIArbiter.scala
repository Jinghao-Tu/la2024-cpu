package Skeleton.mem

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class AXIArbiter(config: CPUConfig) extends Component {
    val io = new Bundle {
        val out = master(AXIBundle(true, config))
        val iCache = slave(AXIBundle(false, config))
        val dCache = slave(AXIBundle(true, config))
    }

    // TODO: maybe this is not a good idea
    // notice that we can only have one dcache request and one icache request at a time at most
    // when to send dcache request:
    // 1. last cycle is sending an unresponsed dcache request (it must be the same request)
    // 2. last cycle get a response from axi
    // 3. last cycle is empty
    val ARidLast = RegNext(io.out.arid)
    val ARvalidLast = RegNext(io.out.arvalid)
    val ARreadyLast = RegNext(io.out.arready)
    val chooseDCacheAR = io.dCache.arvalid && (
        (ARidLast === B(1).resized && ARvalidLast && !ARreadyLast) ||
        ARreadyLast ||
        !ARvalidLast
    )
    val chooseDCacheR = io.out.rid === B(1).resized

    io.out.arid    := Mux(chooseDCacheAR, io.dCache.arid   , io.iCache.arid   )
    io.out.araddr  := Mux(chooseDCacheAR, io.dCache.araddr , io.iCache.araddr )
    io.out.arlen   := Mux(chooseDCacheAR, io.dCache.arlen  , io.iCache.arlen  )
    io.out.arsize  := Mux(chooseDCacheAR, io.dCache.arsize , io.iCache.arsize )
    io.out.arburst := Mux(chooseDCacheAR, io.dCache.arburst, io.iCache.arburst)
    io.out.arlock  := Mux(chooseDCacheAR, io.dCache.arlock , io.iCache.arlock )
    io.out.arcache := Mux(chooseDCacheAR, io.dCache.arcache, io.iCache.arcache)
    io.out.arprot  := Mux(chooseDCacheAR, io.dCache.arprot , io.iCache.arprot )
    io.out.arvalid := Mux(chooseDCacheAR, io.dCache.arvalid, io.iCache.arvalid)
    
    io.iCache.arready := io.out.arready && ~chooseDCacheAR
    io.dCache.arready := io.out.arready && chooseDCacheAR

    io.iCache.rid    := io.out.rid   
    io.iCache.rdata  := io.out.rdata 
    io.iCache.rresp  := io.out.rresp 
    io.iCache.rlast  := io.out.rlast 
    io.iCache.rvalid := io.out.rvalid && ~chooseDCacheR

    io.dCache.rid    := io.out.rid   
    io.dCache.rdata  := io.out.rdata 
    io.dCache.rresp  := io.out.rresp 
    io.dCache.rlast  := io.out.rlast 
    io.dCache.rvalid := io.out.rvalid && chooseDCacheR
    
    io.out.rready := Mux(chooseDCacheR, io.dCache.rready, io.iCache.rready)

    io.out.awid    := io.dCache.awid   
    io.out.awaddr  := io.dCache.awaddr 
    io.out.awlen   := io.dCache.awlen  
    io.out.awsize  := io.dCache.awsize 
    io.out.awburst := io.dCache.awburst
    io.out.awlock  := io.dCache.awlock 
    io.out.awcache := io.dCache.awcache
    io.out.awprot  := io.dCache.awprot 
    io.out.awvalid := io.dCache.awvalid

    io.dCache.awready := io.out.awready

    io.out.wid    := io.dCache.wid   
    io.out.wdata  := io.dCache.wdata 
    io.out.wstrb  := io.dCache.wstrb 
    io.out.wlast  := io.dCache.wlast 
    io.out.wvalid := io.dCache.wvalid

    io.dCache.wready := io.out.wready

    io.dCache.bid    := io.out.bid   
    io.dCache.bresp  := io.out.bresp 
    io.dCache.bvalid := io.out.bvalid

    io.out.bready := io.dCache.bready
}