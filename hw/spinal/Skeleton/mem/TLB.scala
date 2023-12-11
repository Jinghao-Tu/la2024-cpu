package Skeleton.mem

import spinal.core._
import spinal.lib.master

import Skeleton.bundle._
import Skeleton.config._

case class TLB(config: CPUConfig) extends Component {
    val io = new Bundle {
        val iCacheReq = master(TLBRequestBundle(config))
        val dCacheReq = master(TLBRequestBundle(config))
        
    }

}