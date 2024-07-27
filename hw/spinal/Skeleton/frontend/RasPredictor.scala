package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

// ! la32r 没有 call 和 ret 指令, 但有类似指令 如 jirl, ertn, syscall, break. 如何处理留待后续考虑
// ! 先不添加 RAS

// * 用于函数调用和返回地址的栈结构预测器
// RAS 是一个寄存器堆实现的栈存储结构，它对 call 指令的下一条指令的地址进行记录，在 FTB 认为预测块会在 call 指令跳转时压栈，并在 FTB 认为预测块在 ret 指令跳转时弹出。每一项包含一个地址和一个计数器，当重复压栈同一个地址时，栈指针不变，计数器加一，用于处理程序中递归调用的情况。每次预测后，栈顶项和栈指针都会存入 FTQ 的存储结构，用于误预测时恢复
case class RasPredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val lastPC = slave Flow(UInt(config.valen bits)) // time base
        val nextBase = master Flow(UInt(config.valen bits)) // 1-latency
        val branchInfo = out(BranchInfo(config)) // 1-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config)))
        
        val flush = in(Bool())
    }
    val nextBase = UInt(config.valen bits)
    val lastPC = UInt(config.valen bits)
    lastPC := io.lastPC.payload
    val pred_valid = Reg(Bool()) init(False)
    pred_valid := io.lastPC.valid
    
    val callTable = Mem(RasTBundle(config), wordCount = config.rasTableSize)
    val retTable = Mem(RasTBundle(config), wordCount = config.rasTableSize)
    val rasStack = RasStack(config) // return address stack
    
    def hash_tag(pc: UInt): UInt = {
        var hash = U(0, 8 bits)
        for (i <- 0 until 4) {
            hash = hash ^ pc(i * 8 + 7 downto i * 8)
        }
        hash
    }
    
// ------------------------------- predict -------------------------------
    val callHit = Bool()
    val retHit = Bool()
    val pushData = U(0, config.rasStackWidth bits)
    val pushFlag = Bool()
    val popFlag = Bool()
    
    // read
    val index = lastPC(11 downto 2)
    val call_item = callTable.readSync(index) // 1-latency
    val ret_item = retTable.readSync(index) // 1-latency
    val tag = hash_tag(lastPC)
    
    callHit := call_item.valid && call_item.tag === tag
    retHit := ret_item.valid && ret_item.tag === tag
    
    when (callHit) {
        pushData := lastPC + 4
        pushFlag := True
        popFlag := False
    } .elsewhen(retHit) {
        pushData := U(0)
        pushFlag := False
        popFlag := True
    } .otherwise {
        pushData := U(0)
        pushFlag := False
        popFlag := False
    }

    rasStack.io.pushen := pushFlag
    rasStack.io.wdata := pushData
    rasStack.io.popen := popFlag
    nextBase := popFlag ? rasStack.io.rtop | lastPC + 4

    io.nextBase.valid := pred_valid && retHit
    io.nextBase.payload := nextBase
    io.branchInfo.predictTarget := nextBase
    io.branchInfo.predictTaken := retHit
    io.branchInfo.predictJumpInst := retHit
    io.branchInfo.GHR := U(0).resized
    // io.branchInfo.rasSP := U(0).resized
    // io.branchInfo.rasTop := U(0).resized
    // io.branchInfo.rasCount := U(0).resized
    
// ------------------------------- update -------------------------------
    val updateCallFetchMask = Bits(config.fetchWidth bits)
    val updateRetFetchMask = Bits(config.fetchWidth bits)

    (0 until config.fetchWidth).map(i => {
        // updateCallFetchMask(i) := io.updateInfo(i).valid && io.updateInfo(i).isCallInst
        // updateRetFetchMask(i) := io.updateInfo(i).valid && io.updateInfo(i).isRetInst
    })
    
    (0 until config.fetchWidth).map(i => {
        val updatePC = io.updateInfo(i).pc
        val index = updatePC(11 downto 2)
        val tag = hash_tag(updatePC)
        when(updateCallFetchMask(i)) {
            callTable.write(index, RasTBundle(config).setVal(True, tag))
        }
        when(updateRetFetchMask(i)) {
            retTable.write(index, RasTBundle(config).setVal(True, tag))
        }
    })

// ------------------------------- flush -------------------------------
    rasStack.io.flush := io.flush
    // rasStack.io.reTop := io.updateInfo(0).payload.branchInfo.rasTop
    // rasStack.io.reCount := io.updateInfo(0).payload.branchInfo.rasCount
    // rasStack.io.reSp := io.updateInfo(0).payload.branchInfo.rasSP
}