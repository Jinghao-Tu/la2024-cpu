package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

// * 用于函数调用和返回地址的栈结构预测器
// RAS 是一个寄存器堆实现的栈存储结构，它对 call 指令的下一条指令的地址进行记录，在 FTB 认为预测块会在 call 指令跳转时压栈，并在 FTB 认为预测块在 ret 指令跳转时弹出。每一项包含一个地址和一个计数器，当重复压栈同一个地址时，栈指针不变，计数器加一，用于处理程序中递归调用的情况。每次预测后，栈顶项和栈指针都会存入 FTQ 的存储结构，用于误预测时恢复
case class RasPredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow(UInt(config.valen bits))) // 0-latency!
        val npc = Vec.fill(config.fetchWidth)(master Flow(UInt(config.valen bits))) // 0-latency!
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config)))
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow(BPUUpdateBundle(config))) // 0-latency!
        
        val rasTop = out(UInt(config.wordLength bits))
        val rasSP = out(UInt(log2Up(config.rasStackDepth) bits))
    }
    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    nextBase := io.pc(lastPCIdx).payload + 4

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })
    
    val callTable = Mem(UInt(config.rasTableWidth bits), wordCount = config.rasTableSize)
    val retTable = Mem(UInt(config.rasTableWidth bits), wordCount = config.rasTableSize)
    val rasStack = RasStack(config) // return address stack
    io.rasTop <> rasStack.io.rtop
    io.rasSP <> rasStack.io.rsp
    
    def hash_tag(pc: UInt): UInt = {
        var hash = U(0, 8 bits)
        for (i <- 0 until 4) {
            hash = hash ^ pc(i * 8 + 7 downto i * 8)
        }
        hash
    }
    
// ------------------------------- predict -------------------------------
    val callHit = Vec.fill(config.fetchWidth)(Bool)
    val retHit = Vec.fill(config.fetchWidth)(Bool)
    val pushData = U(0, config.rasStackWidth bits)
    val pushFlag = Vec.fill(config.fetchWidth)(Bool)
    val popFlag = Vec.fill(config.fetchWidth)(Bool)
    
    (0 until config.fetchWidth).map(i => {
        val index = io.pc(i).payload(11 downto 2)
        val tag = hash_tag(io.pc(i).payload)
        callHit(i) := callTable.readAsync(index)(7 downto 0) === tag && callTable.readAsync(index)(8)
        retHit(i) := retTable.readAsync(index)(7 downto 0) === tag && retTable.readAsync(index)(8)
    })
    
    (config.fetchWidth - 1 until -1 by -1).map(i => {
        when (fetchMask(i)) {
            when(callHit(i)) {
                pushData := io.pc(i).payload + 1 |<< log2Up(config.instLength / 8)
                pushFlag(i) := True
                popFlag(i) := False
            } elsewhen(retHit(i)) {
                popFlag(i) := True
                pushFlag(i) := False
            }
        }
    })

    rasStack.io.pushen := 
    rasStack.io.wdata := pushData
    rasStack.io.popen := popFlag
    nextBase := rasStack.io.rtop(31 downto 0)

    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := io.pc(i).valid && popFlag
        io.npc(i).payload := nextBase + i |<< log2Up(config.instLength / 8)
        io.branchInfo(i).predictTarget := nextBase + i |<< log2Up(config.instLength / 8)
    })

// ------------------------------- update -------------------------------
    val updateCallFetchMask = Bits(config.fetchWidth bits)
    val updateRetFetchMask = Bits(config.fetchWidth bits)

    (0 until config.fetchWidth).map(i => {
        updateCallFetchMask(i) := io.updateInfo(i).valid && io.updateInfo(i).isCallInst
        updateRetFetchMask(i) := io.updateInfo(i).valid && io.updateInfo(i).isRetInst
    })
    
    (0 until config.fetchWidth).map(i => {
        val index = io.pc(i).payload(11 downto 2)
        val tag = hash_tag(io.pc(i).payload)
        when(updateCallFetchMask(i)) {
            callTable.write(index, True.asUInt @@ tag)
        }
        when(updateRetFetchMask(i)) {
            retTable.write(index, True.asUInt @@ tag)
        }
    })

}