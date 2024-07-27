package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

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

// ------------------------------------------------------------------------------------------
    // 位选信号参数化: change to bundle

    // branch target buffer; 1-bit valid, 8-bit tag, 32-bit target
    val BTB = Mem(BTBBundle(config), wordCount = config.btbSize)

    // global history register
    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR

    // branch history table;  2-bit saturating counter
    val BHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize)

    // pattern history table; 3-bit saturating counter, 8-bit tag, 2-bit useful
    val PHT = scala.Array.fill(config.phtNum)(
      Mem(PHTBundle(config), wordCount = config.phtSize)
    )

    def hash_index(pc: UInt, GHR: UInt, level: Int): UInt = {
        var hash = pc(10 + 1 downto 2)
        for (i <- 0 until 1 << (level - 1)) {
            hash = hash ^ GHR(i * 10 + 9 downto i * 10)
        }
        for (i <- 0 until 3) {
            hash = hash ^ pc(i * 10 + 11 downto i * 10 + 2)
        }
        hash
    }

    def hash_tag(pc: UInt): UInt = {
        var hash = U(0, 8 bits)
        for (i <- 0 until 4) {
            hash = hash ^ pc(i * 8 + 7 downto i * 8)
        }
        hash
    }

// ------------------------------------------------------------------------------------------

    val bht_pred = Bool()

    val pht_tag = Vec.fill(config.phtNum)(UInt(config.phtTagWidth bits))
    val pht_pred = Vec.fill(config.phtNum)(Bool())

    val pred_addr = UInt(config.valen bits)
    val pred_jump = Bool()
    val pred_hit = Bool()

    // read
    // val bht_item = BHT.readSync(hash_index(lastPC, GHR, 4)) // 1-latency
    val bht_item = BHT.readSync(U(0).resized) // 1-latency
    val pht_item = Vec.fill(config.phtNum)(PHTBundle(config)) // 1-latency
    (0 until config.phtNum).map(j => {
        pht_item(j) := PHT(j).readSync(hash_index(lastPC, GHR, j))
    })
    // val btb_item = BTB.readSync(lastPC(7 downto 2))
    val btb_item = BTB.readSync(U(0).resized)

    // predict whether to jump
    bht_pred := bht_item === 3 || bht_item === 2

    (0 until config.phtNum).map(j => {
        pht_tag(j) := pht_item(j).tag
        pht_pred(j) :=
            pht_item(j).counter === 7 || pht_item(j).counter === 6 || pht_item(j).counter === 5 || pht_item(j).counter === 3
    })

    // 是 跳转指令
    when(btb_item.valid && btb_item.tag === hash_tag(lastPC)) {
        when(bht_pred) {
            pred_addr := btb_item.target
            pred_jump := True
        } otherwise {
            pred_addr := lastPC + 4
            pred_jump := False
        }
        (0 until config.phtNum).map(j => {
            when(pht_tag(j) === hash_tag(lastPC)) {
                when(pht_pred(j)) {
                    pred_addr := btb_item.target
                    pred_jump := True
                } otherwise {
                    pred_addr := lastPC + 4
                    pred_jump := False
                }
            }
        })
        pred_hit := True
    } otherwise { // 不是跳转指令
        pred_addr := lastPC + 4
        pred_jump := False
        pred_hit := False
    }

    // 更新 nextBase
    nextBase := pred_addr

    // 输出 nextBase 和 branchInfo
    io.nextBase.valid := pred_valid // 1-latency
    io.nextBase.payload := nextBase // 1-latency
    io.branchInfo.predictTarget := pred_addr // 1-latency
    io.branchInfo.predictTaken := pred_jump  // 1-latency
    io.branchInfo.predictJumpInst := pred_hit // 1-latency
    io.branchInfo.GHR := U(0).resized
    io.branchInfo.pc := lastPC // 1-latency // ! no-needed

// ------------------------------------------------------------------------------------------

    val upd_retireMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        upd_retireMask(i) := io.updateInfo(i).valid
    })

    val upd_bht_pred = Vec.fill(config.retireWidth)(Bool())

    val upd_pht_tag = Vec.fill(config.retireWidth)(Vec.fill(config.phtNum)(UInt(config.phtTagWidth bits)))
    val upd_pht_pred = Vec.fill(config.retireWidth)(Vec.fill(config.phtNum)(Bool()))

    val upd_provider = Vec.fill(config.retireWidth)(UInt(3 bits))
    val upd_altpred = Vec.fill(config.retireWidth)(UInt(3 bits))
    val upd_provider_pred = Vec.fill(config.retireWidth)(Bool())
    val upd_altpred_pred = Vec.fill(config.retireWidth)(Bool())

    val upd_GHR = Vec.fill(config.retireWidth)(UInt(config.ghrWidth bits))

    
    val updInfoWire0 = io.updateInfo // 0-latency
    val updInfoReg1 = RegNext(updInfoWire0) // 1-latency

    // TODO: 可以改成握手, stage 写法
    // 更新
    (0 until config.retireWidth).map(i => {
        
    // stage 0
        // GHR
        upd_GHR(i) := updInfoWire0(i).payload.branchInfo.GHR

        // read
        // val upd_bht_item = BHT.readSync(hash_index(updInfoWire0(i).payload.pc, upd_GHR(i), 4)) // 1-latency
        val upd_bht_item = BHT.readSync(U(0).resized) // 1-latency
        val upd_pht_item = Vec.fill(config.phtNum)(PHTBundle(config))
        (0 until config.phtNum).map(j => {
            upd_pht_item(j) := PHT(j).readSync(hash_index(updInfoWire0(i).payload.pc, upd_GHR(i), j)) // 1-latency
        })
        // val upd_btb_item = BTB.readSync(updInfoWire0(i).payload.pc(7 downto 2)) // 1-latency
        val upd_btb_item = BTB.readSync(U(0).resized) // 1-latency

    // stage 1
        // 找到 provider 和 altpred
        upd_bht_pred(i) := upd_bht_item === 3 || upd_bht_item === 2
        (0 until config.phtNum).map(j => {
            upd_pht_tag(i)(j) := upd_pht_item(j).tag
            upd_pht_pred(i)(j) :=
                upd_pht_item(j).counter === 7 || upd_pht_item(j).counter === 6 || upd_pht_item(j).counter === 5 || upd_pht_item(j).counter === 3
        })

        // upd_provider 为有效的最优先的预测器, upd_alt 为有效的次优先对应的预测器
        upd_provider(i) := 0
        upd_altpred(i) := 0
        val upd_tag = hash_tag(updInfoWire0(i).payload.pc)
        
        (0 until config.phtNum).map(j => {
            when(upd_pht_tag(i)(j) === upd_tag && upd_pht_item(j).useful =/= 0) {
                upd_provider(i) := j + 1
            }
        })

        (0 until config.phtNum).map(j => {
            when(upd_pht_tag(i)(j) === upd_tag && upd_pht_item(j).useful =/= 0 && upd_provider(i) > j + 1) {
                upd_altpred(i) := j + 1
            }
        })

        upd_provider_pred(i) := upd_provider(i).mux(
            0 -> upd_bht_pred(i),
            default -> upd_pht_pred(i)((upd_provider(i) - 1).resized)
        )
        upd_altpred_pred(i) := upd_altpred(i).mux(
            0 -> upd_bht_pred(i),
            default -> upd_pht_pred(i)((upd_altpred(i) - 1).resized)
        )

        // 更新 TAGE
        // 是跳转指令
        when(upd_retireMask(i)) {
            // 是否有记录, 没有记录则添加
            when(!btb_item.valid) {
                BTB.write(
                //   updInfoReg1(i).payload.pc(7 downto 2),
                    U(0).resized,
                    BTBBundle(config).setVal(True, hash_tag(updInfoReg1(i).payload.pc), updInfoReg1(i).payload.branchResult.targetPC)
                )
            }

            // predictFail
            val predictFail = updInfoReg1(i).payload.branchResult.predictFail
            when(!predictFail) {
                when(upd_provider_pred =/= upd_altpred_pred) {
                    // 更新 provider 指向的预测器的 useful 字段和 saturating counter 字段
                    BHT.write(
                    //   address = updInfoReg1(i).payload.pc(10 + 1 downto 2),
                        address = U(0).resized,
                        data = BHT.readSync(updInfoReg1(i).payload.pc(10 + 1 downto 2)) |<< U(1) | updInfoReg1(i).payload.branchResult.taken.asUInt.resized,
                        enable = upd_provider(i) === 0
                    )
                    (0 until config.phtNum).map(j => {
                        val next_counter = pht_item(j).counter |<< U(1) | updInfoReg1(i).payload.branchResult.taken.asUInt.resized
                        val next_useful = pht_item(j).useful +| 1
                        PHT(j).write(
                          hash_index(updInfoReg1(i).payload.pc, upd_GHR(i), j),
                          PHTBundle(config).setVal(next_counter, upd_tag, next_useful),
                          enable = upd_provider(i) === j + 1,
                          mask = Bits(config.phtCounterWidth bits).setAll() ## Bits(config.phtTagWidth bits)
                              .clearAll() ## Bits(config.phtUsefulWidth bits).setAll()
                        )
                    })
                    // 更新 altpred 指向的预测器的 saturating counter 字段
                    BHT.write(
                    //   address = updInfoReg1(i).payload.pc(10 + 1 downto 2),
                      address = U(0).resized,
                      data = BHT.readSync(updInfoReg1(i).payload.pc(10 + 1 downto 2)) |<< 1 | updInfoReg1(i).payload.branchResult.taken.asUInt.resized,
                      enable = upd_altpred(i) === 0
                    )
                    (0 until config.phtNum).map(j => {
                        val next_counter = pht_item(j).counter |<< U(1) | updInfoReg1(i).payload.branchResult.taken.asUInt.resized
                        PHT(j).write(
                          hash_index(updInfoReg1(i).payload.pc, upd_GHR(i), j),
                          PHTBundle(config).setVal(
                            next_counter,
                            hash_tag(updInfoReg1(i).payload.pc),
                            pht_item(j).useful
                          ),
                          enable = upd_altpred(i) === j + 1,
                          mask = Bits(config.phtCounterWidth bits).setAll() ## Bits(config.phtTagWidth bits)
                              .clearAll() ## Bits(config.phtUsefulWidth bits).clearAll()
                        )
                    })
                } otherwise {
                    // 只更新 provider (其实也是 altpred) 的 saturating counter
                    BHT.write(
                    //   address = updInfoReg1(i).payload.pc(10 + 1 downto 2),
                      address = U(0).resized,
                      data = BHT.readSync(updInfoReg1(i).payload.pc(10 + 1 downto 2)) |<< U(1) | updInfoReg1(i).payload.branchResult.taken.asUInt.resized,
                      enable = upd_provider(i) === 0
                    )
                    (0 until config.phtNum).map(j => {
                        val next_counter = pht_item(j).counter |<< U(1) | updInfoReg1(i).payload.branchResult.taken.asUInt.resized
                        PHT(j).write(
                          hash_index(updInfoReg1(i).payload.pc, upd_GHR(i), j),
                          PHTBundle(config).setVal(
                            next_counter,
                            upd_tag,
                            pht_item(j).useful
                          ),
                          enable = upd_provider(i) === j + 1,
                          mask = Bits(config.phtCounterWidth bits).setAll() ## Bits(config.phtTagWidth bits)
                              .clearAll() ## Bits(config.phtUsefulWidth bits).clearAll()
                        )
                    })
                }
            }
                // 预测错误, 分配新的表项
                // 条件: a) GHR 宽度 > provider; b) 对应表项 useful 字段为0
                // 满足 a, b 条件的, 选择 位宽更小的 分配; 否则所有满足 a 条件的, useful 字段减 1
                .otherwise {
                    val alloc_find = Vec.fill(config.phtNum)(False)
                    val alloc_find_front = Vec.fill(config.phtNum)(Bool)
                    alloc_find_front(0) := False
                    (1 until config.phtNum).map(j => {
                        alloc_find_front(j) := alloc_find_front(j - 1) || alloc_find(j - 1)
                    })
                    (0 until config.phtNum).map(j => {
                        when(
                          !alloc_find_front(j) && j > upd_provider(i) - 1 && pht_item(j).useful === 0
                        ) {
                            alloc_find(j) := True
                            PHT(j).write(
                              hash_index(updInfoReg1(i).payload.pc, upd_GHR(i), j),
                                PHTBundle(config).setVal(
                                    U(updInfoReg1(i).payload.branchResult.taken, config.phtCounterWidth bits),
                                    hash_tag(updInfoReg1(i).payload.pc),
                                    U(1, config.phtUsefulWidth bits)
                                ),
                              enable = True
                            )
                        }
                    })
                    when(!alloc_find.orR) {
                        (0 until config.phtNum).map(j => {
                            val next_useful = pht_item(j).useful -| 1
                            PHT(j).write(
                              hash_index(updInfoReg1(i).payload.pc, upd_GHR(i), j),
                                PHTBundle(config).setVal(
                                    U(0, config.phtCounterWidth bits),
                                    hash_tag(updInfoReg1(i).payload.pc),
                                    next_useful
                                ),
                              enable = True,
                              mask = Bits(config.phtCounterWidth bits).clearAll() ## Bits(config.phtTagWidth bits)
                                  .clearAll() ## Bits(config.phtUsefulWidth bits).setAll()
                            )
                        })
                    }

                }
        }
            .elsewhen(updInfoReg1(i).valid) { // 非跳转指令
                // 更新 BTB
                // BTB.write(updInfoReg1(i).payload.pc(7 downto 2), BTBBundle(config).resetVal)
                BTB.write(U(0).resized, BTBBundle(config).resetVal)
            }
    })

}
