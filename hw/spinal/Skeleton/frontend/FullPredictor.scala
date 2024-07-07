package Skeleton.frontend

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class FullPredictor(config: CPUConfig) extends Component {
    val io = new Bundle {
        val pc = Vec.fill(config.fetchWidth)(slave Flow (UInt(config.valen bits))) // time base
        val npc = Vec.fill(config.fetchWidth)(master Flow (UInt(config.valen bits))) // 1-latency
        val branchInfo = out(Vec.fill(config.fetchWidth)(BranchInfo(config))) // 1-latency
        val updateInfo = Vec.fill(config.retireWidth)(slave Flow (BPUUpdateBundle(config)))
        
        val GHR = in(UInt(config.ghrWidth bits))
    }

    val fetchMask = Bits(config.fetchWidth bits)
    val nextBase = UInt(config.valen bits)
    val lastPCIdx = UInt(log2Up(config.fetchWidth) bits)
    lastPCIdx := OHToUInt(OHMasking.last(fetchMask))
    nextBase := io.pc(lastPCIdx).payload + 4

    (0 until config.fetchWidth).map(i => {
        fetchMask(i) := io.pc(i).valid
    })

// ------------------------------------------------------------------------------------------
    // TODO: 位选信号参数化, 考虑在 CPUConfig 中添加 offset.
    // branch target buffer; 1-bit valid, 8-bit tag, 32-bit target
    val BTB = Mem(UInt(config.btbWidth bits), wordCount = config.btbSize) init (Seq.fill(config.btbSize)(
      U(0, config.bhtWidth bits)
    ))
    // global history register
    val GHR = UInt(config.ghrWidth bits)
    GHR := io.GHR
    // branch history table;  2-bit saturating counter
    val BHT = Mem(UInt(config.bhtWidth bits), wordCount = config.bhtSize) init (Seq.fill(config.bhtSize)(
      U(1, config.bhtWidth bits)
    ))
    // pattern history table; 3-bit saturating counter, 8-bit tag, 2-bit useful
    val PHT = scala.Array.fill(config.phtNum)(Mem(UInt(config.phtWidth bits), wordCount = config.phtSize))

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

    val bht_pred = Vec.fill(config.fetchWidth)(Bool())

    val pht_tag = Vec.fill(config.fetchWidth)(Vec.fill(config.phtNum)(UInt(config.phtTagWidth bits)))
    val pht_pred = Vec.fill(config.fetchWidth)(Vec.fill(config.phtNum)(Bool()))

    val pred_addr = Vec.fill(config.fetchWidth)(UInt(config.valen bits))
    val pred_jump = Vec.fill(config.fetchWidth)(Bool())
    val pred_hit = Vec.fill(config.fetchWidth)(Bool())

    // predict
    (0 until config.fetchWidth).map(i => {
        bht_pred(i) :=
            BHT.readAsync(io.pc(i).payload(10 + 1 downto 2)) === 3 ||
                BHT.readAsync(io.pc(i).payload(10 + 1 downto 2)) === 2

        (0 until config.phtNum).map(j => {
            pht_tag(i)(j) := PHT(j).readAsync(hash_index(io.pc(i).payload, GHR, j))(9 downto 2)
            pht_pred(i)(j) :=
                PHT(j).readAsync(hash_index(io.pc(i).payload, GHR, j))(12 downto 10) === 7 ||
                    PHT(j).readAsync(hash_index(io.pc(i).payload, GHR, j))(12 downto 10) === 6 ||
                    PHT(j).readAsync(hash_index(io.pc(i).payload, GHR, j))(12 downto 10) === 5 ||
                    PHT(j).readAsync(hash_index(io.pc(i).payload, GHR, j))(12 downto 10) === 3
        })

        // 是 跳转指令
        when(
          BTB.readAsync(io.pc(i).payload(7 downto 2))(40) === True && BTB
              .readAsync(io.pc(i).payload(7 downto 2))(39 downto 32) === hash_tag(io.pc(i).payload)
        ) {
            when(bht_pred(i)) {
                pred_addr(i) := BTB.readAsync(io.pc(i).payload(7 downto 2))(31 downto 0)
                pred_jump(i) := True
            } otherwise {
                pred_addr(i) := io.pc(i).payload + 4
                pred_jump(i) := False
            }
            (0 until config.phtNum).map(j => {
                when(pht_tag(i)(j) === hash_tag(io.pc(i).payload)) {
                    when(pht_pred(i)(j)) {
                        pred_addr(i) := BTB.readAsync(io.pc(i).payload(7 downto 2))(31 downto 0)
                        pred_jump(i) := True
                    } otherwise {
                        pred_addr(i) := io.pc(i).payload + 4
                        pred_jump(i) := False
                    }
                }
            })
            pred_hit(i) := True
        } otherwise { // 不是跳转指令
            pred_addr(i) := io.pc(i).payload + 4
            pred_jump(i) := False
            pred_hit(i) := False
        }

    })

    // 更新 nextBase 和 GHR, nextBase 只有在跳转时才需要更新
    (config.fetchWidth - 1 until -1 by -1).map(i => {
        when(pred_hit(i)) {
            when(pred_jump(i)) {
                nextBase := pred_addr(i)
            }
        }
    })

    // 输出 npc 和 branchInfo
    (0 until config.fetchWidth).map(i => {
        io.npc(i).valid := True
        io.npc(i).payload := nextBase + i |<< log2Up(config.instLength / 8)
        // io.branchInfo(i).predictPC := pred_addr(i)
        // io.branchInfo(i).predictResult := pred_jump(i) && fetchMask(i)
        io.branchInfo(i).GHR := GHR
    })

// ------------------------------------------------------------------------------------------

    val upd_fetchMask = Bits(config.retireWidth bits)
    (0 until config.retireWidth).map(i => {
        upd_fetchMask(i) := io.updateInfo(i).valid && io.updateInfo(i).payload.isJumpInst
    })

    val upd_bht_pred = Vec.fill(config.retireWidth)(Bool())

    val upd_pht_tag = Vec.fill(config.retireWidth)(Vec.fill(config.phtNum)(UInt(config.phtTagWidth bits)))
    val upd_pht_pred = Vec.fill(config.retireWidth)(Vec.fill(config.phtNum)(Bool()))

    val upd_provider = Vec.fill(config.retireWidth)(UInt(3 bits))
    val upd_altpred = Vec.fill(config.retireWidth)(UInt(3 bits))
    val upd_provider_pred = Vec.fill(config.retireWidth)(Bool())
    val upd_altpred_pred = Vec.fill(config.retireWidth)(Bool())

    val upd_GHR = Vec.fill(config.retireWidth)(UInt(80 bits))

    // 更新
    (0 until config.retireWidth).map(i => {
        // 找 GHR
        upd_GHR(i) := io.updateInfo(i).payload.GHR

        // 找到 provider 和 altpred
        upd_bht_pred(i) := BHT.readSync(io.updateInfo(i).payload.pc(10 + 1 downto 2)) === 3 ||
            BHT.readSync(io.updateInfo(i).payload.pc(10 + 1 downto 2)) === 2

        (0 until config.phtNum).map(j => {
            upd_pht_tag(i)(j) := PHT(j).readSync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(9 downto 2)
            upd_pht_pred(i)(j) :=
                PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(12 downto 10) === 7 ||
                    PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(12 downto 10) === 6 ||
                    PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(12 downto 10) === 5 ||
                    PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(12 downto 10) === 3
        })

        // upd_provider 为有效的最优先的预测器, upd_alt 为有效的次优先对应的预测器
        upd_provider(i) := 0
        upd_altpred(i) := 0

        (0 until config.phtNum).map(j => {
            when(upd_pht_tag(i)(j) === hash_tag(io.updateInfo(i).payload.pc)) {
                upd_provider(i) := j + 1
            }
        })

        (0 until config.phtNum).map(j => {
            when(upd_pht_tag(i)(j) === hash_tag(io.updateInfo(i).payload.pc) && upd_provider(i) > j + 1) {
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
        when(upd_fetchMask(i)) {
            // 是否有记录, 没有记录则添加
            when(!BTB.readAsync(io.updateInfo(i).payload.pc(7 downto 2))(40)) {
                BTB.write(
                  io.updateInfo(i).payload.pc(7 downto 2),
                  U(1) @@ hash_tag(io.updateInfo(i).payload.pc) @@ io.updateInfo(i).payload.target
                )
            }

            when(!io.updateInfo(i).payload.predictFail) {
                when(upd_provider_pred =/= upd_altpred_pred) {
                    // 更新 provider 指向的预测器的 useful 字段和 saturating counter 字段
                    BHT.write(
                      address = io.updateInfo(i).payload.pc(10 + 1 downto 2),
                      data = BHT.readAsync(io.updateInfo(i).payload.pc(10 + 1 downto 2)) |<< 1 | io
                          .updateInfo(i)
                          .payload
                          .taken
                          .asUInt
                          .resized,
                      enable = upd_provider(i) === 0
                    )
                    (0 until config.phtNum).map(j => {
                        val next_counter = PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(
                          12 downto 10
                        ) |<< 1 | io.updateInfo(i).payload.taken.asUInt.resized
                        val next_useful =
                            PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(1 downto 0) +| 1
                        PHT(j).write(
                          hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j),
                          next_counter @@ U(0, config.phtTagWidth bits) @@ next_useful,
                          enable = upd_provider(i) === j + 1,
                          mask = Bits(config.phtCounterWidth bits).setAll() ## Bits(config.phtTagWidth bits)
                              .clearAll() ## Bits(config.phtUsefulWidth bits).setAll()
                        )
                    })
                } otherwise {
                    // 只更新 saturating counter
                    BHT.write(
                      address = io.updateInfo(i).payload.pc(10 + 1 downto 2),
                      data = BHT.readAsync(io.updateInfo(i).payload.pc(10 + 1 downto 2)) |<< 1 | io
                          .updateInfo(i)
                          .payload
                          .taken
                          .asUInt
                          .resized,
                      enable = upd_provider(i) === 0
                    )
                    (0 until config.phtNum).map(j => {
                        val next_counter = PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(
                          12 downto 10
                        ) |<< 1 | io.updateInfo(i).payload.taken.asUInt.resized
                        PHT(j).write(
                          hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j),
                          next_counter @@ U(0, config.phtTagWidth bits) @@ U(0, config.phtUsefulWidth bits),
                          enable = upd_provider(i) === j + 1,
                          mask = Bits(config.phtCounterWidth bits).setAll() ## Bits(config.phtTagWidth bits)
                              .clearAll() ## Bits(config.phtUsefulWidth bits).clearAll()
                        )
                    })
                }
                // 更新 altpred 指向的预测器的 saturating counter 字段
                BHT.write(
                  address = io.updateInfo(i).payload.pc(10 + 1 downto 2),
                  data = BHT.readAsync(io.updateInfo(i).payload.pc(10 + 1 downto 2)) |<< 1 | io
                      .updateInfo(i)
                      .payload
                      .taken
                      .asUInt
                      .resized,
                  enable = upd_altpred(i) === 0
                )
                (0 until config.phtNum).map(j => {
                    val next_counter = PHT(j).readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(
                      12 downto 10
                    ) |<< 1 | io.updateInfo(i).payload.taken.asUInt.resized
                    PHT(j).write(
                      hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j),
                      next_counter @@ U(0, config.phtTagWidth bits) @@ U(0, config.phtUsefulWidth bits),
                      enable = upd_altpred(i) === j + 1,
                      mask = Bits(config.phtCounterWidth bits).setAll() ## Bits(config.phtTagWidth bits)
                          .clearAll() ## Bits(config.phtUsefulWidth bits).clearAll()
                    )
                })
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
                          !alloc_find_front(j) && j > upd_provider(i) - 1 && PHT(j).readAsync(
                            hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j)
                          )(1 downto 0) === 0
                        ) {
                            alloc_find(j) := True
                            PHT(j).write(
                              hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j),
                              U(io.updateInfo(i).payload.taken, config.phtCounterWidth bits) @@ hash_tag(
                                io.updateInfo(i).payload.pc
                              ) @@ U(1, config.phtUsefulWidth bits),
                              enable = True
                            )
                        }
                    })
                    when(!alloc_find.orR) {
                        (0 until config.phtNum).map(j => {
                            val next_useful = PHT(j)
                                .readAsync(hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j))(1 downto 0) -| 1
                            PHT(j).write(
                              hash_index(io.updateInfo(i).payload.pc, upd_GHR(i), j),
                              U(0, config.phtCounterWidth + config.phtTagWidth bits) @@ next_useful,
                              enable = True,
                              mask = Bits(config.phtCounterWidth bits).clearAll() ## Bits(config.phtTagWidth bits)
                                  .clearAll() ## Bits(config.phtUsefulWidth bits).setAll()
                            )
                        })
                    }

                }
        }
        .elsewhen(io.updateInfo(i).valid) { // 非跳转指令
            // 更新 BTB
            BTB.write(io.updateInfo(i).payload.pc(7 downto 2), U(0, 41 bits))
        }
    })

}
