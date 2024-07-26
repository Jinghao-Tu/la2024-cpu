package Skeleton.fu

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class MULU(config: CPUConfig) extends Component {
    require(config.wordLength == 32 || config.wordLength == 64, "Unsupported word length")
    val wakeNum = if (config.wordLength == 32 || config.debug) 3 else 4
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(FUType.mulu, config))
        val output = master Stream(FUWBBundle(config))
        val forward = master Flow(ForwardBundle(config)) // 0-latency!
        val wakeOut = Vec.fill(wakeNum)(master(Flow(Bits(config.prfIdxWidth bits)))) // 0-latency!
    }
    if (config.debug) {
        // Stage 1
        val stage12 = Stream(ROFUBundle(FUType.mulu, config))
        stage12 <-< io.input
        // Stage 2
        val stage23 = Stream(ROFUBundle(FUType.mulu, config))
        stage23 <-< stage12
        // Stage 3
        val resu = stage23.payload.src1 * stage23.payload.src2
        val ress = stage23.payload.src1.asSInt * stage23.payload.src2.asSInt
        val res = UInt(config.wordLength bits)
        io.forward.valid := stage23.valid && (stage23.payload.prd =/= B(0).resized)
        io.forward.payload.idx := stage23.payload.prd
        io.forward.payload.payload := res
        io.output.valid := stage23.valid
        stage23.ready := io.output.ready
        io.output.payload.robIdx := stage23.payload.robIdx
        io.output.payload.data := res
        io.output.payload.prd := stage23.payload.prd
        io.output.payload.branchResult := stage23.payload.branchResult
        io.output.payload.exceptionInfo := stage23.payload.exceptionInfo
        switch(stage23.payload.uop.muluOp) {
            is(MULUOp.mullo) { res := resu(config.wordLength-1 downto 0) }
            is(MULUOp.mulhi) { res := ress(config.wordLength*2-1 downto config.wordLength).asUInt }
            is(MULUOp.mulhiu) { res := resu(config.wordLength*2-1 downto config.wordLength) }
        }
        io.wakeOut(0).valid := io.input.valid
        io.wakeOut(0).payload := io.input.payload.prd
        io.wakeOut(1).valid := stage12.valid
        io.wakeOut(1).payload := stage12.payload.prd
        io.wakeOut(2).valid := stage23.valid
        io.wakeOut(2).payload := stage23.payload.prd
    } else {
        val multipliand = UInt(config.wordLength + 1 bits)
        val multiplier = UInt(config.wordLength + 2 bits)
        val sign1 = (io.input.payload.uop.muluOp === MULUOp.mulhiu) ? False | io.input.payload.src1.msb
        val sign2 = (io.input.payload.uop.muluOp === MULUOp.mulhiu) ? False | io.input.payload.src2.msb
        multipliand := (sign1 ## io.input.payload.src1).asUInt
        multiplier := ((sign2 #* 2) ## io.input.payload.src2).asUInt

        val pos1 = multipliand.msb ## multipliand
        val pos2 = (multipliand.expand |<< 1).asBits
        val neg1 = ~pos1
        val neg2 = neg1 |<< 1

        val partialProduct = Vec.fill(config.wordLength/2+1)(UInt(config.wordLength*2 bits)) // Let compiler prune unused signals
        (0 until config.wordLength/2+1).map(i => {
            val selector = if (i == 0) (multiplier(1 downto 0) ## U(0, 1 bits)) else multiplier(i*2+1 downto i*2-1).asBits
            val pp = selector.mux(
                1 -> pos1,
                2 -> pos1,
                3 -> pos2,
                4 -> neg2,
                5 -> neg1,
                6 -> neg1,
                default -> B(0).resized
            )
            val selectorPrev = if (i == 1) multiplier(1 downto 0) ## U(0, 1 bits) else if (i > 1) multiplier(i*2-1 downto i*2-3).asBits else null
            val fix = if (i == 0) null else Bits(2 bits)
            if (i > 0) {
                fix := selectorPrev.mux(
                    4 -> B(2, 2 bits),
                    5 -> B(1, 2 bits),
                    6 -> B(1, 2 bits),
                    default -> B(0, 2 bits)
                )
            }
            if (i == 0) {
                partialProduct(i) := (B(0, config.wordLength-5 bits) ## ~pp.msb ## (pp.msb #* 2) ## pp).asUInt
            } else if (i == config.wordLength/2) {
                partialProduct(i) := (~pp.msb ## pp ## fix ## B(0, config.wordLength-2 bits)).resize(config.wordLength*2 bits).asUInt
            } else {
                partialProduct(i) := (B(0, config.wordLength bits) ## B(1, 1 bits) ## ~pp.msb ## pp ## fix ## B(0, (i-1)*2 bits)).resize(config.wordLength*2 bits).asUInt
            }
        })
        val wideResult = UInt(config.wordLength*2 bits)
        if (config.multiplier == MultiplierType.bundle) { // Synthesizer seems not to be happy with this one
            val tree = WallaceTree(config.wordLength*2, config.wordLength/2+1)
            tree.io.input := partialProduct
            tree.io.valid := io.input.valid
            tree.io.ready := io.output.ready
            wideResult := tree.io.sum + tree.io.shiftedCOut
        } else { // Perhaps this have better space utilization, thus bringing lower path delay
            val tree = Array.fill(config.wordLength*2)(WallaceTreeSingle(config))
            val sum = UInt(config.wordLength*2 bits)
            val carry = UInt(config.wordLength*2 bits)
            (0 until config.wordLength*2).map(i => {
                sum(i) := tree(i).io.sum.asBool
                carry(i) := tree(i).io.carry.asBool
                if (i == 0) {
                    tree(i).io.cin := Vec.fill(tree(0).carryNum)(U(0, 1 bits))
                } else {
                    tree(i).io.cin := tree(i-1).io.cout
                }
                tree(i).io.valid := io.input.valid
                tree(i).io.ready := io.output.ready
                (0 until config.wordLength/2+1).map(j => {
                    tree(i).io.input(j) := partialProduct(j)(i).asUInt
                })
            })
            wideResult := sum + (carry |<< 1)
        }

        val stage1In = Stream(MULUPipelineBundle(config))
        val stage1Out = Stream(MULUPipelineBundle(config))
        val stage2In = Stream(MULUPipelineBundle(config))
        val stage2Out = Stream(MULUPipelineBundle(config))
        val stage3In = Stream(MULUPipelineBundle(config))
        val stage3Out = Stream(MULUPipelineBundle(config))
        stage1In >-> stage1Out
        stage2In >-> stage2Out
        stage3In >-> stage3Out
        stage1Out >> stage2In
        stage1In.valid := io.input.valid
        io.input.ready := stage1In.ready
        stage1In.payload.fetchHi := ~(io.input.payload.uop.muluOp === MULUOp.mullo)
        stage1In.payload.robIdx := io.input.payload.robIdx
        stage1In.payload.prd := io.input.payload.prd
        stage1In.payload.branchResult := io.input.payload.branchResult
        stage1In.payload.exceptionInfo := io.input.payload.exceptionInfo
        io.wakeOut(0).valid := io.input.valid
        io.wakeOut(0).payload := io.input.payload.prd
        io.wakeOut(1).valid := stage1Out.valid
        io.wakeOut(1).payload := stage1Out.payload.prd
        io.wakeOut(2).valid := stage2Out.valid
        io.wakeOut(2).payload := stage2Out.payload.prd
        if (config.wordLength == 32) {
            stage2Out.ready := io.output.ready
            io.output.valid := stage2Out.valid

            io.output.payload.robIdx := stage2Out.payload.robIdx
            io.output.payload.data := stage2Out.payload.fetchHi ? wideResult(config.wordLength*2-1 downto config.wordLength) | wideResult(config.wordLength-1 downto 0)
            io.output.payload.prd := stage2Out.payload.prd
            io.output.payload.branchResult := stage2Out.payload.branchResult
            io.output.payload.exceptionInfo := stage2Out.payload.exceptionInfo

            io.forward.valid := stage2Out.valid && (stage2Out.payload.prd =/= B(0).resized)
            io.forward.payload.idx := stage2Out.payload.prd
            io.forward.payload.payload := io.output.payload.data
        } else {
            stage2Out >> stage3In
            stage3Out.ready := io.output.ready
            io.output.valid := stage3Out.valid

            io.wakeOut(3).valid := stage3Out.valid
            io.wakeOut(3).payload := stage3Out.payload.prd

            io.output.payload.robIdx := stage3Out.payload.robIdx
            io.output.payload.data := stage3Out.payload.fetchHi ? wideResult(config.wordLength*2-1 downto config.wordLength) | wideResult(config.wordLength-1 downto 0)
            io.output.payload.prd := stage3Out.payload.prd
            io.output.payload.branchResult := stage3Out.payload.branchResult
            io.output.payload.exceptionInfo := stage3Out.payload.exceptionInfo

            io.forward.valid := stage3Out.valid && (stage3Out.payload.prd =/= B(0).resized)
            io.forward.payload.idx := stage3Out.payload.prd
            io.forward.payload.payload := io.output.payload.data
        }
    }
}

case class WallaceTreeSingle(config: CPUConfig) extends Component {
    require(config.wordLength == 32 || config.wordLength == 64)
    val carryNum = if (config.wordLength == 32) 14 else 30
    val io = new Bundle {
        val input = in(Vec.fill(config.wordLength/2+1)(UInt(1 bits)))
        val sum = out(UInt(1 bits))
        val carry = out(UInt(1 bits))
        val cin = in(Vec.fill(carryNum)(UInt(1 bits)))
        val cout = out(Vec.fill(carryNum)(UInt(1 bits)))
        val valid = in(Bool())
        val ready = in(Bool())
    }
    if (config.wordLength == 32) {
        val csa1 = Array.fill(5)(CSA32(1))
        val csa2 = Array.fill(4)(CSA32(1))
        val csa3 = Array.fill(2)(CSA32(1))
        val csa4 = Array.fill(2)(CSA32(1))
        val csa5 = CSA32(1)
        val csa6 = CSA32(1)
        (0 until 5).map(i => {
            (0 until 3).map(j => {
                csa1(i).io.src(j) := io.input(i*3+j)
            })
        })
        csa2(0).io.src(0) := csa1(0).io.sum
        csa2(0).io.src(1) := csa1(1).io.sum
        csa2(0).io.src(2) := csa1(2).io.sum
        csa2(1).io.src(0) := csa1(3).io.sum
        csa2(1).io.src(1) := csa1(4).io.sum
        csa2(1).io.src(2) := io.input(15)
        csa2(2).io.src(0) := io.input(16)
        csa2(2).io.src(1) := io.cin(0)
        csa2(2).io.src(2) := io.cin(1)
        csa2(3).io.src(0) := io.cin(2)
        csa2(3).io.src(1) := io.cin(3)
        csa2(3).io.src(2) := io.cin(4)
        val pipeline1In = Stream(Vec.fill(8)(UInt(1 bits)))
        val pipeline1Out = Stream(Vec.fill(8)(UInt(1 bits)))
        pipeline1In >-> pipeline1Out
        pipeline1In.valid := io.valid
        pipeline1In.payload(0) := csa2(0).io.sum
        pipeline1In.payload(1) := csa2(1).io.sum
        pipeline1In.payload(2) := csa2(2).io.sum
        pipeline1In.payload(3) := csa2(3).io.sum
        pipeline1In.payload(4) := io.cin(5)
        pipeline1In.payload(5) := io.cin(6)
        pipeline1In.payload(6) := io.cin(7)
        pipeline1In.payload(7) := io.cin(8)
        csa3(0).io.src(0) := pipeline1Out.payload(0)
        csa3(0).io.src(1) := pipeline1Out.payload(1)
        csa3(0).io.src(2) := pipeline1Out.payload(2)
        csa3(1).io.src(0) := pipeline1Out.payload(3)
        csa3(1).io.src(1) := pipeline1Out.payload(4)
        csa3(1).io.src(2) := pipeline1Out.payload(5)
        csa4(0).io.src(0) := csa3(0).io.sum
        csa4(0).io.src(1) := csa3(1).io.sum
        csa4(0).io.src(2) := pipeline1Out.payload(6)
        csa4(1).io.src(0) := pipeline1Out.payload(7)
        csa4(1).io.src(1) := io.cin(9)
        csa4(1).io.src(2) := io.cin(10)
        csa5.io.src(0) := csa4(0).io.sum
        csa5.io.src(1) := csa4(1).io.sum
        csa5.io.src(2) := io.cin(11)
        csa6.io.src(0) := csa5.io.sum
        csa6.io.src(1) := io.cin(12)
        csa6.io.src(2) := io.cin(13)
        val pipeline2In = Stream(Vec.fill(2)(UInt(1 bits)))
        val pipeline2Out = Stream(Vec.fill(2)(UInt(1 bits)))
        pipeline2In >-> pipeline2Out
        pipeline2In.valid := pipeline1Out.valid
        pipeline2Out.ready := io.ready
        pipeline1Out.ready := pipeline2In.ready
        pipeline2In.payload(0) := csa6.io.sum
        pipeline2In.payload(1) := csa6.io.cout
        io.sum := pipeline2Out.payload(0)
        io.carry := pipeline2Out.payload(1)
        io.cout( 0) := csa1(0).io.cout
        io.cout( 1) := csa1(1).io.cout
        io.cout( 2) := csa1(2).io.cout
        io.cout( 3) := csa1(3).io.cout
        io.cout( 4) := csa1(4).io.cout
        io.cout( 5) := csa2(0).io.cout
        io.cout( 6) := csa2(1).io.cout
        io.cout( 7) := csa2(2).io.cout
        io.cout( 8) := csa2(3).io.cout
        io.cout( 9) := csa3(0).io.cout
        io.cout(10) := csa3(1).io.cout
        io.cout(11) := csa4(0).io.cout
        io.cout(12) := csa4(1).io.cout
        io.cout(13) := csa5.io.cout
    } else {
        val csa1 = Array.fill(11)(CSA32(1))
        val csa2 = Array.fill(7)(CSA32(1))
        val csa3 = Array.fill(5)(CSA32(1))
        val csa4 = Array.fill(3)(CSA32(1))
        val csa5 = Array.fill(2)(CSA32(1))
        val csa6 = CSA32(1)
        val csa7 = CSA32(1)
        val csa8 = CSA32(1)
        (0 until 11).map(i => {
            (0 until 3).map(j => {
                csa1(i).io.src(j) := io.input(i*3+j)
            })
        })
        csa2(0).io.src(0) := csa1( 0).io.sum
        csa2(0).io.src(1) := csa1( 1).io.sum
        csa2(0).io.src(2) := csa1( 2).io.sum
        csa2(1).io.src(0) := csa1( 3).io.sum
        csa2(1).io.src(1) := csa1( 4).io.sum
        csa2(1).io.src(2) := csa1( 5).io.sum
        csa2(2).io.src(0) := csa1( 6).io.sum
        csa2(2).io.src(1) := csa1( 7).io.sum
        csa2(2).io.src(2) := csa1( 8).io.sum
        csa2(3).io.src(0) := csa1( 9).io.sum
        csa2(3).io.src(1) := csa1(10).io.sum
        csa2(3).io.src(2) := io.cin(0)
        csa2(4).io.src(0) := io.cin(1)
        csa2(4).io.src(1) := io.cin(2)
        csa2(4).io.src(2) := io.cin(3)
        csa2(5).io.src(0) := io.cin(4)
        csa2(5).io.src(1) := io.cin(5)
        csa2(5).io.src(2) := io.cin(6)
        csa2(6).io.src(0) := io.cin(7)
        csa2(6).io.src(1) := io.cin(8)
        csa2(6).io.src(2) := io.cin(9)
        val pipeline1In = Stream(Vec.fill(15)(UInt(1 bits)))
        val pipeline1Out = Stream(Vec.fill(15)(UInt(1 bits)))
        pipeline1In >-> pipeline1Out
        pipeline1In.valid := io.valid
        pipeline1In.payload( 0) := csa2(0).io.sum
        pipeline1In.payload( 1) := csa2(1).io.sum
        pipeline1In.payload( 2) := csa2(2).io.sum
        pipeline1In.payload( 3) := csa2(3).io.sum
        pipeline1In.payload( 4) := csa2(4).io.sum
        pipeline1In.payload( 5) := csa2(5).io.sum
        pipeline1In.payload( 6) := csa2(6).io.sum
        pipeline1In.payload( 7) := io.cin(10)
        pipeline1In.payload( 8) := io.cin(11)
        pipeline1In.payload( 9) := io.cin(12)
        pipeline1In.payload(10) := io.cin(13)
        pipeline1In.payload(11) := io.cin(14)
        pipeline1In.payload(12) := io.cin(15)
        pipeline1In.payload(13) := io.cin(16)
        pipeline1In.payload(14) := io.cin(17)
        csa3(0).io.src(0) := pipeline1Out.payload( 0)
        csa3(0).io.src(1) := pipeline1Out.payload( 1)
        csa3(0).io.src(2) := pipeline1Out.payload( 2)
        csa3(1).io.src(0) := pipeline1Out.payload( 3)
        csa3(1).io.src(1) := pipeline1Out.payload( 4)
        csa3(1).io.src(2) := pipeline1Out.payload( 5)
        csa3(2).io.src(0) := pipeline1Out.payload( 6)
        csa3(2).io.src(1) := pipeline1Out.payload( 7)
        csa3(2).io.src(2) := pipeline1Out.payload( 8)
        csa3(3).io.src(0) := pipeline1Out.payload( 9)
        csa3(3).io.src(1) := pipeline1Out.payload(10)
        csa3(3).io.src(2) := pipeline1Out.payload(11)
        csa3(4).io.src(0) := pipeline1Out.payload(12)
        csa3(4).io.src(1) := pipeline1Out.payload(13)
        csa3(4).io.src(2) := pipeline1Out.payload(14)
        csa4(0).io.src(0) := csa3(0).io.sum
        csa4(0).io.src(1) := csa3(1).io.sum
        csa4(0).io.src(2) := csa3(2).io.sum
        csa4(1).io.src(0) := csa3(3).io.sum
        csa4(1).io.src(1) := csa3(4).io.sum
        csa4(1).io.src(2) := io.cin(18)
        csa4(2).io.src(0) := io.cin(19)
        csa4(2).io.src(1) := io.cin(20)
        csa4(2).io.src(2) := io.cin(21)
        csa5(0).io.src(0) := csa4(0).io.sum
        csa5(0).io.src(1) := csa4(1).io.sum
        csa5(0).io.src(2) := csa4(2).io.sum
        csa5(1).io.src(0) := io.cin(22)
        csa5(1).io.src(1) := io.cin(23)
        csa5(1).io.src(2) := io.cin(24)
        val pipeline2In = Stream(Vec.fill(5)(UInt(1 bits)))
        val pipeline2Out = Stream(Vec.fill(5)(UInt(1 bits)))
        pipeline2In >-> pipeline2Out
        pipeline2In.valid := pipeline1Out.valid
        pipeline1Out.ready := pipeline2In.ready
        pipeline2In.payload(0) := csa5(0).io.sum
        pipeline2In.payload(1) := csa5(1).io.sum
        pipeline2In.payload(2) := io.cin(25)
        pipeline2In.payload(3) := io.cin(26)
        pipeline2In.payload(4) := io.cin(27)
        csa6.io.src(0) := pipeline2Out.payload(0)
        csa6.io.src(1) := pipeline2Out.payload(1)
        csa6.io.src(2) := pipeline2Out.payload(2)
        csa7.io.src(0) := csa6.io.sum
        csa7.io.src(1) := pipeline2Out.payload(3)
        csa7.io.src(2) := pipeline2Out.payload(4)
        csa8.io.src(0) := csa7.io.sum
        csa8.io.src(1) := io.cin(28)
        csa8.io.src(2) := io.cin(29)
        val pipeline3In = Stream(Vec.fill(2)(UInt(1 bits)))
        val pipeline3Out = Stream(Vec.fill(2)(UInt(1 bits)))
        pipeline3In >-> pipeline3Out
        pipeline3In.valid := pipeline2Out.valid
        pipeline3Out.ready := io.ready
        pipeline2Out.ready := pipeline3In.ready
        pipeline3In.payload(0) := csa8.io.sum
        pipeline3In.payload(1) := csa8.io.cout
        io.sum := pipeline3Out.payload(0)
        io.carry := pipeline3Out.payload(1)
        (0 until 11).map(i => {
            io.cout(i) := csa1(i).io.cout
        })
        (0 until 7).map(i => {
            io.cout(i+11) := csa2(i).io.cout
        })
        (0 until 5).map(i => {
            io.cout(i+18) := csa3(i).io.cout
        })
        (0 until 3).map(i => {
            io.cout(i+23) := csa4(i).io.cout
        })
        (0 until 2).map(i => {
            io.cout(i+26) := csa5(i).io.cout
        })
        io.cout(28) := csa6.io.cout
        io.cout(29) := csa7.io.cout
    }
}

case class WallaceTree(width: Int, size: Int) extends Component {
    val io = new Bundle {
        val input = in(Vec.fill(size)(UInt(width bits)))
        val sum = out(UInt(width bits))
        val shiftedCOut = out(UInt(width bits))
        val valid = in(Bool())
        val ready = in(Bool())
    }
    require(width == 64 || width == 128)
    if (width == 64) {
        val csa1 = Array.fill(5)(CSA32(width))
        val csa2 = Array.fill(4)(CSA32(width))
        val csa3 = Array.fill(2)(CSA32(width))
        val csa4 = Array.fill(2)(CSA32(width))
        val csa5 = CSA32(width)
        val csa6 = CSA32(width)
        (0 until 5).map(i => {
            (0 until 3).map(j => {
                csa1(i).io.src(j) := io.input(i*3+j)
            })
        })
        csa2(0).io.src(0) := csa1(0).io.sum
        csa2(0).io.src(1) := csa1(1).io.sum
        csa2(0).io.src(2) := csa1(2).io.sum
        csa2(1).io.src(0) := csa1(3).io.sum
        csa2(1).io.src(1) := csa1(4).io.sum
        csa2(1).io.src(2) := io.input(15)
        csa2(2).io.src(0) := io.input(16)
        csa2(2).io.src(1) := csa1(0).io.cout |<< 1
        csa2(2).io.src(2) := csa1(1).io.cout |<< 1
        csa2(3).io.src(0) := csa1(2).io.cout |<< 1
        csa2(3).io.src(1) := csa1(3).io.cout |<< 1
        csa2(3).io.src(2) := csa1(4).io.cout |<< 1
        val pipeline1In = Stream(Vec.fill(8)(UInt(width bits)))
        val pipeline1Out = Stream(Vec.fill(8)(UInt(width bits)))
        pipeline1In >-> pipeline1Out
        pipeline1In.valid := io.valid
        pipeline1In.payload(0) := csa2(0).io.sum
        pipeline1In.payload(1) := csa2(1).io.sum
        pipeline1In.payload(2) := csa2(2).io.sum
        pipeline1In.payload(3) := csa2(3).io.sum
        pipeline1In.payload(4) := csa2(0).io.cout |<< 1
        pipeline1In.payload(5) := csa2(1).io.cout |<< 1
        pipeline1In.payload(6) := csa2(2).io.cout |<< 1
        pipeline1In.payload(7) := csa2(3).io.cout |<< 1
        csa3(0).io.src(0) := pipeline1Out.payload(0)
        csa3(0).io.src(1) := pipeline1Out.payload(1)
        csa3(0).io.src(2) := pipeline1Out.payload(2)
        csa3(1).io.src(0) := pipeline1Out.payload(3)
        csa3(1).io.src(1) := pipeline1Out.payload(4)
        csa3(1).io.src(2) := pipeline1Out.payload(5)
        csa4(0).io.src(0) := csa3(0).io.sum
        csa4(0).io.src(1) := csa3(1).io.sum
        csa4(0).io.src(2) := pipeline1Out.payload(6)
        csa4(1).io.src(0) := pipeline1Out.payload(7)
        csa4(1).io.src(1) := csa3(0).io.cout |<< 1
        csa4(1).io.src(2) := csa3(1).io.cout |<< 1
        csa5.io.src(0) := csa4(0).io.sum
        csa5.io.src(1) := csa4(1).io.sum
        csa5.io.src(2) := csa4(0).io.cout |<< 1
        csa6.io.src(0) := csa5.io.sum
        csa6.io.src(1) := csa4(1).io.cout |<< 1
        csa6.io.src(2) := csa5.io.cout |<< 1
        val pipeline2In = Stream(Vec.fill(2)(UInt(width bits)))
        val pipeline2Out = Stream(Vec.fill(2)(UInt(width bits)))
        pipeline2In >-> pipeline2Out
        pipeline2In.valid := pipeline1Out.valid
        pipeline2Out.ready := io.ready
        pipeline1Out.ready := pipeline2In.ready
        pipeline2In.payload(0) := csa6.io.sum
        pipeline2In.payload(1) := csa6.io.cout |<< 1
        io.sum := pipeline2Out.payload(0)
        io.shiftedCOut := pipeline2Out.payload(1)
    } else {
        val csa1 = Array.fill(11)(CSA32(width))
        val csa2 = Array.fill(7)(CSA32(width))
        val csa3 = Array.fill(5)(CSA32(width))
        val csa4 = Array.fill(3)(CSA32(width))
        val csa5 = Array.fill(2)(CSA32(width))
        val csa6 = CSA32(width)
        val csa7 = CSA32(width)
        val csa8 = CSA32(width)
        (0 until 11).map(i => {
            (0 until 3).map(j => {
                csa1(i).io.src(j) := io.input(i*3+j)
            })
        })
        csa2(0).io.src(0) := csa1(0).io.sum
        csa2(0).io.src(1) := csa1(1).io.sum
        csa2(0).io.src(2) := csa1(2).io.sum
        csa2(1).io.src(0) := csa1(3).io.sum
        csa2(1).io.src(1) := csa1(4).io.sum
        csa2(1).io.src(2) := csa1(5).io.sum
        csa2(2).io.src(0) := csa1(6).io.sum
        csa2(2).io.src(1) := csa1(7).io.sum
        csa2(2).io.src(2) := csa1(8).io.sum
        csa2(3).io.src(0) := csa1(9).io.sum
        csa2(3).io.src(1) := csa1(10).io.sum
        csa2(3).io.src(2) := csa1(0).io.cout |<< 1
        csa2(4).io.src(0) := csa1(1).io.cout |<< 1
        csa2(4).io.src(1) := csa1(2).io.cout |<< 1
        csa2(4).io.src(2) := csa1(3).io.cout |<< 1
        csa2(5).io.src(0) := csa1(4).io.cout |<< 1
        csa2(5).io.src(1) := csa1(5).io.cout |<< 1
        csa2(5).io.src(2) := csa1(6).io.cout |<< 1
        csa2(6).io.src(0) := csa1(7).io.cout |<< 1
        csa2(6).io.src(1) := csa1(8).io.cout |<< 1
        csa2(6).io.src(2) := csa1(9).io.cout |<< 1
        val pipeline1In = Stream(Vec.fill(15)(UInt(width bits)))
        val pipeline1Out = Stream(Vec.fill(15)(UInt(width bits)))
        pipeline1In >-> pipeline1Out
        pipeline1In.valid := io.valid
        pipeline1In.payload( 0) := csa2( 0).io.sum
        pipeline1In.payload( 1) := csa2( 1).io.sum
        pipeline1In.payload( 2) := csa2( 2).io.sum
        pipeline1In.payload( 3) := csa2( 3).io.sum
        pipeline1In.payload( 4) := csa2( 4).io.sum
        pipeline1In.payload( 5) := csa2( 5).io.sum
        pipeline1In.payload( 6) := csa2( 6).io.sum
        pipeline1In.payload( 7) := csa1(10).io.cout |<< 1
        pipeline1In.payload( 8) := csa2( 0).io.cout |<< 1
        pipeline1In.payload( 9) := csa2( 1).io.cout |<< 1
        pipeline1In.payload(10) := csa2( 2).io.cout |<< 1
        pipeline1In.payload(11) := csa2( 3).io.cout |<< 1
        pipeline1In.payload(12) := csa2( 4).io.cout |<< 1
        pipeline1In.payload(13) := csa2( 5).io.cout |<< 1
        pipeline1In.payload(14) := csa2( 6).io.cout |<< 1
        csa3(0).io.src(0) := pipeline1Out.payload( 0)
        csa3(0).io.src(1) := pipeline1Out.payload( 1)
        csa3(0).io.src(2) := pipeline1Out.payload( 2)
        csa3(1).io.src(0) := pipeline1Out.payload( 3)
        csa3(1).io.src(1) := pipeline1Out.payload( 4)
        csa3(1).io.src(2) := pipeline1Out.payload( 5)
        csa3(2).io.src(0) := pipeline1Out.payload( 6)
        csa3(2).io.src(1) := pipeline1Out.payload( 7)
        csa3(2).io.src(2) := pipeline1Out.payload( 8)
        csa3(3).io.src(0) := pipeline1Out.payload( 9)
        csa3(3).io.src(1) := pipeline1Out.payload(10)
        csa3(3).io.src(2) := pipeline1Out.payload(11)
        csa3(4).io.src(0) := pipeline1Out.payload(12)
        csa3(4).io.src(1) := pipeline1Out.payload(13)
        csa3(4).io.src(2) := pipeline1Out.payload(14)
        csa4(0).io.src(0) := csa3(0).io.sum
        csa4(0).io.src(1) := csa3(1).io.sum
        csa4(0).io.src(2) := csa3(2).io.sum
        csa4(1).io.src(0) := csa3(3).io.sum
        csa4(1).io.src(1) := csa3(4).io.sum
        csa4(1).io.src(2) := csa3(0).io.cout |<< 1
        csa4(2).io.src(0) := csa3(1).io.cout |<< 1
        csa4(2).io.src(1) := csa3(2).io.cout |<< 1
        csa4(2).io.src(2) := csa3(3).io.cout |<< 1
        csa5(0).io.src(0) := csa4(0).io.sum
        csa5(0).io.src(1) := csa4(1).io.sum
        csa5(0).io.src(2) := csa4(2).io.sum
        csa5(1).io.src(0) := csa3(4).io.cout |<< 1
        csa5(1).io.src(1) := csa4(0).io.cout |<< 1
        csa5(1).io.src(2) := csa4(1).io.cout |<< 1
        val pipeline2In = Stream(Vec.fill(5)(UInt(width bits)))
        val pipeline2Out = Stream(Vec.fill(5)(UInt(width bits)))
        pipeline2In >-> pipeline2Out
        pipeline2In.valid := pipeline1Out.valid
        pipeline1Out.ready := pipeline2In.ready
        pipeline2In.payload(0) := csa5(0).io.sum
        pipeline2In.payload(1) := csa5(1).io.sum
        pipeline2In.payload(2) := csa5(0).io.cout |<< 1
        pipeline2In.payload(3) := csa5(1).io.cout |<< 1
        pipeline2In.payload(4) := csa4(2).io.cout |<< 1
        csa6.io.src(0) := pipeline2Out.payload(0)
        csa6.io.src(1) := pipeline2Out.payload(1)
        csa6.io.src(2) := pipeline2Out.payload(2)
        csa7.io.src(0) := csa6.io.sum
        csa7.io.src(1) := pipeline2Out.payload(3)
        csa7.io.src(2) := pipeline2Out.payload(4)
        csa8.io.src(0) := csa7.io.sum
        csa8.io.src(1) := csa6.io.cout |<< 1
        csa8.io.src(2) := csa7.io.cout |<< 1
        val pipeline3In = Stream(Vec.fill(2)(UInt(width bits)))
        val pipeline3Out = Stream(Vec.fill(2)(UInt(width bits)))
        pipeline3In >-> pipeline3Out
        pipeline3In.valid := pipeline2Out.valid
        pipeline3Out.ready := io.ready
        pipeline2Out.ready := pipeline3In.ready
        pipeline3In.payload(0) := csa8.io.sum
        pipeline3In.payload(1) := csa8.io.cout |<< 1
        io.sum := pipeline3Out.payload(0)
        io.shiftedCOut := pipeline3Out.payload(1)
    }
}

case class CSA32(width: Int) extends Component {
    val io = new Bundle {
        val src = in(Vec.fill(3)(UInt(width bits)))
        val sum = out(UInt(width bits))
        val cout = out(UInt(width bits))
    }
    io.sum := io.src(0) ^ io.src(1) ^ io.src(2)
    io.cout := (io.src(0) & io.src(1)) | ((io.src(0) ^ io.src(1)) & io.src(2))
}

case class MULUPipelineBundle(config: CPUConfig) extends Bundle {
    val fetchHi = Bool()
    val robIdx = Bits(config.robIdxWidth bits)
    val prd = Bits(config.prfIdxWidth bits)
    val branchResult = BranchResult(config)
    val exceptionInfo = ExceptionInfo()
}