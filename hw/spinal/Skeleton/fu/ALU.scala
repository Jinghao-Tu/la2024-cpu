package Skeleton.fu

import spinal.core._
import spinal.lib._

import Skeleton.bundle._
import Skeleton.config._

case class ALU(fuType: SpinalEnumElement[FUType.type], config: CPUConfig) extends Component {
    require(fuType == FUType.counter || fuType == FUType.csr)
    val io = new Bundle {
        val input = slave Stream(ROFUBundle(fuType, config))
        val output = master Stream(FUWBBundle(config))
        val forward = master Flow(ForwardBundle(config)) // 0-latency!
        val wakeOut = master(Flow(Bits(config.prfIdxWidth bits))) // 0-latency!
        val csrWrite = (fuType == FUType.csr) generate out(CSRSwIOBundle(true, config))
    }
    io.input.ready := io.output.ready
    io.output.valid := io.input.valid
    io.output.payload.robIdx := io.input.payload.robIdx
    io.output.payload.prd := io.input.payload.prd
    io.output.payload.exceptionInfo := io.input.payload.exceptionInfo
    io.forward.valid := io.input.valid && (io.input.payload.prd =/= B(0).resized)
    io.forward.payload.idx := io.input.payload.prd
    io.forward.payload.payload := io.output.payload.data
    io.wakeOut.valid := io.input.valid
    io.wakeOut.payload := io.input.payload.prd
    // ALU
    val add   = io.input.payload.src1 + io.input.payload.src2
    val sub   = io.input.payload.src1 - io.input.payload.src2
    val slt   = (io.input.payload.src1.asSInt < io.input.payload.src2.asSInt).asUInt.resized
    val sltu  = (io.input.payload.src1 < io.input.payload.src2).asUInt.resized
    val eq    = (io.input.payload.src1 === io.input.payload.src2).asUInt.resized
    val nor   = ~(io.input.payload.src1 | io.input.payload.src2)
    val and   = io.input.payload.src1 & io.input.payload.src2
    val or    = io.input.payload.src1 | io.input.payload.src2
    val xor   = io.input.payload.src1 ^ io.input.payload.src2
    val sll   = io.input.payload.src1 |<< io.input.payload.src2(4 downto 0)
    val srl   = io.input.payload.src1 |>> io.input.payload.src2(4 downto 0)
    val sra   = (io.input.payload.src1.asSInt >> io.input.payload.src2(4 downto 0)).asUInt
    val passa = io.input.payload.src1
    val passb = io.input.payload.src2
    switch (io.input.payload.uop.aluOp) {
        is (ALUOp.add  ) { io.output.payload.data := add   }
        is (ALUOp.sub  ) { io.output.payload.data := sub   }
        is (ALUOp.slt  ) { io.output.payload.data := slt   }
        is (ALUOp.sltu ) { io.output.payload.data := sltu  }
        is (ALUOp.eq   ) { io.output.payload.data := eq    }
        is (ALUOp.nor  ) { io.output.payload.data := nor   }
        is (ALUOp.and  ) { io.output.payload.data := and   }
        is (ALUOp.or   ) { io.output.payload.data := or    }
        is (ALUOp.xor  ) { io.output.payload.data := xor   }
        is (ALUOp.sll  ) { io.output.payload.data := sll   }
        is (ALUOp.srl  ) { io.output.payload.data := srl   }
        is (ALUOp.sra  ) { io.output.payload.data := sra   }
        is (ALUOp.passa) { io.output.payload.data := passa }
        is (ALUOp.passb) { io.output.payload.data := passb }
    }
    // BRU
    io.output.payload.branchResult.targetPC := io.input.payload.src3 + io.input.payload.src4
    io.output.payload.branchResult.predictFail := testFailedPrediction()
    switch (io.input.payload.uop.bruOp) {
        is(BRUOp.add) { io.output.payload.branchResult.branchResult := True }
        is(BRUOp.cadd) { io.output.payload.branchResult.branchResult := io.output.payload.data(0) }
        is(BRUOp.ncadd) { io.output.payload.branchResult.branchResult := io.output.payload.data(0) }
        default { io.output.payload.branchResult.branchResult := False }
    }
    // CRU
    if (fuType == FUType.csr) {
        io.csrWrite.address := io.input.payload.src2.asBits.resized
        // Default value
        io.csrWrite.value := io.input.payload.src4.asBits
        io.csrWrite.wen := False
        switch(io.input.payload.uop.cruOp) {
            is(CRUOp.pass) {
                io.csrWrite.value := io.input.payload.src4.asBits
                io.csrWrite.wen := True
            }
            is(CRUOp.mask) {
                io.csrWrite.value := ((io.input.payload.src4 & io.input.payload.src3) | (io.input.payload.src1 & ~io.input.payload.src3)).asBits
                io.csrWrite.wen := True
            }
        }
    }

    def testFailedPrediction(): Bool = {
        // return (io.output.payload.branchResult.targetPC =/= io.input.payload.branchInfo.predictPC) && (io.output.payload.branchResult.branchResult & io.input.payload.branchInfo.predictResult) || (io.output.payload.branchResult.branchResult ^ io.input.payload.branchInfo.predictResult)
        return False
    }
}