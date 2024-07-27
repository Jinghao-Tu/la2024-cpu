package Skeleton

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
    // targetDirectory = "hw/gen",
    targetDirectory = "/home/jht213/Projects/nscscc-team-la32r/perf_test/soc_axi_perf/rtl/myCPU",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetKind = SYNC,
      resetActiveLevel = LOW
    ),
    onlyStdLogicVectorAtTopLevelIo = true
  )

  def sim = SimConfig.withConfig(spinal).withFstWave
}
