package Skeleton

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "user/src",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    onlyStdLogicVectorAtTopLevelIo = true
  )

  def sim = SimConfig.withConfig(spinal).withFstWave
}
