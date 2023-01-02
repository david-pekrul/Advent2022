package day19

import helpers.Helpers

import scala.collection.mutable

object Day19 {
  def main(args: Array[String]): Unit = {
    val startingStates = Helpers.readFile("day19/day19.txt").map(parse)

    val processed = startingStates.map(s => {
      s._1 -> runDFS(s._1, s._2, s._3, s._4)
    })

    val part1 = processed.map(x => x._1 * x._2).sum
    println(s"Part 1: $part1")

    val part2 = startingStates.take(3).map(s => {
      runDFS(s._1, s._2, s._3, s._4, minuteLimit = 32)
    }).reduce(_ * _)

    println(s"Part 2: $part2")
  }

  def runDFS(id: Int, startingState: RobotArmyState, thresholds: RobotThresholds, robotCosts: Seq[RobotCost], minuteLimit: Int = 24) = {

    var currentMax = 0

    //the number of geodes at a given minute
    val memoMap: mutable.Map[Int, Int] = mutable.Map()

    def _dfs(minute: Int, currentState: RobotArmyState): Int = {

      memoMap.get(minute) match {
        case None => {
          memoMap.update(minute, currentState.resources.geodes)
        }
        case Some(existingGeodeCount) => {
          if (currentState.resources.geodes + 1 < existingGeodeCount) {
            //this path is too far behind in geode count than another path at this minute mark
            return 0
          }
          if (currentState.resources.geodes > existingGeodeCount) {
            memoMap.update(minute, currentState.resources.geodes)
          }
        }
      }

      if (thresholds.exceededBy(currentState.robotArmy)) {
        /*this is the magic for culling states!*/
        return 0
      }


      val newlyGatheredResources = currentState.robotArmy.gatherResources()
      val nextRobotBuildOptionsAndMinutes = RobotFactory.getBuildOptions2(currentState, robotCosts)
      val gatheredValues = nextRobotBuildOptionsAndMinutes.map { case (nextRobotCost, minutesTaken) => {

        if (minutesTaken >= (minuteLimit - minute)) {
          //not enough time to build a new robot and have it collect anything
          //can still use the current robots to gather for the remaining time
          val resourcesAfterGatheringForMinutes = newlyGatheredResources.scale(minuteLimit - minute).combine(currentState.resources)
          if (resourcesAfterGatheringForMinutes.geodes > currentMax) {
            currentMax = resourcesAfterGatheringForMinutes.geodes
          }
          resourcesAfterGatheringForMinutes.geodes
        } else {
          //the +1's are to account for the build time of this robot
          //the existing robots get to gather for the time it takes to gather the resources to start the build AND the 1 minute the build takes
          val resourcesAfterGatheringForMinutes = newlyGatheredResources.scale(minutesTaken + 1).combine(currentState.resources)
          val (newRobot, remainingResources) = nextRobotCost.build(resourcesAfterGatheringForMinutes)
          val fullArmy = newRobot.combine(currentState.robotArmy)
          val nextState = RobotArmyState(remainingResources, fullArmy)
          _dfs(minute + minutesTaken + 1, nextState)
        }
      }
      }

      gatheredValues.max
    }

    val result = _dfs(0, startingState)
    println(id, result)
    result
  }

  def parse(line: String) = {
    val regex = """Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.""".r
    val regex(id, oreRobotCostOre, clayRobotCostOre, obsidianRobotCostOre, obsidianRobotCostClay, geodeRobotCostOre, geodeRobotCostObsidian) = line

    implicit def sToI(s: String) = Integer.parseInt(s)

    val oreRobotThreshold: Int = Seq[Int](oreRobotCostOre, clayRobotCostOre, obsidianRobotCostOre, geodeRobotCostOre).max
    val clayRobotThreshold: Int = obsidianRobotCostClay
    val obsidianRobotThreshold: Int = geodeRobotCostObsidian
    val thresholds = RobotThresholds(oreRobotThreshold, clayRobotThreshold, obsidianRobotThreshold)

    val oreRobotCost = RobotCost(oreCost = oreRobotCostOre, clayCost = 0, obsidianCost = 0, RobotArmy.newOreRobot)
    val clayRobotCost = RobotCost(oreCost = clayRobotCostOre, clayCost = 0, obsidianCost = 0, RobotArmy.newClayRobot)
    val obsidianRobotCost = RobotCost(oreCost = obsidianRobotCostOre, clayCost = obsidianRobotCostClay, obsidianCost = 0, RobotArmy.newObsidianRobot)
    val geodeRobotCost = RobotCost(oreCost = geodeRobotCostOre, clayCost = 0, obsidianCost = geodeRobotCostObsidian, RobotArmy.newGeodeRobot)

    val robotCosts = Seq(oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost)

    val startingState = RobotArmyState(Resources(0, 0, 0, 0), RobotArmy(1, 0, 0, 0))

    (sToI(id), startingState, thresholds, robotCosts)
  }
}

case class RobotThresholds(oreRobotThreshold: Int, clayRobotThreshold: Int, obsidianRobotThreshold: Int) {
  def exceededBy(robotArmy: RobotArmy): Boolean = {
    robotArmy.oreRobots > oreRobotThreshold || robotArmy.clayRobots > clayRobotThreshold || robotArmy.obsidianRobots > obsidianRobotThreshold
  }
}

case class Resources(ore: Int, clay: Int, obsidian: Int, geodes: Int) {
  def use(useOre: Int = 0, useClay: Int = 0, useObsidian: Int = 0): Resources = {
    val next = Resources(ore - useOre, clay - useClay, obsidian - useObsidian, geodes)
    validate(next)
  }

  def scale(scalar: Int) = {
    val next = Resources(ore * scalar, clay * scalar, obsidian * scalar, geodes * scalar)
    validate(next)
  }

  def use(robotCost: RobotCost): Resources = {
    val next = Resources(ore - robotCost.oreCost, clay - robotCost.clayCost, obsidian - robotCost.obsidianCost, geodes)
    validate(next)
  }

  def validate(next: Resources): Resources = {
    if (next.ore < 0 || next.clay < 0 || next.obsidian < 0) {
      throw new RuntimeException("Negative Resources")
    }
    next
  }

  def combine(other: Resources): Resources = {
    val next = Resources(
      ore + other.ore,
      clay + other.clay,
      obsidian + other.obsidian,
      geodes + other.geodes
    )
    validate(next)
  }
}

case class RobotArmy(oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int) {
  def gatherResources(): Resources = {
    Resources(oreRobots, clayRobots, obsidianRobots, geodeRobots)
  }

  def combine(other: RobotArmy): RobotArmy = {
    RobotArmy(oreRobots + other.oreRobots, clayRobots + other.clayRobots, obsidianRobots + other.obsidianRobots, geodeRobots + other.geodeRobots)
  }

  def scale(scaler: Int) = {
    RobotArmy(oreRobots * scaler, clayRobots * scaler, obsidianRobots * scaler, geodeRobots * scaler)
  }
}

object RobotArmy {
  val empty = RobotArmy(0, 0, 0, 0)
  val newOreRobot = RobotArmy(1, 0, 0, 0)
  val newClayRobot = RobotArmy(0, 1, 0, 0)
  val newObsidianRobot = RobotArmy(0, 0, 1, 0)
  val newGeodeRobot = RobotArmy(0, 0, 0, 1)
}


case class RobotArmyState(resources: Resources, robotArmy: RobotArmy) {}

case class RobotCost(oreCost: Int, clayCost: Int, obsidianCost: Int, builtRobot: RobotArmy) {
  def build(resources: Resources): (RobotArmy, Resources) = {
    (builtRobot, resources.use(this))
  }

  def minutesToGet(robotArmyState: RobotArmyState): Int = {
    val neededOre = Math.max(oreCost - robotArmyState.resources.ore, 0)
    val neededClay = Math.max(clayCost - robotArmyState.resources.clay, 0)
    val neededObsidian = Math.max(obsidianCost - robotArmyState.resources.obsidian, 0)
    //    println(robotArmyState)

    val minutesToGetOre = Math.ceil((1.0 * neededOre) / robotArmyState.robotArmy.oreRobots).intValue()

    val minutesToGetClay = if (robotArmyState.robotArmy.clayRobots == 0 && neededClay > 0) {
      return 100
    } else {
      Math.ceil((1.0 * neededClay) / robotArmyState.robotArmy.clayRobots).intValue()
    }


    val minutesToGetObsidian = if (robotArmyState.robotArmy.obsidianRobots == 0 && neededObsidian > 0) {
      return 100
    } else {
      Math.ceil((1.0 * neededObsidian) / robotArmyState.robotArmy.obsidianRobots).intValue()
    }

    val allMinutes = Seq(minutesToGetOre, minutesToGetClay, minutesToGetObsidian)

    allMinutes.max
  }
}

object RobotFactory {
  def getBuildOptions2(robotArmyState: RobotArmyState, robotCosts: Seq[RobotCost]) = {
    robotCosts.map(robotCost => {
      (robotCost, robotCost.minutesToGet(robotArmyState))
    })
  }
}
