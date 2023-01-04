package day19

import helpers.Helpers

import scala.collection.mutable

object Day19 {
  def main(args: Array[String]): Unit = {
    val startingStates = Helpers.readFile("day19/test.txt").map(parse)


    //    val processed = startingStates.headOption.map(s => {
    //      s._1 -> run(s._1, s._2, s._3, s._4)
    //    })
    val processed = startingStates.map(s => {
      s._1 -> runDFS(s._1, s._2, s._3, s._4, s._5)
    })

    val part1 = processed.map(x => x._1 * x._2).sum
    println(s"Part 1: $part1")


    val part2 = startingStates.take(3).map(s => {
      runDFS(s._1, s._2, s._3, s._4, s._5, minuteLimit = 32)
    }).reduce(_ * _)

    println(s"Part 2: $part2")

  }

  def run(id: Int, startingState: RobotArmyState, factory: RobotFactory, thresholds: RobotThresholds, minuteLimit: Int = 24) = {

    def _bfs(minute: Int, currentStates: Set[RobotArmyState]): Int = {
      val start = System.currentTimeMillis()
      if (minute > minuteLimit) {
        //ran out of time, lets figure out the max
        val maxGeodes = currentStates.toSeq.map(_.resources.geodes).max
        return id * maxGeodes
      }

      val nextStates = currentStates.map(currentState => {
        //List of Robots to add after gathering resources
        val possibleBuildOptions = factory.getBuildOptions(currentState.resources)
        val newlyGatheredResources = currentState.robotArmy.gatherResources()
        val nextStates = possibleBuildOptions
          .map(p => (p._1, p._2.combine(newlyGatheredResources))) //add in the newly gathered resources for each possible build path
          .map(e => {
            RobotArmyState(e._2, currentState.robotArmy.combine(e._1))
          })
          .filter(s => {
            s.robotArmy.oreRobots <= thresholds.oreRobotThreshold && s.robotArmy.clayRobots <= thresholds.clayRobotThreshold && s.robotArmy.obsidianRobots <= thresholds.obsidianRobotThreshold
          })
        nextStates
      }).flatten

      //filter the nextStates to figure out which ones can't even produce the same number of Geodes as others in the set
      //      val minutesRemaining = minuteLimit - minute
      //      val maxNumberOfGeodeRobots = nextStates.map(_.robotArmy.geodeRobots).max
      //      val guaranteedGeodesCutoff = minutesRemaining * maxNumberOfGeodeRobots //there is a state that can guarantee at least get this many without building any more geode robots

      val end = System.currentTimeMillis()
      val size = nextStates.size
      println(s"Minute: $minute:\t$size\t\t${(end - start) / 1000.0}s")


      _bfs(minute + 1, nextStates)
    }

    _bfs(1, Set(startingState))

  }

  def runDFS(id: Int, startingState: RobotArmyState, factory: RobotFactory, thresholds: RobotThresholds, robotCosts: Seq[RobotCost], minuteLimit: Int = 24) = {

    var counter = 0
    var currentMax = 0

    def _dfs(minute: Int, currentState: RobotArmyState, robotBuildOrder: Seq[(Int,RobotArmyState)]): Int = {

//      counter +=1
//      if(counter % 100 == 0){
//        println(counter)
//      }

      if (minute > minuteLimit) {
        //ran out of time, lets figure out the max
        //        counter+= 1
        //        if(counter % 10000 ==0){
        //          println(counter)
        //        }
        val numberOfGeodes = currentState.resources.geodes
        if(numberOfGeodes > currentMax){
          currentMax = numberOfGeodes
        }
        return numberOfGeodes
      }

      if(thresholds.exceededBy(currentState.robotArmy)){
        return 0
      }


      val newlyGatheredResources = currentState.robotArmy.gatherResources()
      val nextRobotBuildOptionsAndMinutes = factory.getBuildOptions2(currentState, robotCosts)
      val gatheredValues = nextRobotBuildOptionsAndMinutes.map { case (nextRobotCost, minutesTaken) => {

        if(minutesTaken >= (minuteLimit - minute)){
          //not enough time to build a new robot and have it collect anything
          //can still use the current robots to gather for the remaining time
          val resourcesAfterGatheringForMinutes = newlyGatheredResources.scale(minuteLimit - minute).combine(currentState.resources)
          resourcesAfterGatheringForMinutes.geodes
        } else {
          val resourcesAfterGatheringForMinutes = newlyGatheredResources.scale(minutesTaken+1).combine(currentState.resources)
          val (newRobot, remainingResources) = nextRobotCost.build(resourcesAfterGatheringForMinutes)
          val fullArmy = newRobot.combine(currentState.robotArmy)
          val nextState = RobotArmyState(remainingResources, fullArmy)
          val nextRobotBuildOrder = robotBuildOrder :+ (minute+minutesTaken+1,nextState)
          _dfs(minute + minutesTaken + 1, nextState, nextRobotBuildOrder)
        }
      }
      }

      gatheredValues.max
    }

    val result = _dfs(0, startingState, Seq())
    println(id,result)
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

    def buildOreRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      //^^ Incorrect! After a few minutes, we can gather enough resources in one minute to make multiples!
      val maxToMake = r.ore / oreRobotCostOre
      (0 to maxToMake).map(quantity => {
        (RobotArmy.newOreRobot.scale(quantity), r.use(useOre = quantity * oreRobotCostOre))
      })
    }

    def buildClayRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      //^^ Incorrect! After a few minutes, we can gather enough resources in one minute to make multiples!
      val maxToMake = r.ore / clayRobotCostOre
      (0 to maxToMake).map(quantity => {
        (RobotArmy.newClayRobot.scale(quantity), r.use(useOre = quantity * clayRobotCostOre))
      })
    }

    def buildObsidianRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      //^^ Incorrect! After a few minutes, we can gather enough resources in one minute to make multiples!
      val maxToMake = Math.min(r.ore / obsidianRobotCostOre, r.clay / obsidianRobotCostClay)
      (0 to maxToMake).map(quantity => {
        (RobotArmy.newObsidianRobot.scale(quantity), r.use(useOre = quantity * obsidianRobotCostOre, useClay = quantity * obsidianRobotCostClay))
      })
    }

    def buildGeodeRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      //^^ Incorrect! After a few minutes, we can gather enough resources in one minute to make multiples!
      val maxToMake = Math.min(r.ore / geodeRobotCostOre, r.obsidian / geodeRobotCostObsidian)
      (0 to maxToMake).map(quantity => {
        val n = (RobotArmy.newGeodeRobot.scale(quantity), r.use(useOre = quantity * geodeRobotCostOre, useClay = quantity * geodeRobotCostObsidian))
        n
      })
    }

    val oreRobotCost = RobotCost(oreCost = oreRobotCostOre, clayCost = 0, obsidianCost = 0, RobotArmy.newOreRobot)
    val clayRobotCost = RobotCost(oreCost = clayRobotCostOre, clayCost = 0, obsidianCost = 0, RobotArmy.newClayRobot)
    val obsidianRobotCost = RobotCost(oreCost = obsidianRobotCostOre, clayCost = obsidianRobotCostClay, obsidianCost = 0, RobotArmy.newObsidianRobot)
    val geodeRobotCost = RobotCost(oreCost = geodeRobotCostOre, clayCost = 0, obsidianCost = geodeRobotCostObsidian, RobotArmy.newGeodeRobot)

    val robotCosts = Seq(oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost)

    //    val oreProductionThreshold = 0 + oreRobotCostOre + clayRobotCostOre + obsidianRobotCostOre + geodeRobotCostOre

    val robotFactory = RobotFactory(buildOreRobotFunc, buildClayRobotFunc, buildObsidianRobotFunc, buildGeodeRobotFunc)

    val startingState = RobotArmyState(Resources(0, 0, 0, 0), RobotArmy(1, 0, 0, 0))

    (sToI(id), startingState, robotFactory, thresholds, robotCosts)
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
    val next = Resources(ore*scalar,clay*scalar,obsidian*scalar,geodes*scalar)
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

    val minutesToGetClay = if(robotArmyState.robotArmy.clayRobots == 0 && neededClay > 0){
      return 100
    } else {
      Math.ceil((1.0 * neededClay) / robotArmyState.robotArmy.clayRobots).intValue()
    }


    val minutesToGetObsidian = if(robotArmyState.robotArmy.obsidianRobots == 0 && neededObsidian > 0) {
      return 100
    } else {
      Math.ceil((1.0 * neededObsidian) / robotArmyState.robotArmy.obsidianRobots).intValue()
    }

    val allMinutes = Seq(minutesToGetOre, minutesToGetClay, minutesToGetObsidian)

    allMinutes.max
  }
}

case class RobotFactory(
                         buildOre: Resources => Seq[(RobotArmy, Resources)],
                         buildClay: Resources => Seq[(RobotArmy, Resources)],
                         buildObsidian: Resources => Seq[(RobotArmy, Resources)],
                         buildGeode: Resources => Seq[(RobotArmy, Resources)]
                       ) {
  def getBuildOptions(inputResources: Resources): Set[(RobotArmy, Resources)] = {

    //for the given input Resources, find all the options for building new robots AND remaining resources
    //keep in mind, there will always be the option to NOT build a robot
    val possibleResults = buildOre(inputResources) ++ buildClay(inputResources) ++ buildGeode(inputResources) ++ buildObsidian(inputResources)
    val distinctOptions = possibleResults.toSet
    distinctOptions
  }

  def getBuildOptions2(robotArmyState: RobotArmyState, robotCosts: Seq[RobotCost]) = {
    robotCosts.map(robotCost => {
      (robotCost, robotCost.minutesToGet(robotArmyState))
    })
  }
}
