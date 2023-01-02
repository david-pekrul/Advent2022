package day19

import helpers.Helpers

object Day19 {
  def main(args: Array[String]): Unit = {
    val startingStates = Helpers.readFile("day19/test.txt").map(parse)


    val processed = startingStates.headOption.map(s => {
      s._1 -> run(s._1, s._2, s._3)
    })

    println(processed)


  }

  def run(id: Int, startingState: RobotArmyState, factory: RobotFactory, minuteLimit: Int = 24) = {

    def _bfs(minute: Int, currentStates: Set[RobotArmyState]): Int = {

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
        nextStates
      }).flatten

      //filter the nextStates to figure out which ones can't even produce the same number of Geodes as others in the set

      _bfs(minute + 1, nextStates)
    }

    _bfs(0, Set(startingState))

  }

  def parse(line: String) = {
    val regex = """Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.""".r
    val regex(id, oreRobotCotsOre, clayRobotCostOre, obsidianRobotCostOre, obsidianRobotCostClay, geodeRobotCostOre, geodeRobotCostObsidian) = line

    implicit def sToI(s: String) = Integer.parseInt(s)

    def buildOreRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      //^^ Incorrect! After a few minutes, we can gather enough resources in one minute to make multiples!
      val maxToMake = r.ore / oreRobotCotsOre
      (0 to maxToMake).map(quantity => {
        (RobotArmy.newOreRobot.scale(quantity), r.use(useOre = quantity * oreRobotCotsOre))
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
      if (maxToMake == 0) {
        Seq() //never pass on an opportunity to build one of these
      } else {
        if(maxToMake > 1){
          println(s"MaxToMake Geode: $maxToMake")
        }
        (1 to maxToMake).map(quantity => {
          (RobotArmy.newObsidianRobot.scale(quantity), r.use(useOre = quantity * geodeRobotCostOre, useClay = quantity * geodeRobotCostObsidian))
        })
      }
    }

    val robotFactory = RobotFactory(buildOreRobotFunc, buildClayRobotFunc, buildObsidianRobotFunc, buildGeodeRobotFunc)

    val startingState = RobotArmyState(Resources(0, 0, 0, 0), RobotArmy(1, 0, 0, 0))

    (sToI(id), startingState, robotFactory)
  }
}

case class Resources(ore: Int, clay: Int, obsidian: Int, geodes: Int) {
  def use(useOre: Int = 0, useClay: Int = 0, useObsidian: Int = 0): Resources = {
    Resources(ore - useOre, clay - useClay, obsidian - useObsidian, geodes)
  }

  def combine(other: Resources): Resources = {
    Resources(ore + other.ore, clay + other.clay, obsidian + other.obsidian, geodes + other.geodes)
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


case class RobotArmyState(resources: Resources, robotArmy: RobotArmy)

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
}
