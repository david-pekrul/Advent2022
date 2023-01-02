package day19

import helpers.Helpers

object Day19 {
  def main(args: Array[String]): Unit = {
    val startingStates = Helpers.readFile("day19/test.txt").map(parse)


    val processed = startingStates.headOption.map(s => {
      s._1 -> run(s._1,s._2,s._3)
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
      val maxToMake = r.ore / oreRobotCotsOre
      if (maxToMake >= 1) {
        Seq((RobotArmy.empty, r), (RobotArmy.newOreRobot, r.use(useOre = oreRobotCotsOre)))
      } else {
        Seq((RobotArmy.empty, r))
      }
    }

    def buildClayRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      val maxToMake = r.ore / clayRobotCostOre
      if (maxToMake >= 1) {
        Seq((RobotArmy.empty, r), (RobotArmy.newClayRobot, r.use(useOre = clayRobotCostOre)))
      } else {
        Seq((RobotArmy.empty, r))
      }
    }

    def buildObsidianRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      val maxToMake = Math.min(r.ore / obsidianRobotCostOre, r.clay / obsidianRobotCostClay)
      if (maxToMake >= 1) {
        Seq((RobotArmy.empty, r), (RobotArmy.newObsidianRobot, r.use(useOre = oreRobotCotsOre)))
      } else {
        Seq((RobotArmy.empty, r))
      }
    }

    def buildGeodeRobotFunc(r: Resources) = {
      //max how many can be built?
      //if we can make 2 now, it means we chose not to make 1 earlier and let resources go idle, which is bad
      val maxToMake = Math.min(r.ore / geodeRobotCostOre, r.obsidian / geodeRobotCostObsidian)
      if (maxToMake >= 1) {
        Seq((RobotArmy.empty, r), (RobotArmy.newGeodeRobot, r.use(useOre = oreRobotCotsOre)))
      } else {
        Seq() //if we CAN build a Geode robot, we don't pass this opportunity
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
