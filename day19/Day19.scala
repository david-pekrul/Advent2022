package day19

import helpers.Helpers

object Day19 {
  def main(args: Array[String]): Unit = {
    Helpers.readFile("day19/test.txt")
  }

  def parse(line: String) = {
    val regex = """Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.""".r
    val regex(id,oreRobotCotsOre, clayRobotCostOre, obsidianRobotCostOre, obsidianRobotCostClay, geodeRobotCostOre, geodeRobotCostObsidian) = line
    implicit def sToI(s: String) = Integer.parseInt(s)

    (id,oreRobotCotsOre, clayRobotCostOre, obsidianRobotCostOre, obsidianRobotCostClay, geodeRobotCostOre, geodeRobotCostObsidian)

    val startingResources = Resources(0,0,0,0)
    val startingRobots = RobotArmyState(0,RobotArmy(1,0,0,0))
    def buildOreRobotFunc(input: Resources) = {

    }

  }


}

case class Blueprint(id: Int)

case class Resources(ore: Int, clay: Int, obsidian: Int, geodes: Int)

case class RobotArmy(oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int)

case class RobotArmyState(minute: Int, robotArmy: RobotArmy)

case class RobotFactory(
                         buildOre: Resources => Int,
                         buildClay: Resources => Int,
                         buildObsidian: Resources => Int,
                         buildGeode: Resources => Int
                       ) {
  def getBuildOptions(inputResources: Resources): Seq[(RobotArmy,Resources)] = {
    //for the given input Resources, find all the options for building new robots AND remaining resources
    //keep in mind, there will always be the option to NOT build a robot
    ???
  }
}
