package day16

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    def rawLines = helpers.Helpers.readFile("day16/day16.txt")

    val (valves, network) = ValveNetwork.parse(rawLines)
    val startingValve = valves.filter(_.id.equals("AA")).head
    val (flowValves, slimNetwork) = ValveNetwork.slimNetwork(valves, network, startingValve)
//    println(flowValves.mkString("\r\n"))
//    println(slimNetwork.toSeq.mkString("\r\n"))


    val paths = ValveNetwork.findPaths(30, startingValve, valves, slimNetwork)
    val optimalPath = paths
      .map(p => (ValveNetwork.calculateScore(p), p))
      .reduce((a, b) => {
        if (a._1 > b._1) {
          a
        } else {
          b
        }
      })

    println(s"Part 1 optimal path: ${optimalPath._2}")
    println(s"Part 1: ${optimalPath._1}")
  }
}

case class Valve(id: String, flowRate: Int, openedMinute: Int = 0) {
  def setOpenTime(minute: Int): Valve = {
    Valve(id, flowRate, minute)
  }
}

object ValveNetwork {
  def parse(rawLines: Seq[String]) = {
    rawLines.map(parseLine).foldLeft(Set[Valve](), Map[String, Seq[String]]())((acc, next) => {
      (
        acc._1 + next._1,
        acc._2 ++ next._2
      )
    })
  }

  val valveIdAndFlowRegex = "Valve (\\w\\w) has flow rate=(\\d+);".r

  implicit def sToI(s: String): Int = Integer.parseInt(s)

  private def parseLine(line: String): (Valve, Map[String, Seq[String]]) = {
    val firstMatch = valveIdAndFlowRegex.findAllMatchIn(line).next()
    val valve = Valve(firstMatch.group(1), firstMatch.group(2))
    val linksTo = line.split(";")(1)
      .replace("tunnels lead to valves", "")
      .replace("tunnel leads to valve", "")
      .trim().split(",").map(_.trim).toSeq
    (valve, Map(valve.id -> linksTo))
  }

  def slimNetwork(valves: Set[Valve], map: Map[String, Seq[String]], startValve: Valve): (Set[Valve], Map[String, Map[String, Int]]) = {
    //slim the network down so we just have the non-zero flow rate valves, and the weighted min distances between just them

    val flowValves = valves.filter(_.flowRate > 0) + startValve

    val slimedValves = flowValves.map(flowValve => {
      bfs(flowValve, valves, map)
    })

    val mergedSlimValves = slimedValves.foldLeft(Map[String, Map[String, Int]]())((acc, next) => {
      acc ++ next
    })

    (flowValves, mergedSlimValves)
  }

  private def bfs(start: Valve, allValves: Set[Valve], originalMap: Map[String, Seq[String]]): Map[String, Map[String, Int]] = {

    val allValveDict = allValves.map(x => (x.id -> x)).toMap

    @tailrec
    def _bfs(currentLayer: Seq[String], currentDepth: Int, distances: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
      if (currentLayer.isEmpty) {
        return distances
      }
      val updatedDistances = currentLayer.foldLeft(distances)((acc, next) => {
        val nextValveOpt = allValveDict.get(next)
        if (nextValveOpt.isEmpty) {
          throw new RuntimeException(s"Missing Valve! $next")
        }
        acc.updated(
          start.id,
          acc.getOrElse(start.id, Map()).updated(next, currentDepth)
        )
      })

      val alreadyVisited = distances.get(start.id).getOrElse(Map()).keys.toSeq
      val neighbors = currentLayer.flatMap(id => originalMap.get(id)).flatten.toSet

      val nextLayer = (neighbors -- alreadyVisited).toSeq

      _bfs(nextLayer, currentDepth + 1, updatedDistances)
    }

    val allDistances = _bfs(Seq(start.id), 0, Map())
    //remove zero-flow valves

    allDistances.updated(start.id, allDistances.get(start.id).get.filter(target => {
      allValveDict.get(target._1).get.flowRate > 0 && !target._1.equals(start.id)
    }))

  }

  def findPaths(minutes: Int, startValve: Valve, allValves: Set[Valve], slimNetwork: Map[String, Map[String, Int]]): Seq[Seq[Valve]] = {

    val allValveDict = allValves.map(x => (x.id -> x)).toMap

    def _step(pathSoFar: Seq[Valve], nextValve: Valve, minutesRemaining: Int): Seq[Seq[Valve]] = {

      if (minutesRemaining <= 0) {
        return Seq(pathSoFar)
      }

      //don't waste time opening the starting 0-flow-rate valve
      val timeAfterOpeningValve = minutesRemaining - (if (nextValve.flowRate == 0) {
        0
      } else {
        1
      })
      val updatedPath = pathSoFar :+ nextValve.setOpenTime(timeAfterOpeningValve)

      val allNeighborsAndCosts = slimNetwork.get(nextValve.id).get.toSeq.map(n => (allValveDict.get(n._1).get, n._2))

      val visitedNeighborIds = pathSoFar.map(_.id).toSet
      val unvisitedNeighbors = allNeighborsAndCosts.filter(n => {
        !visitedNeighborIds.contains(n._1.id)
      })

      if (unvisitedNeighbors.isEmpty) {
        //we have run out of valves to open.
        //We don't know if the valves were opened in the most optimal order though!
        return Seq(updatedPath)
      }

      val furtherPaths = unvisitedNeighbors.map(neighborAndCost => {
        val (neighborValve, neighborCost) = neighborAndCost
        _step(updatedPath, neighborValve, timeAfterOpeningValve - neighborCost)
      }).flatten

      furtherPaths
    }

    _step(Seq(), startValve, minutes)
  }

  def calculateScore(path: Seq[Valve]): Int = {
    path.map(v => {
      v.flowRate * v.openedMinute
    }).sum
  }

}