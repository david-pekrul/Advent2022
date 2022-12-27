package day16

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    def rawLines = helpers.Helpers.readFile("day16/day16.txt")

    val (valves, network) = ValveNetwork.parse(rawLines)

    val (flowValves, slimNetwork) = ValveNetwork.slimNetwork(valves, network)
    println(flowValves.mkString("\r\n"))
    println(slimNetwork.toSeq.mkString("\r\n"))


  }
}

case class Valve(id: String, flowRate: Int, openedMinute: Int = 0)

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

  def slimNetwork(valves: Set[Valve], map: Map[String, Seq[String]]): (Set[Valve], Map[String, Map[String, Int]]) = {
    //slim the network down so we just have the non-zero flow rate valves, and the weighted min distances between just them

    val flowValves = valves.filter(_.flowRate > 0)

    val slimedValves = flowValves.map(flowValve => {
      bfs(flowValve, valves, map)
    })

    val mergedSlimValves = slimedValves.foldLeft(Map[String, Map[String, Int]]())((acc, next) => {
      acc ++ next
    })

    (flowValves, mergedSlimValves)
  }

  def bfs(start: Valve, allValves: Set[Valve], originalMap: Map[String, Seq[String]]): Map[String, Map[String, Int]] = {

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

    allDistances.updated(start.id,allDistances.get(start.id).get.filter(target => {
      allValveDict.get(target._1).get.flowRate > 0 && !target._1.equals(start.id)
    }))

  }
}