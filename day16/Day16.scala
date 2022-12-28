package day16

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()

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

    /*  val optimalPath1Deux = ValveNetwork.findPaths2(30, startingValve, valves, slimNetwork, false)

      println(s"Part 1 deux optimal path: ${optimalPath1Deux}")
      println(s"Part 1 deux: ${ValveNetwork.calculateScore(optimalPath1Deux)}")

  */
    val optimalScore1_2 = ValveNetwork.findPaths3(30, startingValve, valves, slimNetwork, multiPath = false)
    println(s"Part 1_2: ${optimalScore1_2}")
    println("-----------------------------")


    //    val optimalPath2 = ValveNetwork.findPaths2(26, startingValve, valves, slimNetwork, true)
    //
    //    println(s"Optimal Path 2: ${optimalPath2}")
    //    println(s"Part 2: ${ValveNetwork.calculateScore(optimalPath2)}")

    val optimalScore3 = ValveNetwork.findPaths3(26, startingValve, valves, slimNetwork, multiPath = true)
    println(s"Part 2: ${optimalScore3}")

    val end = System.currentTimeMillis()
    println(s"\t[time: ${(end - start) / 1000.0}s]")
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
      val timeAfterOpeningValve = minutesRemaining - timeToOpenValve(nextValve)
      val updatedPath = pathSoFar :+ nextValve.setOpenTime(timeAfterOpeningValve)

      val allNeighborsAndCosts = slimNetwork.get(nextValve.id).get.toSeq.map(n => (allValveDict.get(n._1).get, n._2))

      val visitedNeighborIds = pathSoFar.map(_.id).toSet
      val unvisitedNeighbors = allNeighborsAndCosts.filter(n => {
        !visitedNeighborIds.contains(n._1.id)
      })

      if (unvisitedNeighbors.isEmpty) {
        //we have run out of valves to open.
        //We don't know if the valves were opened in the most optimal order though!
        return Seq(updatedPath.tail) //tail to drop the leading AA valve
      }

      val furtherPaths = unvisitedNeighbors.map(neighborAndCost => {
        val (neighborValve, neighborCost) = neighborAndCost
        _step(updatedPath, neighborValve, timeAfterOpeningValve - neighborCost)
      }).flatten

      furtherPaths
    }

    _step(Seq(startValve), startValve, minutes)
  }

  def findPaths3(minutes: Int, startValve: Valve, allValves: Set[Valve], slimNetwork: Map[String, Map[String, Int]], multiPath: Boolean): Int = {
    val findStart = System.currentTimeMillis()
    val allValveDict = allValves.map(x => (x.id -> x)).toMap
    val pairCostMap = slimNetwork.keys.map(k1 => {
      val innerMap = slimNetwork.get(k1).get
      innerMap.map(kv2 => {
        (k1, kv2._1) -> kv2._2
      })
    }).flatten.toMap

    def _step3(scoresSoFar: Int, minutesRemaining: (Int, Int), currentLocation: (String, String), unvisitedValves: Seq[String]): Int = {
      if (unvisitedValves.isEmpty) {
        return scoresSoFar
      }

      unvisitedValves.to(LazyList).map(unvisitedValveId => {
        val unvisitedValve = allValveDict.get(unvisitedValveId).get

        val leftCostToMove = pairCostMap.get((currentLocation._1, unvisitedValveId)).get
        val rightCostToMove = pairCostMap.get((currentLocation._2, unvisitedValveId)).get

        val leftMinutesValveOpenFor = minutesRemaining._1 - leftCostToMove - 1
        val rightMinutesValveOpenFor = minutesRemaining._2 - rightCostToMove - 1

        val updatedUnvisited = unvisitedValves.filterNot(x => x.equals(unvisitedValveId))

        val bestLeftScore = if (leftMinutesValveOpenFor <= 0) {
          scoresSoFar
        } else {
          val leftScore = scoresSoFar + (leftMinutesValveOpenFor) * unvisitedValve.flowRate
          _step3(leftScore, (leftMinutesValveOpenFor, minutesRemaining._2), (unvisitedValveId, currentLocation._2), updatedUnvisited)
        }

        val bestRightScore = if (rightMinutesValveOpenFor <= 0 || multiPath == false) { //multipath = false => don't add anything to the right side
          scoresSoFar
        } else {
          val rightScore = scoresSoFar + (rightMinutesValveOpenFor) * unvisitedValve.flowRate
          _step3(rightScore, (minutesRemaining._1, rightMinutesValveOpenFor), (currentLocation._1, unvisitedValveId), updatedUnvisited)
        }

        //If we added this valve next, the best we can do is given by the max of either path
        Math.max(bestLeftScore, bestRightScore)
      }).reduce(Math.max)
    }

    val result = _step3(0, (minutes, minutes), (startValve.id, startValve.id), slimNetwork.keys.filterNot(_.equals("AA")).toSeq)

    val findEnd = System.currentTimeMillis()
    println(s"\t[findTime: ${(findEnd - findStart) / 1000.0}s]")
    result
  }

  def findPaths2(minutes: Int, startValve: Valve, allValves: Set[Valve], slimNetwork: Map[String, Map[String, Int]], multiPath: Boolean): (Seq[Valve], Seq[Valve]) = {
    val allValveDict = allValves.map(x => (x.id -> x)).toMap
    val slimKeys = slimNetwork.keys.toSet

    /*Recursive */
    def _step2(pathsSoFar: (ValvePath, ValvePath), visitedValveIds: Set[String]): (Seq[Valve], Seq[Valve]) = {

      val unvisitedValves = slimKeys.diff(visitedValveIds)

      if (unvisitedValves.isEmpty) {
        Counter.increaseC1()
        return (pathsSoFar._1.path, pathsSoFar._2.path)
      }

      def addValveToPath(unvisitedValve: Valve, vp: ValvePath) = {
        def valveOpenTime = travelCost(vp.path.last, unvisitedValve) + timeToOpenValve(unvisitedValve)

        if (vp.minutesRemaining - valveOpenTime <= 0) {
          None
        } else {
          Some(ValvePath(vp.path :+ unvisitedValve.setOpenTime(vp.minutesRemaining - valveOpenTime), vp.minutesRemaining - valveOpenTime))
        }
      }

      def createPathPairs(unvisitedValve: Valve) = {
        def leftGoesToValve = (
          addValveToPath(unvisitedValve, pathsSoFar._1),
          Some(pathsSoFar._2)
        )

        def rightGoesToValve = (
          Some(pathsSoFar._1),
          addValveToPath(unvisitedValve, pathsSoFar._2),
        )

        val nextPaths = if (multiPath) {
          Set((leftGoesToValve, visitedValveIds + unvisitedValve.id), (rightGoesToValve, visitedValveIds + unvisitedValve.id))
        } else {
          Set((leftGoesToValve, visitedValveIds + unvisitedValve.id))
        }

        val result = nextPaths
          .filter(x => {
            x._1._1.isDefined && x._1._2.isDefined
          })
          .map(x => {
            ((x._1._1.get, x._1._2.get), x._2)
          })

        result
      }

      val updatedPaths = unvisitedValves.toSeq.map(unvisitedValveId => {
        def unvisitedValve = allValveDict.get(unvisitedValveId).get

        createPathPairs(unvisitedValve)
      }).flatten

      if (updatedPaths.isEmpty) {
        //no way to add another valve in the given remaining time
        Counter.increaseC1()
        return (pathsSoFar._1.path, pathsSoFar._2.path)
      }

      val optimalPath = updatedPaths.foldLeft((Int.MinValue, (Seq[Valve](), Seq[Valve]())))((acc, next) => {
        val furtherOptimalPath = _step2(next._1, next._2)
        val score = calculateScore(furtherOptimalPath)
        if (score > acc._1) {
          (score, furtherOptimalPath)
        } else {
          acc
        }
      })

      if (optimalPath._1 == Int.MinValue) {
        println("bummer")
      }

      optimalPath._2
    } //end _step

    def travelCost(source: Valve, target: Valve): Int = {
      val sourceMapOpt = slimNetwork.get(source.id)
      if (sourceMapOpt.isEmpty) {
        throw new RuntimeException("unknown source")
      }
      val costOpt = sourceMapOpt.get.get(target.id)
      if (costOpt.isEmpty) {
        throw new RuntimeException("unknown target")
      }
      costOpt.get
    }

    val startingPaths = (ValvePath(Seq(startValve), minutes), ValvePath(Seq(startValve), minutes))
    _step2(startingPaths, Set(startValve.id))
  }

  def timeToOpenValve(v: Valve): Int = {
    if (v.flowRate == 0) {
      0
    } else {
      1
    }
  }

  case class ValvePath(path: Seq[Valve], minutesRemaining: Int)

  def calculateScore(path: Seq[Valve]): Int = {
    path.map(v => {
      v.flowRate * v.openedMinute
    }).sum
  }

  def calculateScore(paths: (Seq[Valve], Seq[Valve])): Int = {
    calculateScore(paths._1) + calculateScore(paths._2)
  }

}

object Counter {
  var c1 = 0

  def increaseC1(): Unit = {
    c1 += 1
    if (c1 % 100000 == 0) {
      println(s"C1 $c1")
    }
  }
}