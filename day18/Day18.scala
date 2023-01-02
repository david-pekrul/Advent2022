package day18

import day18.VoxelType.VoxelType
import helpers.Helpers

import scala.collection.mutable

object Day18 {
  def main(args: Array[String]): Unit = {
    val rawInput = Helpers.readFile("day18/day18.txt").map(parse)

    def rockVoxels = rawInput.map(coord => {
      val c = Coord(coord._1, coord._2, coord._3)
      c -> VoxelType.Rock
    }).toMap

    val boundingBox = BoundingBox(
      minX = rawInput.map(_._1).min - 1,
      maxX = rawInput.map(_._1).max + 1,
      minY = rawInput.map(_._2).min - 1,
      maxY = rawInput.map(_._2).max + 1,
      minZ = rawInput.map(_._3).min - 1,
      maxZ = rawInput.map(_._3).max + 1
    )

    val filledOutVoxels = Map[Coord, VoxelType]().withDefault(_ => VoxelType.Air) ++ rockVoxels

    val part1 = getSurfaceArea(filledOutVoxels, boundingBox)
    println(s"Part 1: $part1")
    val part2 = getSurfaceArea2(filledOutVoxels, boundingBox)
    println(s"Part 2: $part2")
  }


  def parse(line: String): (Int, Int, Int) = {
    val regex = """(\d+),(\d+),(\d+)""".r

    implicit def sToI(s: String): Int = Integer.parseInt(s)

    val regex(x, y, z) = line
    (x, y, z)
  }

  def getSurfaceArea(allVoxels: Map[Coord, VoxelType], boundingBox: BoundingBox): Int = {
    allVoxels.filter(_._2 == VoxelType.Rock).map(_._1).foldLeft(0)((surfaceAreaAcc, rockCoord) => {
      val neighborTypesMap = rockCoord.getNeighbors(boundingBox).map(n => {
        n -> allVoxels(n)
      })
      val addedSurfaceArea = neighborTypesMap.filter(_._2 == VoxelType.Air)
      surfaceAreaAcc + addedSurfaceArea.size
    })
  }

  def getSurfaceArea2(allVoxels: Map[Coord, VoxelType], boundingBox: BoundingBox): Int = {

    val outsideAir = getOutsideAirCoords(allVoxels, boundingBox)

    allVoxels.filter(_._2 == VoxelType.Rock).map(_._1).foldLeft(0)((surfaceAreaAcc, rockCoord) => {
      val neighborTypesMap = rockCoord.getNeighbors(boundingBox).map(n => {
        n -> allVoxels(n)
      })
      val addedSurfaceArea = neighborTypesMap.filter(n => {
        outsideAir.contains(n._1)
      })
      surfaceAreaAcc + addedSurfaceArea.size
    })
  }

  def getOutsideAirCoords(allVoxels: Map[Coord, VoxelType], boundingBox: BoundingBox): Set[Coord] = {

    val startingCoord = Coord(boundingBox.minX, boundingBox.minY, boundingBox.minZ)

    def _bfs(currentLayer: Set[Coord], outsideAirCoords: Set[Coord]): Set[Coord] = {
      if (currentLayer.isEmpty) {
        return outsideAirCoords
      }

      val updatedOutsideAirCoords: Set[Coord] = outsideAirCoords ++ currentLayer

      val nextLayer = currentLayer.flatMap(outsideAirCoord => {
        outsideAirCoord.getNeighbors(boundingBox).filter(c => allVoxels(c) == VoxelType.Air)
      }).filter(n => {
        !outsideAirCoords.contains(n)
      })

      _bfs(nextLayer, updatedOutsideAirCoords)

    }

    _bfs(Set(startingCoord), Set())
  }
}

case class BoundingBox(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int) {
  def contains(c: Coord): Boolean = {
    minX <= c.x && c.x <= maxX && minY <= c.y && c.y <= maxY && minZ <= c.z && c.z <= maxZ
  }
}

case class Coord(x: Int, y: Int, z: Int) {
  def getNeighbors(boundingBox: BoundingBox): Seq[Coord] = {
    Seq(
      Coord(x, y, z + 1),
      Coord(x, y, z - 1),
      Coord(x, y + 1, z),
      Coord(x, y - 1, z),
      Coord(x + 1, y, z),
      Coord(x - 1, y, z)
    ).filter(boundingBox.contains)
  }

}

object VoxelType extends Enumeration {
  type VoxelType = Value
  val Rock, Air = Value
}


