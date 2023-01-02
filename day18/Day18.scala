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

    val filledOutVoxels = boundingBox.xRange().foldLeft(Map[Coord, VoxelType]())((xAcc, x) => {
      xAcc ++ boundingBox.yRange().foldLeft(xAcc)((yAcc, y) => {
        yAcc ++ boundingBox.zRange().foldLeft(yAcc)((zAcc, z) => {
          val c = Coord(x, y, z)
          zAcc.updated(c, rockVoxels.getOrElse(c, VoxelType.Air))
        })
      })
    })

    val part1 = getSurfaceArea(filledOutVoxels, boundingBox)
    println(s"Part 1: $part1")
  }


  def parse(line: String): (Int, Int, Int) = {
    val regex = """(\d+),(\d+),(\d+)""".r

    implicit def sToI(s: String): Int = Integer.parseInt(s)

    val regex(x, y, z) = line
    (x, y, z)
  }

  def getSurfaceArea(allVoxels: Map[Coord, VoxelType], boundingBox: BoundingBox): Int = {
    allVoxels.filter(_._2 == VoxelType.Rock).map(_._1).foldLeft(0)((surfaceAreaAcc,rockCoord) => {
      val neighborTypesMap = rockCoord.getNeighbors(boundingBox).map(n => {
        n -> allVoxels(n)
      })
      val addedSurfaceArea = neighborTypesMap.filter(_._2 == VoxelType.Air)
      surfaceAreaAcc + addedSurfaceArea.size
    })
  }
}

case class BoundingBox(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int) {
  def xRange() = (minX to maxX)

  def yRange() = (minY to maxY)

  def zRange() = (minZ to maxZ)

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


