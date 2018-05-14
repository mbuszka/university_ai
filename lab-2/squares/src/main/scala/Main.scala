import util.Random
import io.StdIn

case class Grid(filled: Vector[Vector[Int]], empty: Vector[Vector[Int]]) {
  val width = filled(0).sum + empty(0).sum
  val height = filled.size

  def moveInterval(row: Int, idx: Int, dir: Int) = {
    val old = empty(row)
    val emp = old.updated(idx, old(idx) + dir).updated(idx + 1, old(idx) - dir)
    Grid(filled, empty.updated(row, emp))
  }

  def getMoves(row: Int) = {
    for {
      (interval, i) <- filled(row).zipWithIndex
      b = empty(row)(i)
      a = empty(row)(i + 1)
      x = if (b > 0) Seq(moveInterval(row, i, -1)) else Seq()
      y = if (a > 0) Seq(moveInterval(row, i, 1)) else Seq()
      z <- x ++ y
    } yield z
  }

  def valueAt(row: Int, col: Int): Boolean = {
    def go(idx: Int, empty: Vector[Int], filled: Vector[Int]): Boolean = {
      if (idx < empty(0)) return false
      if (idx < empty(0) + filled(0)) return true
      return go(idx - empty(0) - filled(0), empty.tail, filled.tail)
    }
    go(col, empty(row), filled(row))
  }

  def getColumn(col: Int) = {
    for (row <- 0.to(height)) {
      valueAt(row, col)
    }
  }
}

object Grid {
  def apply(rowsDesc: Vector[Vector[Int]], cols: Int): Grid = {
    val empty = for {
      intervals <- rowsDesc
      splits = Game.generateSplits(cols, intervals).toVector
      split = splits(Random.nextInt(splits.size))
    } yield split
    Grid(rowsDesc, empty.toVector)
  }
}


case class Game(rowLengths: Vector[Vector[Int]], colLengths: Vector[Vector[Int]]) {
  def scoreColumn(col: Int, bits: Vector[Boolean]): Int = {
    val len = bits.size
    val intervals = colLengths(col)
    (for {
      ints <- Game.generateSplits(len, intervals)
      c = Game.scoreSingle(bits, colLengths(col), ints)
    } yield c).min
  }
}

object Game {
  def scoreSingle(col: Vector[Boolean], filled: Vector[Int], empty: Vector[Int]): Int  = {
    var s = 0
    var off = 0
    for ((f, i) <- filled.zipWithIndex) {
      val e = empty(i)
      for (j <- off.to(off + e)) {
        if (col(j)) s += 1
      }
      off += e
      for (j <- off.to(off + f)) {
        if (!col(j)) s += 1
      }
      off += f
    }
    for (j <- off.to(off + empty.last)) {
      if (!col(j)) s += 1
    }
    return s
  }

  def splitToSum(splitCount: Int, sum: Int): Iterator[Vector[Int]] = {
    if (splitCount <= 1) return Iterator(Vector(sum))
    for {
      x <- 0.to(sum).iterator
      xs <- splitToSum(splitCount - 1, sum - x)
    } yield x +: xs
  }

  def generateSplits(len: Int, intervals: Vector[Int]): Iterator[Vector[Int]] = {
    val intCount = intervals.size
    val freeSpace = len - intervals.sum - (intCount - 1)
    for {
      s <- Game.splitToSum(intCount + 1, freeSpace)
      ints = s.head +: ((s.tail.init.map(_ + 1) :+ s.last))
    } yield ints
  }
}


trait Loader {
  def load = {
    var line = StdIn.readLine
    val counts = line.trim.split(" ")
    val rows = counts(0).toInt
    val cols = counts(1).toInt
    val rowDesc = for {
      i <- 1.to(rows)
      line = StdIn.readLine
    } yield line.trim.split(" ").map(_.toInt).toVector
    val colDesc = for {
      i <- 1.to(cols)
      line = StdIn.readLine
    } yield line.trim.split(" ").map(_.toInt).toVector
    Game(rowDesc.toVector, colDesc.toVector)
  }
}

object Main extends App with Loader {
  val game = load
  val state = Grid(game.rowLengths, game.colLengths.size)
  println(state)
}
