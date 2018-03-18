package commando

import io.StdIn
import collection.immutable.HashSet


trait Loader {
  def loadInput = {
    var lines = Seq[String]()
    var line = StdIn.readLine
    while (line != null) {
      lines = lines :+ line
      line = StdIn.readLine
    }
    // val lines = Source.fromFile("zad_input.txt").getLines.zipWithIndex
    lines.zipWithIndex.foldLeft(Game(HashSet(), HashSet(), Set())) { (g, p) =>
      parseLine(p._2, g, p._1)
    }
  }
  
  def parseLine(y: Int, game: Game, line: String) = {
    line.iterator.zipWithIndex.foldLeft(game) { (g: Game, p: (Char, Int)) =>
      val c = C(p._2, y)
      p._1 match {
        case '#' => Game(g.walls + c, g.goals, g.drops)
        case 'S' => Game(g.walls, g.goals, g.drops + c)
        case 'G' => Game(g.walls, g.goals + c, g.drops)
        case 'B' => Game(g.walls, g.goals + c, g.drops + c)
        case _ => g
      }
    }
  }
}

object Main extends Loader {
  def main(args: Array[String]): Unit = {
    val game = loadInput
    val search = Astar(game)
    val path = search.run
    path.foreach(print)
    println
  }
}
