import scala.collection.mutable.{ Map, HashMap, Queue }
import scala.io.Source

trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

case class Pos(x: Int, y: Int) {
  def onBoard = {
    1 <= x && x <= 8 &&
    1 <= y && y <= 8
  }
  
  def move(d: Direction) = {
    val p = d match {
      case Up => Pos(x, y + 1)
      case Down => Pos(x, y - 1)
      case Left => Pos(x - 1, y)
      case Right => Pos(x + 1, y)
    }
    if (p.onBoard) Some(p) else None
  }

  def neighbourhood = {
    for {
      i <- -1 to 1
      j <- -1 to 1
      p = Pos(x + i, y + j)
      if (p.onBoard && p != this)
    } yield p
  }
}

trait Color {
  def next: Color
}

case object Black extends Color {
  def next = White
}

case object White extends Color {
  def next = Black
}

case class State(wt: Pos, wk: Pos, bk: Pos) {
  def isValid = {
    wt != wk &&
    wt != bk &&
    wk != bk &&
    bk.neighbourhood.forall { n => n != wk }
  }

  def check = {
    wt.x == bk.x || wt.y == bk.y
  }

  def checkMate = check && moves(Black).isEmpty

  def moves(c: Color) = c match {
    case Black => bk.neighbourhood.map { State(wt, wk, _) }.filter {
      s => s.isValid && !s.check
    }
    case White => {
      val wkm = wk.neighbourhood.map(State(wt, _, bk))

      def go(p: Option[Pos], d: Direction): Seq[Pos] = {
        p match {
          case Some(p) =>
            if (p == wt) return go(p.move(d), d)
            if (p == wk || p == bk) return Seq()
            return p +: go(p.move(d), d)
          case None =>
            return Seq()
        }
      }

      val wtm = go(Some(wt), Up) ++ go(Some(wt), Down) ++ go(Some(wt), Left) ++ go(Some(wt), Right)

      (wtm.map(State(_, wk, bk)) ++ wkm).filter { s =>
        s.isValid &&
        ((s.bk.move(Up) != Some(s.wt) && s.bk.move(Down) != Some(s.wt) &&
          s.bk.move(Left) != Some(s.wt) && s.bk.move(Right) != Some(s.wt)) ||
          s.wk.neighbourhood.contains(s.wt))
      }
    }
  }

  def show = {
    println("+--------+")
    for (y <- 8.to(1, -1)) {
      print("|")
      for (x <- 1 to 8) {
        val c = if  (x == wt.x && y == wt.y) "T"
          else if (x == wk.x && y == wk.y) "W"
          else if (x == bk.x && y == bk.y) "B"
          else "."
        print(c)
      }
      println("|")
    }
    println("+--------+")
  }
}

object Main {
  def choose[A](a: Seq[A], b: Seq[A]) = if (a.size < b.size) a else b

  def parseNum(s: String) = Pos(s.charAt(0) - 'a' + 1, s.charAt(1).asDigit)

  def parseInput(str: String) = {
    val Array(c, wk, wt, bk) = str.split("\\W+")

    ( if (c == "white") White else Black
    , State(parseNum(wt), parseNum(wk), parseNum(bk)))
  }

  def run(init: State, c: Color): Option[Seq[State]] = {
    val visited: Map[(Color, State), Seq[State]] = HashMap.empty
    val queue: Queue[(Color, Seq[State])] = Queue((c, Seq(init)))
    while (queue.nonEmpty) {
      val (c, states) = queue.head
      queue.dequeue()
      val moves = states.head.moves(c)
      moves.foreach { s =>
        val newStates = s +: states
        visited.get((c.next, s)) match {
          case None =>
            queue += ((c.next, newStates))
            visited += (((c.next, s), newStates))
          case Some(ss) =>
            if (ss.size > newStates.size)
              visited.update((c.next, s), newStates)
        }
      }    

      if (states.head.checkMate) {
        return Some(states)
      }
    }
    return None
  }

  def one(str: String) = {
    val (c, s) = parseInput(str)
    //s.moves(White).foreach(_.show)
    val states = run(s, c).get.reverse
    //states.foreach(_.show)
    println(states.size - 1)
  }

  def main(args: Array[String]): Unit = {
    val filename = "in_1.txt"
    for (line <- Source.fromFile(filename).getLines) {
      one(line)
    }
  }
}