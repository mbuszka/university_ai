import scala.collection.mutable.{ Map, HashMap, Queue }

trait Figure {
  val x: Int
  val y: Int
  def inRange(x: Int) = x >= 1 && x <= 8
  def possiblePositions: Seq[Figure]
  def pos = (x, y)
} 

case class King(x: Int, y: Int) extends Figure {
  def possiblePositions: Seq[King] = {
    for {
      a <- (x - 1) to (x + 1)
      b <- (y - 1) to (y + 1)
      if (inRange(a) && inRange(b))
    } yield King(a, b)
  }
}

case class Tower(x: Int, y: Int) extends Figure {
  def possiblePositions: Seq[Tower] = {
    (1 to 8).map { a => Tower(a, y) } ++
    (1 to 8).map { b => Tower(x, b) }
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

case class State(wt: Tower, wk: King, bk: King) {
  def isValid = {
    wt.pos != wk.pos &&
    wt.pos != bk.pos &&
    wk.pos != bk.pos &&
    bk.possiblePositions.forall { _ != wk }
  }

  def check = {
    wt.x == bk.x || wt.y == bk.y
  }

  def checkMate = moves(Black).forall(_.check)

  def moves(c: Color) = c match {
    case Black => bk.possiblePositions.map { State(wt, wk, _) }.filter(_.isValid)
    case White => 
      wk.possiblePositions.map { State(wt, _, bk) }.filter(_.isValid) ++
      wt.possiblePositions.map { State(_, wk, bk) }.filter(_.isValid)
  }
}

object Main {
  def choose[A](a: Seq[A], b: Seq[A]) = if (a.size < b.size) a else b

  def run(init: State, c: Color) = {
    val visited: Map[(Color, State), Seq[State]] = HashMap.empty
    val queue: Queue[(Color, Seq[State])] = Queue((c, Seq(init)))
    var best: Option[Seq[State]] = None
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
      best = best match {
        case None => Some(states)
        case Some(ss) => Some(choose(ss, states))
      }
    }
    best
  }

  def main(args: Array[String]): Unit = {
    println("starting")
    println(run(State(Tower(3, 8), King(3, 4), King(8, 3)), Black))
  }
}