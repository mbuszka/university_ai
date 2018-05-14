package commando

import util.Random
import collection.immutable.HashSet


abstract sealed trait Dir
case object U extends Dir
case object D extends Dir
case object L extends Dir
case object R extends Dir

object Dir {
  val all = List(U, D, L, R)
}

case class C(x: Int, y: Int) {
  def move(d: Dir) = d match {
    case U => C(x, y - 1)
    case D => C(x, y + 1)
    case L => C(x - 1, y)
    case R => C(x + 1, y)
  }

  def dist(c: C) = {
    math.abs(x - c.x) + math.abs(y - c.y)
  }
}

object Game {
  type State = Set[C]
}

case class Game(walls: HashSet[C], goals: HashSet[C], drops: Set[C]) {
  import Game.State
  def move(c: C, d: Dir): C = {
    val c1 = c.move(d)
    if (walls.contains(c1)) c else c1
  }

  def move(s: State, d: Dir): State = {
    s.map(move(_, d))
  }

  def isDone(s: State) = {
    s.subsetOf(goals)
  }

  def chooseBest(states: Seq[(State, List[Dir])]) = {
    states.foldLeft(states.head) { (a, p) =>
      if (a._1.size > p._1.size)
        p
      else
        a
    }
  }

  def chooseBestOrRandom(states: Seq[(State, List[Dir])]) = {
    if (Random.nextInt(20) < 1) {
      states(Random.nextInt(states.size))
    } else {
      chooseBest(states)
    }
  }
}

trait Search {
  def run: Seq[Dir]
}
