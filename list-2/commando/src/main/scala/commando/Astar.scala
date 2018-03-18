package commando

import collection.mutable.PriorityQueue
import collection.mutable.HashMap
import collection.mutable.HashSet
import collection.mutable.Queue
import Game.State


case class E(k: Int, s: State)

object EOrdering extends Ordering[E] {
  def compare(a: E, b: E) = b.k compare a.k
}

case class Astar(game: Game) extends Search {
  def h1(state: State, game: Game) = {
    val dists = state.toSeq.map { c =>
      game.goals.foldLeft(Int.MaxValue) { (a, g) =>
        val d = c.dist(g)
        if (d < a) d else a
      }
    }
    dists.max
  }
  val h2Lookup = {
    val dists = HashMap[C, Int]()
    def go(state: State, dist: Int): Unit = {
      val states = Dir.all.flatMap(game.move(state, _)).toSet
      var added = false
      for (s <- states) {
        if (!dists.contains(s)) {
          dists.put(s, dist + 1)
          added = true
        }
      }
      if (added) go(states, dist + 1)
    }
    go(game.goals, 0)
    dists.toMap
  }
  
  def h2(state: State, game: Game): Int = {
    val dists = state.toSeq.map { c =>
      h2Lookup(c)
      // val known = HashMap[C, Int](c -> 0)
      // val waiting = Queue[C](c)
      // while (known.nonEmpty) {
      //   val c = waiting.dequeue
      //   val d = known(c)
      //   if (game.goals.contains(c)) return d
      //   for (dir <- Dir.all) {
      //     val c1 = game.move(c, dir)
      //     if (!known.contains(c1)) {
      //       known.put(c1, d + 1)
      //       waiting.enqueue(c1)
      //     }
      //   }
      // }
      // return Int.MaxValue
    }
    dists.max
  }

  def run: Seq[Dir] = {
    val known = HashMap[State, Vector[Dir]](game.drops -> Vector())
    val waiting = PriorityQueue[E](E(h2(game.drops, game), game.drops))(EOrdering)
    while (waiting.nonEmpty) {
      val E(cost, state) = waiting.dequeue
      val path = known(state)
      // println(cost, path)
      if (game.isDone(state)) {
        return path.reverse
      }
      Dir.all.foreach { d =>
        val newState = game.move(state, d)
        val newPath = d +: path
        if (!known.contains(newState)) {
          val e = E(newPath.length + h2(newState, game), newState)
          // print(e.k, d)
          waiting.enqueue(e)
          known.put(newState, newPath)
        }
      }
      // println
    }
    List()
  }  
}
