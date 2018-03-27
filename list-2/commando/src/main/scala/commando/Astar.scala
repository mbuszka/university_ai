package commando

import collection.mutable.PriorityQueue
import collection.mutable.HashMap
import collection.mutable.HashSet
import collection.mutable.Queue
import Game.State


case class E(k: Int, d: Int, s: State)

object EOrdering extends Ordering[E] {
  def compare(a: E, b: E) = {
    val c = b.k compare a.k
    if (c == 0)
      b.d compare a.d
    else
      c
  }
}

case class Astar(game: Game) extends Search {
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

  val h3Lookup = h2Lookup.mapValues(Math.pow(_, 1.5).toInt)

  def h3(state: State, game: Game): Int = {
    state.toSeq.map(h3Lookup(_)).max * state.size
  }

  def h2(state: State, game: Game): Int = {
    val dists = state.toSeq.map { c =>
      h2Lookup(c)
    }

    val param = 0.08 // exercise 5
    // val param =  1 // exercise 6

    dists.max + (param * state.size).toInt
  }

  def run: Seq[Dir] = {
    val known = HashMap[State, Vector[Dir]](game.drops -> Vector())
    val waiting = PriorityQueue[E](E(h3(game.drops, game), 0, game.drops))(EOrdering)
    while (waiting.nonEmpty) {
      val E(cost, dist, state) = waiting.dequeue
      val path = known(state)
      if (game.isDone(state)) {
        Console.err.println(path.size)
        return path.reverse
      }
      Dir.all.foreach { d =>
        val newState = game.move(state, d)
        val newPath = d +: path
        if (!known.contains(newState)) {
          val e = E(dist + 1 + h3(newState, game), dist + 1, newState)
          waiting.enqueue(e)
          known.put(newState, newPath)
        }
      }
    }
    List()
  }
}
