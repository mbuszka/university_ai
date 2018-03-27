package commando

import collection.mutable.PriorityQueue
import collection.mutable.HashMap
import collection.mutable.Queue


object StateOrdering extends Ordering[Set[C]] {
  def compare(a: Set[C], b: Set[C]) = a.size compare b.size
}

case class Bfs(game: Game) extends Search {
  import Game.State
  def reduceStateSingle(init: State): (State, List[Dir]) = {
    def go(cnt: Int, state: State, path: List[Dir]): (State, List[Dir]) = {
      if (cnt > 100) return (state, path)
      if (state.size <= 2) return (state, path)
      val states = Dir.all.map { d => (game.move(state, d), d +: path) }
      val (nextS, nextP) = game.chooseBestOrRandom(states)
      return go(cnt + 1, nextS, nextP)
    }
    
    go(0, init, List())
  }

  def reduceStateSpace(init: State) = {
    game.chooseBest(Vector.fill(1000)(reduceStateSingle(init)))
  }

  def run(init: State): List[Dir] = {
    val known = HashMap[State, List[Dir]](init -> List())
    val waiting = Queue[State](init)
    while (waiting.nonEmpty) {
      val state = waiting.dequeue
      val path = known(state)
      if (game.isDone(state)) {
        return path
      }
      Dir.all.foreach { d =>
        val newState = game.move(state, d)
        val newPath = d +: path
        if (!known.contains(newState)) {
          waiting.enqueue(newState)
          known.put(newState, newPath)
        }
      }
    }
    return List()
  }

  def run: List[Dir] = {
    val (reduced, path1) = reduceStateSpace(game.drops)
    val path2 = run(reduced)
    path1.reverse ++ path2.reverse
  }
}
