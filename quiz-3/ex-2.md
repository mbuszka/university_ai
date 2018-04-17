# BFS and DFS

  - `K` - observations (finite set)
  - `Q` - agent state
  - `A` - available actions
  - `δ : QxKxA -> QxK` - progression function

```
  dfs(q):
    states = stack((q, Φ))
    known = set((q, Φ))
    while states non empty:
      s = states.pop
      if finished(s):
        return s, known
      for a in actions:
        s' = δ(s, a)
        if not known s':
          known.insert(s' -> (a, s))
          stack.push(s')
```
