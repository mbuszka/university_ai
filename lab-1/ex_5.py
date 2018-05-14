from random import choice, expovariate, randint
from math import floor
from itertools import starmap

class Board:
  state = []
  row_len = 0
  col_len = 0
  def __init__(self, rows, cols):
    self.state = [[randint(0, 1) for j in range(rows)] for i in range(cols)]
    self.row_len = cols
    self.col_len = rows

  def __str__(self):
    return '\n'.join([''.join(map(lambda x: '.' if x == 0 else '#', r)) for r in self.state])

  def get_col(self, i):
    return [ r[i] for r in self.state ]

  def get_row(self, i):
    return self.state[i]

  def flip(self, i, j):
    self.state[i][j] = 1 - self.state[i][j]

  def random(self):
    self.state = [[randint(0, 1) for j in range(self.col_len)] for i in range(self.row_len) ]

def opt_dist(line, d):
  n = len(line)
  m = n
  for i in range(0, n - d + 1):
    res = 0
    for j in range(i, i + d):
      res += 1 - line[j]
    for j in range(i):
      res += line[j]
    for j in range(i+d, n):
      res += line[j]
    m = min(m, res)
  return m

def disturb():
  return randint(0, 1000) > 900

class Image:
  board = None
  row_counts = []
  col_counts = []
  def __init__(self, row_counts, col_counts):
    self.board = Board(len(row_counts), len(col_counts))
    self.row_counts = row_counts
    self.col_counts = col_counts

  def __str__(self):
    return str(self.row_counts) + ', ' + str(self.col_counts) + '\n' + str(self.board)

  def row_scores(self):
    return [opt_dist(self.board.get_row(i), self.row_counts[i])
            for i in range(len(self.row_counts))]
  
  def col_scores(self):
    return [opt_dist(self.board.get_col(i), self.col_counts[i])
            for i in range(len(self.col_counts))]
  
  def score(self):
    return sum(self.col_scores()) + sum(self.row_scores())

  def local_score(self, i, j):
    return opt_dist(self.board.get_row(i), self.row_counts[i]) \
           + opt_dist(self.board.get_col(j), self.col_counts[j])

  def is_done(self):
    return set(self.row_scores()) == { 0 } and set(self.col_scores()) == { 0 }
  
  def step(self):
    row_scores = self.row_scores()
    choices = [ i for i in range(len(row_scores)) if row_scores[i] != 0]
    if len(choices) == 0:
      if disturb():
        k = randint(0, len(self.row_counts) - 1)
      else:
        return False
    else:
      k = choice(choices)
    scores = []
    # for i in range(len(self.row_counts)):
    for j in range(len(self.col_counts)):
      self.board.flip(k, j)
      score = self.score()
      self.board.flip(k, j)
      scores.append((k, j, score))
    
    scores.sort(key=lambda p: p[2])
    # print(scores)
    if disturb():
      idx = choice([s[0:2] for s in scores])
    else:
      idx = scores[0][0:2]
    self.board.flip(idx[0], idx[1])

    return True

def test(rows, cols):
  c = 0
  b = Image(rows, cols)
  delta = 0
  score = 0
  while not b.is_done():
    delta = score
    b.step()
    score = b.score()
    delta = abs(score - delta)
    # print(score)
    if delta < 2:
      c += 1
    if c > 20:
      # print('reroll!')
      b = Image(rows, cols)
      c = 0
  
  print(b)

cases = [  
  ([7,7,7,7,7,7,7], [7,7,7,7,7,7,7]), 
  ([2,2,7,7,2,2,2], [2,2,7,7,2,2,2]), 
  ([2,2,7,7,2,2,2], [4,4,2,2,2,5,5]),
  ([7,6,5,4,3,2,1], [1,2,3,4,5,6,7]),
  ([7,5,3,1,1,1,1], [1,2,3,7,3,2,1])
]

for (r, c) in cases:
  test(r, c)