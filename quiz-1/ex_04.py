from random import randint, sample

def add_spaces(str):
  n = len(str)
  k = randint(0, n - 1)
  spaces = sample(range(1, n), k)
  spaces.extend([0, n])
  spaces.sort()
  # print(spaces)
  text = []
  for j in range(1, len(spaces)):
    text.append(str[spaces[j - 1]:spaces[j]])
  return text

def test():
  s = 'alamakota'
  words = []
  with open('../list-1/ex_2_words.txt') as f:
    words = { s.strip() for s in f.readlines() }

  spaced = add_spaces(s)
  while not all([ w in words for w in spaced]):
    spaced = add_spaces(s)
  print(" ".join(spaced))

# test()
