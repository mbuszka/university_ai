from ex_04 import add_spaces
import sys

words = []
with open('../list-1/ex_2_words.txt') as f:
  words = { s.strip() for s in f.readlines() }

text = [ s.strip() for s in sys.stdin.readlines() ]

for line in text:
  # print('line', line)
  spaced = add_spaces(line)
  # print(spaced)
  while not all([ w in words for w in spaced]):
    spaced = add_spaces(line)
  print(" ".join(spaced))
