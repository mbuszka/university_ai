import sys

words = []
with open('ex_2_words.txt') as f:
  words = { s.strip() for s in f.readlines() }

text = sys.stdin.read()

m = max([len(x) for x in words])
n = len(text)

score = [-1 for i in range(n)]
spaces = [0 for i in range(n)]
score[0] = 0

for i in range(n):
  for j in range(max(0, i - m - 1), i + 1):
    if score[j] != -1:
      if text[j:i] in words:
        if score[i] < score[j] + (i - j) ** 2:
          score[i] = score[j] + (i - j) ** 2
          spaces[i] = j
      elif i == j + 1 and text[j:i] == '\n':
        score[i] = score[j]
        spaces[i] = spaces[j]

cuts = []
i = n - 1
while spaces[i] != 0:
  i = spaces[i]
  cuts.append(i)
cuts.reverse()

def apply_cuts(text, cuts):
  res = []
  cuts = [ 0 ] + cuts
  for i in range(len(cuts) - 1):
    res.append(text[cuts[i]:cuts[i+1]])
  return " ".join(res)

res = apply_cuts(text, cuts)
# print(score[0:15])
# print(score[n-1])
print(res)
