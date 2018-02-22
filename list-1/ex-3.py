from collections import defaultdict
from statistics import stdev, mean

import random

# card_order_dict = {"2":2, "3":3, "4":4, "5":5, "6":6, "7":7, "8":8, "9":9, "10":10,"J":11, "Q":12, "K":13, "A":14}

def check_straight_flush(hand):
  return check_flush(hand) and check_straight(hand)

def check_four_of_a_kind(hand):
  values = [i[0] for i in hand]
  value_counts = defaultdict(lambda:0)
  for v in values: 
    value_counts[v]+=1
  
  return sorted(value_counts.values()) == [1,4]

def check_full_house(hand):
  values = [i[0] for i in hand]
  value_counts = defaultdict(lambda:0)
  for v in values:
    value_counts[v]+=1
  
  return sorted(value_counts.values()) == [2,3]

def check_flush(hand):
  suits = { i[1] for i in hand }
  return len(suits) == 1

def check_straight(hand):
  values = [c[0] for c in hand]
  counts = defaultdict(lambda: 0)
  for v in values:
    counts[v] += 1
  value_range = max(values) - min(values)
  return counts.values() == { 1 } and value_range == 4

def check_three_of_a_kind(hand):
    values = [i[0] for i in hand]
    value_counts = defaultdict(lambda:0)
    for v in values:
        value_counts[v]+=1
    return set(value_counts.values()) == { 3, 1 }
        
def check_two_pairs(hand):
    values = [i[0] for i in hand]
    value_counts = defaultdict(lambda:0)
    for v in values:
        value_counts[v]+=1
    if sorted(value_counts.values())==[1,2,2]:
        return True
    else:
        return False

def check_one_pairs(hand):
    values = [i[0] for i in hand]
    value_counts = defaultdict(lambda:0)
    for v in values:
        value_counts[v]+=1
    if 2 in value_counts.values():
        return True
    else:
        return False

def rank(hand):
  if check_straight_flush(hand):
    return 9
  if check_four_of_a_kind(hand):
    return 8
  if check_full_house(hand):
    return 7
  if check_flush(hand):
    return 6
  if check_straight(hand):
    return 5
  if check_three_of_a_kind(hand):
    return 4
  if check_two_pairs(hand):
    return 3
  if check_one_pairs(hand):
    return 2
  return 1

suits = [ 'C', 'S', 'H', 'D' ]

low_cards = [(i, s) for i in range(2, 11) for s in suits ]
high_cards = [(i, s) for i in range(11, 15) for s in suits]

def test_one(low_hand, high_hand):
  return rank(low_hand) > rank(high_hand)

def run(n, low_cards, high_cards):
  wins = 0
  for i in range(n):
    random.shuffle(low_cards)
    random.shuffle(high_cards)
    if test_one(low_cards[0:5], high_cards[0:5]):
      wins += 1

  return wins / n

def test(low_cards, high_cards):
  n = 100
  k = 32
  res = [run(n, low_cards, high_cards) for i in range(n)]
  while stdev(res) > 0.05:
    n *= 2
    res = [run(n, low_cards, high_cards) for i in range(n)]
  return mean(res)

res = [(cards - 1, ss, test([(i, s) for i in range(2, cards) for s in suits[0:ss]], high_cards)) \
        for cards in range(7, 12) for ss in range(1, 5)]

res.sort(key=lambda t: t[2], reverse=True)

for (cards, suits, score) in res:
  print('Deck with cards from 2 to', cards, 'and with', suits, 'suits had a score of', score)