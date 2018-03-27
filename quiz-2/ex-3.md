## Spójna heurystyka jest zawsze optymistyczna

### Heurystyka jest spójna, jeśli
`h(n) <= c(n, n') + h(n')`

### Dowód (indukcja wzgl. dł. ścieżki)
Niech `s1 ... sn` będzie ścieżką
Wiemy, że dla dow. ścieżki o dł. n - 1 jest ok
```
C(s1, sn) = c(s1, s2) + ... + c(sn-1, sn)
h(s1) <= c(s1, s2) + h(s2) <= c(s1, s2) + C(s2, sn) = C(s1, sn)
```


### Przykład heurystyki optymistycznej, ale nie spójnej
1.
```
    a, h = 5
    /    \
   1      1
  /        \
b, h = 2    c, h = 8
```

2. Rozwiązujemy 8-puzzle, wybieramy zbiory { 1, 2, 3 } oraz { 4, 5, 6 }
   - obliczamy odległość taksówkową dla każdego zbioru
   - losowo wybieramy jedną z wartości dla każdego ze stanów
