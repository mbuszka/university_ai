Niech h będzie optymistyczną heurystyką, a T drzewem.
Wtedy jeżeli jakiś wierzchołek jest najbliższym celem i ma koszt k
to algorytm badając wszystkie wierzchołki o `f <= k` na pewno zbada każdy wierzchołek
na ścieżce między początkiem a celem, ponieważ dla każdego `v` na ścieżce
`f(v) = d(s, v) + h(s) <= d(s, v) + d(v, t) = k`
