Przeszukujemy graf możliwych stanów gry, gdzie dostępne akcje to nasze ruchy, a przeciwnika
można deterministycznie symulować. Szukamy stanu wygrywającego (tzn. że mamy ścieżkę do zwycięstwa)
a. dfs / iterative deepening / hill-climbing / local beam search (coś co mocno przycina stany)
b. piszemy świetną heurystykę i odpalamy local beam search. (Bo chcemy znaleźć zwycięstwo,
niekoniecznie najszybsze)
