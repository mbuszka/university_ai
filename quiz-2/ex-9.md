Zmienne X oraz Y są łukowo spójne wzgl C jeśli dla każdego el. x z dom(X) istnieje y z dom(Y)
t. że C(x, y)

Sieć jest łukowo spójna, jeśli każda para zmiennych jest.

Jeśli domeny są duże, to O(cd^3) boli

ci są znane, dla każdej sumy można policzyć min i max.
Bo jak minimalizujemy i ci ujemne to bierzemy maksimum z dziedziny, a jak ci dodatnie to minimum.
W drugim przypadku analogicznie.

Potem ograniczamy y:
  - dla == zawężamy do przedziału min, max
  - dla != usuwamy min i max z dziedziny
  - dla <= zawężamy y <= max
  - dla >= zawężamy y >= min

Złożoność liniowa wzgl. ilości x
