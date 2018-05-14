1. Fajne gdy mamy dużo maksimów lokalnych
  - hill climbing do maks. lokalnego
  - krzyżówka i mutacja
2. Np. komandos
  - najpierw generuję trochę stanów początkowych (np losowe sekwencje ruchów)
  - potem odpalam na nich local beam search - na trochę kroków
  - wybieram najlepszy i odpalam A*

  Albo obcinamy kolejkę w A*
3. algorytm ewolucyjny, w którym z czasem spada tempo mutacji
4. zabraniamy powtarzania słabych genotypów (albo powtarzających się)
5. Na przemian
  - hill-climber
  - bfsem rozchodzimy się trochę na boki
  - odpalamy trochę nowych hill climbers
