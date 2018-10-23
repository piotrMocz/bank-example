# Bank Example


Niniejsze repozytorium zawiera materiały uzupełniające wykład, który można znaleźć [tutaj](https://slides.com/piotrmoczurad/type-safe-programming). Pierwsza część wykładu zawiera ogólne informacje o programowaniu funkcyjnym, typach i Haskellu. Druga część przedstawia "ewolucję" programu Haskellowego: zaczynamy od bardzo prostej, naiwnej implementacji, która w zasadzie nie korzysta z dobrodziejstw Haskella. Dodajemy kolejne ulepszenia tak, by w końcu uzyskać program, który jest bezpieczny, łatwo rozwijalny i zwięzły.


### Uruchamianie projektu

Całość konfiguracji ogranicza się tak naprawdę do zainstalowania programu `stack`, służącego do zarządzania Haskellowymi projektami. Instrukcje instalacji znajdują się [tutaj](https://docs.haskellstack.org/en/stable/README/#how-to-install). Gdy `stack` będzie zainstalowany, należy (w głównym katalogu projektu) wykonać polecenie `stack setup`, które zadba o ściągnięcie odpowiedniej wersji kompilatora Haskella (`ghc`). Projekt najłatwiej budować instrukcją `stack install`. Następnie możemy uruchomić testy poleceniem `stack test`. Uwaga: `stack` instaluje programy do folderu `~/.local/bin` -- warto więc dodać go do ścieżki.

Konfiguracja projektu znajduje się w pliku `package.yaml` i tam też należy dodawać ewentualne zależności (plik `*.cabal` jest generowany i nie należy go edytować).


