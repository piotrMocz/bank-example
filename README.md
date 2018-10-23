# Bank Example


Niniejsze repozytorium zawiera materiały uzupełniające wykład, który można znaleźć [tutaj](https://slides.com/piotrmoczurad/type-safe-programming). Pierwsza część wykładu zawiera ogólne informacje o programowaniu funkcyjnym, typach i Haskellu. Druga część przedstawia "ewolucję" programu Haskellowego: zaczynamy od bardzo prostej, naiwnej implementacji, która w zasadzie nie korzysta z dobrodziejstw Haskella. Dodajemy kolejne ulepszenia tak, by w końcu uzyskać program, który jest bezpieczny, łatwo rozwijalny i zwięzły.


## Uruchamianie projektu

Całość konfiguracji ogranicza się tak naprawdę do zainstalowania programu `stack`, służącego do zarządzania Haskellowymi projektami. Instrukcje instalacji znajdują się [tutaj](https://docs.haskellstack.org/en/stable/README/#how-to-install). Gdy `stack` będzie zainstalowany, należy (w głównym katalogu projektu) wykonać polecenie `stack setup`, które zadba o ściągnięcie odpowiedniej wersji kompilatora Haskella (`ghc`). Projekt najłatwiej budować instrukcją `stack install`. Następnie możemy uruchomić testy poleceniem `stack test`. Uwaga: `stack` instaluje programy do folderu `~/.local/bin` -- warto więc dodać go do ścieżki.

Konfiguracja projektu znajduje się w pliku `package.yaml` i tam też należy dodawać ewentualne zależności (plik `*.cabal` jest generowany i nie należy go edytować). Dodanie zależności polega na dołożeniu kolejnej pozycji w rubryce `dependencies`.


## Zadania

Przykładowe zadania, oparte na wykładzie.

### Zadanie 1.

**Punkty: 15**

Zadanie polega na ulepszeniu prostego systemu, pozwalającego na rejestrowanie transakcji w banku. System posiada tak naprawdę tylko jedną funkcję, którą wywołuje się, by odnotować przepływ pieniędzy między wysyłającym środki, a adresatem. Dodatkową funkcjonalnością jest obsługa różnych walut: bank wszystkie transakcje odnotowuje w dolarach, ale użytkownik może wybrać jedną z dostępnych walut (w przykładach jedyne waluty to `USD` i `PLN`, ale łatwo dodać kolejne).

Wyjściowy moduł to [`Bank`](https://github.com/piotrMocz/bank-example/blob/master/src/Bank.hs) -- zawiera szereg problemów i pułapek. Każdy kolejny moduł ([`Bank2`](https://github.com/piotrMocz/bank-example/blob/master/src/Bank2.hs) i kolejne) ma poprawiony jeden z tych problemów: są to oczywiście przykładowe rozwiązania i nie jedyne.

Punkty są przyznawane zależnie od ilości rozwiązanych problemów (problemów jest 5, więc każdy z nich to 20% punktów za zadanie). Można zrezygnować z części punktów, zaczynając od któregoś z późniejszych etapów.

### Zadanie 2.

**Punkty: 25 (+1)**

W obecnej formie system akceptuje adresy użytkowników, ale nie utrzymuje ich kont: w ten sposób nie jesteśmy w stanie sprawdzić, czy użytkownicy nie wysyłają więcej pieniędzy, niż posiadają. Zadanie polega na dopisaniu takiej funkcjonalności:
* Stan banku powinien zawierać informację o kontach użytkowników (dla uproszczenia przyjmiemy, że na konto składają się tylko jego numer oraz stan (ilość pieniędzy)).
* API powinno zostać wzbogacone o funkcję pozwalającą na zakładanie konta (powinna sprawdzić, czy takie konto już nie istnieje: wyborem implementującego jest, co zrobić w sytuacji, gdy już istnieje). Dodatkowo, funkcja powinna sama wygenerować unikalny numer konta: można sobie pomóc pakietem [uuid](http://hackage.haskell.org/package/uuid).
* Funkcja do przesyłania pieniędzy powinna sprawdzać, czy wysyłający posiada na koncie wystarczające środki (ponownie: obsługa błędów jest wyborem implementującego tak długo, jak nie jest to użycie funkcji `error` z modułu `Prelude`).
* Każda funkcjonalność powinna otrzymać swoje własne testy.

**Uwaga:** można za punkt wyjściowy przyjąć moduł [`Bank5`](https://github.com/piotrMocz/bank-example/blob/master/src/Bank5.hs).

**Uwagi o obsłudze błędów:**
* Niedozwolone jest używanie funkcji `error` oraz zwracanie `undefined` z funkcji.
* Można używać typu `Maybe`, choć preferowany jest `Either` (niesie więcej informacji).
* Dodatkowy punkt będzie przyznany za użycie Haskellowych wyjątków (najlepiej tych z pakietu [`safe-exceptions`](https://hackage.haskell.org/package/safe-exceptions)).

### Zadanie 3.

**Punkty: 35**

W obecnej formie system jest "zamknięty na świat" (w zasadzie jest bardziej biblioteką, niż systemem _per se_). Nie musi tak być, gdyż pisanie aplikacji webowych jest w Haskellu całkiem przyjemne. Można użyć dowolnego frameworku, ale najbardziej przyszłościową inwestycją wydaje się być zapoznanie się z [Yesodem](https://www.yesodweb.com/). Zadanie polega na napisaniu serwera WWW, który będzie obsługiwał trzy rodzaje zapytań:
1. [POST] utworzenie konta.
2. [POST] przelew z jednego konta na drugie.
3. [GET] pobranie stanu konta dla danego numeru konta (nie martwimy się autentykacją).

**Uwaga 1:** nie potrzeba nam żadnego interfejsu użytkownika (choć oczywiście można go napisać). Wystarczy nam, żeby serwer odpowiadał na pisane "z palca" zapytania HTTP.

**Uwaga 2:** zależy nam na używaniu "pure" funkcji. Jeśli program ma wykonać jakąś operację w `IO` (lub inną "impure"), najlepiej, jeśli zrobi to jak najwyżej: nie chcemy chować potencjalnie niebezpiecznych operacji pod warstwami abstrakcji. 
