# Zoya
Rozwiązanie podzielone jest na dwie części:
- typechecker
    - `src/TypeCheker.hs`
    - sprawdza poprawność typów schodząc po drzewie składniowym
- interpreter
    - `src/Interpreter.hs`
    - wykonuje program nie sprawdzając poprawności typów.
    - wykonanie programu to obliczenie zmiennej `main`
    - wykonanie programu jest leniwe

Dodatkowo:
- `src/Common.hs`
    - funkcje potrzebne i w interpreterze i typecheckerze
    - usuwa syntax sugar zwiazany z listami z drzewa
        - list to poprostu typ zdefiniowany w Prelude `type List = Node Int List | Empty`
- `build_prelude.py`
    - prosty skrypt, który przekształca w czasie kompilacji `prelude` zdefiniowy w `src/prelude/prelude.zoya` w plik haskellowy z przeparsowanym `prelude`
    - `prelude` jest dołączany do początku każdego programu

