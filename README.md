# MRJP-Assignment-2

Kompilator Latte.

Program kompiluje język latte do LLVM.

uruchamianie programu:
(uruchomienie z opcją -h wypisuje tą wiadomość).
    latc_llvm [-h] [file...]

dla każdego pliku (foo/bar/baz.lat) z listy zostaną wygenerowane w foo/bar/ baz.ll oraz baz.bc

W przypadku nie podania żadnego pliku program wczytuje standardowe wejście i wypisyje kod llvm na standardowe wyjście.

Biblioteki:
    Nie używam niestandardowych bibliotek, w katalogu libs znajdują się tylko pliki runtime z implementacją wbudowanych funkcji.

Opis Rozwiązania:

W kompilatorze zaimplementowane są wszystkie podstawowe wymagania, dodatkowo zrobione są tablice i klasy (ale bez metod wirtualnych).
Nie ma dodatkowych optymalizacji oprócz obliczania stałych wyrażeń i usuwania kodu do którego dotarcie jest niemożliwe. 
Program przechodzi wszystkie publiczne testy. Poprzednia wersja używała zmodyfikowanego bnfc, ale postanowiłem z niej zrezygnować ponieważ
pojawiały się tam problemy przy wczytywaniu napisów.

Znalazłem kilka błędów których niestety nie mam już czasu poprawić są to:

1) Nie działaja dobrze znaki escape w napisach, niestety cały czas nie udało mi sie dobrze tego zrobić.

2) Po zaimplementowaniu typów i rzutowania pojawił się konflikt w gramatyce którego nie potrafię w prosty sposób rozwiązać:

        LVal -> Ident .                                     (rule 39)
        Expr5 -> '(' Ident . ')' Expr6                      (rule 63)
        Expr7 -> Ident . '(' ListExpr ')'                   (rule 76)

        '!='           reduce using rule 39
        '%'            reduce using rule 39
        '&&'           reduce using rule 39
        '('            shift, and enter state 112
        ')'            shift, and enter state 129
                        (reduce using rule 39)

    powoduje to że na przykład takie wyrażenie jest niepoprawne: (x) + 1

3) Po zmianie bnfc i usunięciu informacji o linii z wiadomości o błędach są one teraz lekko nieczytelne. 