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

