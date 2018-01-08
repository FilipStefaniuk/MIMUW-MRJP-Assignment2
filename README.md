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

Dodatkowe:
    W katalogu bin znajduje się lekko zmodyfikowany bnfc którego używam do generowania struktury programu mającej przypisane pozycje
dla każdego z tokenów. Dzięki temu mogę wypisywać gdzie dokładnie wystąpił error.

Opis Rozwiązania:
    Udało mi się w pełni zrobić frontend, czyli działa sprawdzanie języka latte wraz ze wszystkimi rozszerzeniami. Ogólnie moim planem było
kompilowanie języka najpierw do reprezentacji pośredniej, którą miał być LLVM a potem po wykonaniu optymalizacji do assemblera. Ostatecznie,
z braku czasu musiałem ten plan zmodyfikować dopisałem więc "Emit" dla LLVM i póki co jest to kompilator do LLVM. Nie ma też żadnych optymalizacji.
Typy do LLVM są generowane przez BNFC jednak wypisywanie ich za pomocą automatycznie wygenerowanego "print" dawało słabe rezultaty dlatego zdecydowałem
się na ręczne dopisanie "emit". Wygenerowany LLVM jest utrzymany w stylu LLVMa wygenerowanego przez 'clang' czyli nazwy zmiennych są kolejnymi
liczbami a labele są anonimowe (nazwane kolejną liczbą po ostatniej zmiennej).
    Semantyka różni się lekko od tej w której napisane zostały testy w lattest, a dokładnie dwoma rzeczami:
        
        1) program:
            int main () {
                if (true) {
                    return 0;
                } else {}
            }
        Nie kompiluje się bo nie ma optymalizacji polegającej na obliczaniu wyrażeń w czasie kompilacji.

        2) Nie można dodawać do siebie napisów - póki co nie ma zaimplementowanych tablic a w LLVM przy implementacji gdzie napisy są definiowane jako
            const stałe globalne dodawanie ich nie za bardzo ma sens.

Nie wiem ile z tych rzeczy mogę (i ile zdążę) zrobić na drugi termin ale chciałbym dopisać wszystkie rozszerzenia (bo frontend jest już zrobione)
a także poprawić te dwie powyżej opisane rzeczy i dodać optymalizacje. 


