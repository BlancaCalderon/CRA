%Mostrar el sudoku mas bonito :)
mostrar_sudoku([]):-
    nl, write('-------------------'), nl.

mostrar_sudoku([X|L]):-
    length(L, N),
    0 is mod((N+1), 9),
    nl, write('-------------------'), nl,
    write('|'), write(X), write('|'),
    mostrar_sudoku(L),!.
    
mostrar_sudoku([X|L]):-
    write(X), write('|'),
    mostrar_sudoku(L).