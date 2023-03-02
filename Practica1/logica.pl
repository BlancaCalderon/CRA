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
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
filas([1, 2, 3, 4, 5, 6, 7, 8, 9]).
columnas([1, 2, 3, 4, 5, 6, 7, 8, 9]).

buscar_posibilidades(L):-
    buscar_posibilidades_aux([],P, L, 1),
    reverse(P, RP),
    mostrar_sudoku(RP).

buscar_posibilidades_aux(F, F, _, 82).

buscar_posibilidades_aux(F, P, L, N):-
    nth1(N, L, X),
    not('.'==X),
    N1 is N + 1,
    buscar_posibilidades_aux([X | F], P, L, N1), !.

buscar_posibilidades_aux(F, P, L, N):-
    obtener_ejes(N, J1, J2),
    N1 is N + 1,
    posiblesNumeros(L, J1, J2, OP),
    buscar_posibilidades_aux([OP | F], P, L, N1), !.

obtener_ejes(N, J1, J2):-
    columnas(C), filas(F),
    member(J1, C), member(J2, F),
    N is (J1 - 1) * 9 + J2.
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
valoresPosibles([1, 2, 3, 4, 5, 6, 7, 8, 9]).

posiblesNumeros(Vector, Fila, Columna, Posibles) :-
        valoresPosibles(ValoresPosibles),
        obtenerFila(Vector, Fila, ValoresFila),!,
        obtenerColumna(Vector, Columna, ValoresColumna),
        obtenerBloque(Vector, Fila, Columna, ValoresBloque),
        union(ValoresFila, ValoresColumna, ValoresTemp),
        union(ValoresTemp, ValoresBloque, ValoresAux),
        subtract(ValoresPosibles, ValoresAux, Posibles).
%-------------------------------------------------------------------------------
obtenerFila(Sudoku, Fila, ValoresFila) :-
    obtenerFilaAux(Sudoku, Fila, 8, [], ValoresFila).

obtenerFilaAux(_, _, -1, P, P).

obtenerFilaAux(Sudoku, Fila, N, Lista, ValoresColumna):-
    Index is (Fila - 1) * 9 + N,
    nth0(Index, Sudoku, Valor),
    NuevosValores = [Valor | Lista],
    N1 is N - 1,
    obtenerFilaAux(Sudoku, Fila, N1, NuevosValores, ValoresColumna).
%-------------------------------------------------------------------------------
obtenerColumna(Sudoku, Columna, ValoresColumna) :-
    obtenerColumnaAux(Sudoku, Columna, 8, [], ValoresColumna).
    
obtenerColumnaAux(_, _, -1, P, P).

obtenerColumnaAux(Sudoku, Columna, N, Lista, ValoresColumna):-
    Index is (Columna - 1) + N * 9,
    nth0(Index, Sudoku, Valor),
    NuevosValores = [Valor | Lista],
    N1 is N - 1,
    obtenerColumnaAux(Sudoku, Columna, N1, NuevosValores, ValoresColumna).
%-------------------------------------------------------------------------------
obtenerBloque(Vector, Fila, Columna, ValoresBloque) :-
    InicioFila is (Fila - 1) // 3 * 3,
    InicioColumna is (Columna - 1) // 3 * 3,
    % Obtenemos los valores en el bloque
    findall(Valor, (
        between(0, 2, DFila),
        between(0, 2, DColumna),
        FilaBloque is InicioFila + DFila,
        ColumnaBloque is InicioColumna + DColumna,
        IndexBloque is FilaBloque * 9 + ColumnaBloque,
        nth0(IndexBloque, Vector, Valor)
    ), ValoresBloque).
%-------------------------------------------------------------------------------
