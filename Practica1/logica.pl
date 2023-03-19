%Mostrar el sudoku mas bonito :)
mostrar_sudoku([]):-
    nl, write('-------------------'), nl.

mostrar_sudoku([X|L]):-                 %X es primer elemento del sudoku actual
    length(L, N),                       %Guarda en N la longitud del sudoku
    0 is mod((N+1), 9),                 %Determina cuando se pasa a la siguiente sila
    nl, write('-------------------'), nl,
    write('|'), write(X), write('|'),
    mostrar_sudoku(L),!.                 %lLamada recursiva para mostrar el siguiente elemento del sudoku

mostrar_sudoku([X|L]):-
    write(X), write('|'),
    mostrar_sudoku(L).
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
filas([1, 2, 3, 4, 5, 6, 7, 8, 9]).
columnas([1, 2, 3, 4, 5, 6, 7, 8, 9]).

%Busca los distintos números que pueden ir en una pisción maracada por .
buscar_posibilidades(L, RP):-
    buscar_posibilidades_aux([],P, L, 1),       %Llama a función auxiliar empezando en la posicion 1 del sudoku, L el sudoku , empezando lista vacia
    reverse(P, RP).
    %mostrar_sudoku(RP).

buscar_posibilidades_aux(F, F, _, 82).          %Cuando se llega al final del sudoku (pos 82)

buscar_posibilidades_aux(F, P, L, N):-
    nth1(N, L, X),                                      %obtiene de L el elemento N y lo mete en X L --> sudoku N --> numero de la posicion del que queremos pillar elemento
    not('.'==X),                                        %Si elemento actual no es un .
    N1 is N + 1,                                        %inicializa variable N con valor N + 1(is cuando es un constante compara y cuando e suna variable es comparacion)
    buscar_posibilidades_aux([X | F], P, L, N1), !.     %añade elemento X F--> lista que se va a llenar en la recursividad, P --> retorno(posibilidades) N1 -->siguiente posicion

buscar_posibilidades_aux(F, P, L, N):-
    obtener_ejes(N, J1, J2),                               %Obtiene los ejes correspondientes en los que se encuentra nuestro elemento (J1 --> eje x, j2 --> Eke y)
    N1 is N + 1,                                           %N1 es siguiente elemento
    posiblesNumeros(L, J1, J2, OP),                        %busca posibilidades, OP --> retorno (numeros) L --> sudoku
    buscar_posibilidades_aux([OP | F], P, L, N1), !.       %llamada recursiva pasando al siguiente elemento

%Funcion que obtiene los ejes x,y de un elemento N del sudoku
obtener_ejes(N, J1, J2):-
    columnas(C), filas(F),           %Guarda en C la lista de elementos de columna y en F la de filas
    member(J1, C), member(J2, F),    %Revisa si J1 es miembro de las columnas y J2 de las filas
    N is (J1 - 1) * 9 + J2.
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
valoresPosibles([1, 2, 3, 4, 5, 6, 7, 8, 9]).   %Lista que guarda todos los posibles valores que puede haber en una posicion

%Busca posibilidades para una celda, sacando los valores por fila,columna y cuadrante y quitandoselo a los valores posibles iniciales
posiblesNumeros(Vector, Fila, Columna, Posibles) :-
        valoresPosibles(ValoresPosibles),                      %Guarda todos los valores posibles
        obtenerFila(Vector, Fila, ValoresFila),!,              %Obtiene los valores posibles de esa fila
        obtenerColumna(Vector, Columna, ValoresColumna),       %Obtiene los valores posibles de esa columna
        obtenerBloque(Vector, Fila, Columna, ValoresBloque),   %Obtiene los valores posibles de ese bloque
        union(ValoresFila, ValoresColumna, ValoresTemp),
        union(ValoresTemp, ValoresBloque, ValoresAux),        %Une todos los valores encontrados anteriormenete en valoresAux
        subtract(ValoresPosibles, ValoresAux, Posibles).      %quita posibles valores a los valores posibles iniciales
%-------------------------------------------------------------------------------
%Ibtiene los valores que puede tener uan fila
obtenerFila(Sudoku, Fila, ValoresFila) :-
    obtenerFilaAux(Sudoku, Fila, 8, [], ValoresFila).       %Recorre primera fila (posición 8)

obtenerFilaAux(_, _, -1, P, P).        %Para terminar la ejecución cuando posición es -1

obtenerFilaAux(Sudoku, Fila, N, Lista, ValoresFila):-   %Funcion auxiliar para encontrar los valores de una fila
    Index is (Fila - 1) * 9 + N,                        %Calcula la posición del index correspondiente al elemento
    nth0(Index, Sudoku, Valor),                         %Obtiene el valor de index de sudoku y lo introduce en valor
    NuevosValores = [Valor | Lista],                    %Introduce el nuevo valor en la lista
    N1 is N - 1,                                        %Recorre posición anterior
    obtenerFilaAux(Sudoku, Fila, N1, NuevosValores, ValoresFila).       %Llamada recursiva comprobando siguiente valor
%-------------------------------------------------------------------------------
obtenerColumna(Sudoku, Columna, ValoresColumna) :-                  %Obtiene los valores posibles de una columna
    obtenerColumnaAux(Sudoku, Columna, 8, [], ValoresColumna).      %Primera llamada a la función auxiliar empezando en pos 8

obtenerColumnaAux(_, _, -1, P, P).        %Para terminar la ejecución de la función cuando posición es -1

obtenerColumnaAux(Sudoku, Columna, N, Lista, ValoresColumna):-    %Funcion auxiliar que saca los posibles valores de una columna
    Index is (Columna - 1) + N * 9,                               %Calcula el index correspondiente
    nth0(Index, Sudoku, Valor),                                   %Saca el valor correspondiente al idnex del sudoku y lo introduce en valor
    NuevosValores = [Valor | Lista],                                                  %Introduce el valor en la lista de nuevso valores
    N1 is N - 1,                                                                      %Para recorrer siguiente elemento
    obtenerColumnaAux(Sudoku, Columna, N1, NuevosValores, ValoresColumna).            %Llamada recursiva para sacar los valores de la siguiente posicion
%-------------------------------------------------------------------------------
obtenerBloque(Vector, Fila, Columna, ValoresBloque) :-        %Funcion que obtiene los valores podibles del bloque en el que nos encontramos
    InicioFila is (Fila - 1) // 3 * 3,                        %Calcula posición en la que comienza la fila
    InicioColumna is (Columna - 1) // 3 * 3,                  %Calcula la posición en el que comienza la columna
    % Obtenemos los valores en el bloque
    findall(Valor, (                                          %Encuentra todos los valores que se encuentran en la fila y columna correspondiente al bloque, al igual que los valores de ese bloque
        between(0, 2, DFila),
        between(0, 2, DColumna),
        FilaBloque is InicioFila + DFila,                     %Calcula la fila del bloque correspondiente
        ColumnaBloque is InicioColumna + DColumna,            %Calcula la columna del bloque correspondiente
        IndexBloque is FilaBloque * 9 + ColumnaBloque,        %Calcula el index
        nth0(IndexBloque, Vector, Valor)                      %Saca el elemento del vector correspondiente al index y lo guarda en valor
    ), ValoresBloque).
%-------------------------------------------------------------------------------
%Sustituir un elemento
sustituir_elemento(Vector, Indice, NuevoElemento, NuevoVector) :-
    nth1(Indice, Vector, _, Resto),
    nth1(Indice, NuevoVector, NuevoElemento, Resto).

actualizar_sudoku(Sudoku, N, Resultado):-
     obtener_ejes(N, J1, J2),
     nth1(N, Sudoku, X),
     recorrerFila(Sudoku, X, 9, J1, ListaAux),
     recorrerColumna(ListaAux, X, 9, J2, ListaAux2),
     recorrerCuadrado(ListaAux2, X, J1, J2, 3, 3, ListaAux3),
     Resultado = ListaAux3.
%-------------------------------------------------------------------------------
recorrerFila(Final, _, 0, _, Final).

recorrerFila(Sudoku, X, N, Fila, ListaFinal):-
     Index is (Fila - 1) * 9 + N,
     nth1(Index, Sudoku, Y),
     member(X, Y),
     select(X, Y, Borrada),
     sustituir_elemento(Sudoku, Index, Borrada, SudokuAux),
     N1 is N - 1,
     recorrerFila(SudokuAux, X, N1, Fila, ListaFinal),!.

recorrerFila(Sudoku, X, N, Fila, ListaFinal):-
     N1 is N - 1,
     recorrerFila(Sudoku, X, N1, Fila, ListaFinal),!.
%-------------------------------------------------------------------------------
recorrerColumna(Final, _, -1, _, Final).

recorrerColumna(Sudoku, X, N, Columna, ListaFinal):-
     Index is (Columna) + N * 9,
     nth1(Index, Sudoku, Y),
     member(X, Y),
     select(X, Y, Borrada),
     sustituir_elemento(Sudoku, Index, Borrada, SudokuAux),
     N1 is N - 1,
     recorrerColumna(SudokuAux, X, N1, Columna, ListaFinal),!.

recorrerColumna(Sudoku, X, N, Columna, ListaFinal):-
     N1 is N - 1,
     recorrerColumna(Sudoku, X, N1, Columna, ListaFinal),!.
%-------------------------------------------------------------------------------
recorrerCuadrado(Final, _, _, _, _, 0, Final).

recorrerCuadrado(Sudoku, X, Fila, Columna, ContF, ContC, ListaFinal):-
    ContF is 0,
    N1 is ContC - 1,
    recorrerCuadrado(Sudoku, X, Fila, Columna, 3, N1, ListaFinal),!.

recorrerCuadrado(Sudoku, X, Fila, Columna, ContF, ContC, ListaFinal):-
     InicioFila is ((Fila - 1) // 3 * 3) + ContF - 1,
     InicioColumna is ((Columna - 1) // 3 * 3) + ContC,
     Posicion is InicioFila * 9 + InicioColumna,
     valoresPosibles(Num),
     nth1(Posicion, Sudoku, Y),
     not(member(Y, Num)),
     select(X, Y, Borrada),
     sustituir_elemento(Sudoku, Posicion, Borrada, SudokuAux),
     N1 is ContF - 1,
     recorrerCuadrado(SudokuAux, X, Fila, Columna, N1, ContC, ListaFinal),!.

recorrerCuadrado(Sudoku, X, Fila, Columna, ContF, ContC, ListaFinal):-
    N1 is ContF - 1,
    recorrerCuadrado(Sudoku, X, Fila, Columna, N1, ContC, ListaFinal),!.

%-------------------------------------------------------------------------------
%Reglas de simplificacion
%Regla 0 --> Si hay un lugar donde solo cabe un numero, lo escribimos en el lugar correspondiente y lo eliminamos de los lugares en los que aparezca de los que son conflictivos
regla0(Sudoku, Resultado):-
    regla0Aux(Sudoku, 81, Resultado).

regla0Aux(Resultado, 0, Resultado).

regla0Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),
    N1 is N - 1,
    regla0Aux(Sudoku, N1, Resultado),!.

regla0Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Tam),
    Tam > 1,
    N1 is N - 1,
    regla0Aux(Sudoku, N1, Resultado),!.

regla0Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    N1 is N - 1,
    nth1(1, X, X1),
    sustituir_elemento(Sudoku, N, X1, SudokuAux),
    actualizar_sudoku(SudokuAux, N, Prueba),
    regla0Aux(Prueba, N1, Resultado),!.

%-------------------------------------------------------------------------------
regla1(Sudoku, Resultado):-
    regla1Aux(Sudoku, 81, Resultado).

regla1Aux(Resultado, 0, Resultado).

regla1Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),
    N1 is N - 1,
    regla1Aux(Sudoku, N1, Resultado),!.

regla1Aux(Sudoku, N, Resultado):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    length(X, Tam),
    regla1Fila(Sudoku, N, X, Tam, 9, J1, ListaAux),
    regla1Columna(ListaAux, N, X, Tam, 9, J2, ListaAux2),
    regla1Cuadrante(ListaAux2, N, X, Tam, 3, 3, J1, J2, ListaAux3),
    N1 is N - 1,
    regla1Aux(ListaAux3, N1, Resultado),!.
%-------------------------------------------------------------------------------
regla1Cuadrante(Final, _, _, 0, _, _, _, _, Final).

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux):-
    M is 0,
    N is 0,
    nth1(Tam, X, Y),
    sustituir_elemento(Sudoku, Pos, Y, SudokuAux),
    actualizar_sudoku(SudokuAux, Pos, Resultado),
    regla1Cuadrante(Resultado, Pos, X, 0, N, M, Fila, Columna, ListaAux),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    Pos is Posicion,
    N > 0,
    N1 is N - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, N1, M, Fila, Columna, ListaAux),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux):-
    N is 0,
    M1 is M - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, 3, M1, Fila, Columna, ListaAux),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Tam, X, Y),
    nth1(Posicion, Sudoku, Sig),
    member(Y, Sig),
    Tam1 is Tam - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam1, 3, 3, Fila, Columna, ListaAux),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux):-
    N1 is N - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, N1, M, Fila, Columna, ListaAux),!.
%-------------------------------------------------------------------------------
regla1Columna(Final, _, _, 0, _, _, Final).

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    Pos is Index,
    N1 is N - 1,
    regla1Columna(Sudoku, Pos, X, Tam, N1, Columna, ListaAux),!.

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux):-
    N is 0,
    nth1(Tam, X, Y),
    sustituir_elemento(Sudoku, Pos, Y, SudokuAux),
    actualizar_sudoku(SudokuAux, Pos, Resultado),
    regla1Columna(Resultado, Pos, X, 0, 9, Columna, ListaAux),!.

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Sig),
    nth1(Tam, X, Y),
    member(Y, Sig),
    Tam1 is Tam - 1,
    regla1Columna(Sudoku, Pos, X, Tam1, 9, Columna, ListaAux),!.

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux):-
    N1 is N - 1,
    regla1Columna(Sudoku, Pos, X, Tam, N1, Columna, ListaAux),!.
%-------------------------------------------------------------------------------
regla1Fila(Final, _, _, 0, _, _, Final).

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    Pos is Index,
    N1 is N - 1,
    regla1Fila(Sudoku, Pos, X, Tam, N1, Fila, ListaAux),!.

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux):-
    N is 0,
    nth1(Tam, X, Y),
    sustituir_elemento(Sudoku, Pos, Y, SudokuAux),
    actualizar_sudoku(SudokuAux, Pos, Resultado),
    regla1Fila(Resultado, Pos, X, 0, 9, Fila, ListaAux),!.

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Sig),
    nth1(Tam, X, Y),
    member(Y, Sig),
    Tam1 is Tam - 1,
    regla1Fila(Sudoku, Pos, X, Tam1, 9, Fila, ListaAux),!.

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux):-
    N1 is N - 1,
    regla1Fila(Sudoku, Pos, X, Tam, N1, Fila, ListaAux),!.
%-------------------------------------------------------------------------------
regla2(Sudoku, Resultado):-
    regla2Aux(Sudoku, 81, Resultado).

regla2Aux(Final, 0, Final).

regla2Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    number(X),
    N1 is N - 1,
    regla2Aux(Sudoku, N1, Resultado),!.

regla2Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Longitud),
    not(Longitud is 2),
    N1 is N - 1,
    regla2Aux(Sudoku, N1, Resultado),!.

regla2Aux(Sudoku, N, Resultado):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    regla2Fila(Sudoku, N, X, J1, 9, ListaAux),
    regla2Columna(ListaAux, N, X, J2, 9, ListaAux2),
    regla2Cuadrante(ListaAux2, N, X, 3, 3, J1, J2, ListaAux3),
    N1 is N - 1,
    regla2Aux(ListaAux3, N1, Resultado),!.
%-------------------------------------------------------------------------------
regla2Cuadrante(Final, _, _, _, 0, _, _, Final).

regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    N is 0,
    M1 is M - 1,
    regla2Cuadrante(Sudoku, Pos, X, 3, M1, Fila, Columna, ListaAux),!.
    
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    (Pos is Posicion;
    number(Elem);
    (not(
    number(Elem)),
    length(Elem, Longitud),
    Longitud > 2)),
    N >= 1,
    N1 is N - 1,
    regla2Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, ListaAux),!.

regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    borrarParejaCuadrante(Sudoku, Pos, Posicion, Fila, Columna, 3, 3, ListaSin),
    N1 is N - 1,
    regla2Cuadrante(ListaSin, Pos, X, N1, M, Fila, Columna, ListaAux),!.
    
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    N1 is N - 1,
    regla2Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, ListaAux),!.
%-------------------------------------------------------------------------------
regla2Columna(Final, _, _, _, 0, Final).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 2))),
    N1 is N - 1,
    regla2Columna(Sudoku, Pos, [X | Y], Columna, N1, ListaAux).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    member(X, Elem),
    nth1(1, Y, Y1),
    member(Y1, Elem),
    borrarParejaColumna(Sudoku, Pos, Index, Columna, 9, ListaSin),
    N1 is N - 1,
    regla2Columna(ListaSin, Pos, [X | Y], Columna, N1, ListaAux).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux):-
    N1 is N - 1,
    regla2Columna(Sudoku, Pos, [X | Y], Columna, N1, ListaAux).
%-------------------------------------------------------------------------------
regla2Fila(Final, _, _, _, 0, Final).

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 2))),
    N1 is N - 1,
    regla2Fila(Sudoku, Pos, [X | Y], Fila, N1, ListaAux),!.

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    member(X, Elem),
    nth1(1, Y, Y1),
    member(Y1, Elem),
    borrarParejaFila(Sudoku, Pos, Index, Fila, 9, ListaSin),
    N1 is N - 1,
    regla2Fila(ListaSin, Pos, [X | Y], Fila, N1, ListaAux),!. %X --> [X | Y]

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux):-
    N1 is N - 1,
    regla2Fila(Sudoku, Pos, [X | Y], Fila, N1, ListaAux),!.
%-------------------------------------------------------------------------------
borrarParejaCuadrante(Final, _, _, _, _, _, 0, Final).

borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux):-
    N is 0,
    M1 is M - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, 3, M1, SudokuAux),!.

borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    (Pos1 is Posicion;
    Pos2 is Posicion;
    number(Elem)),
    N1 is N - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux),!.
    
borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    borrarTrioGeneral(Sudoku, Posicion, Pos1, 2, SudokuSin),
    N1 is N - 1,
    borrarParejaCuadrante(SudokuSin, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux),!.
    
borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux):-
    N1 is N - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux),!.
%-------------------------------------------------------------------------------
borrarParejaColumna(Final, _, _, _, 0, Final).

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos1 is Index;
    Pos2 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N1, SudokuAux),!.

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    borrarTrioGeneral(Sudoku, Index, Pos1, 2, SudokuSin),
    N1 is N - 1,
    borrarParejaColumna(SudokuSin, Pos1, Pos2, Columna, N1, SudokuAux),!.

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux):-
    N1 is N - 1,
    borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
borrarParejaFila(Final, _, _, _, 0, Final).

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos1 is Index;
    Pos2 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N1, SudokuAux),!.

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    borrarTrioGeneral(Sudoku, Index, Pos1, 2, SudokuSin),
    N1 is N - 1,
    borrarParejaFila(SudokuSin, Pos1, Pos2, Fila, N1, SudokuAux),!.

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux):-
    N1 is N - 1,
    borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
regla3(Sudoku, Resultado):-
    regla3Aux(Sudoku, 81, Resultado).

regla3Aux(Final, 0, Final).

regla3Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    number(X),
    N1 is N - 1,
    regla3Aux(Sudoku, N1, Resultado),!.

regla3Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Longitud),
    not(Longitud is 3),
    N1 is N - 1,
    regla3Aux(Sudoku, N1, Resultado),!.

regla3Aux(Sudoku, N, Resultado):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    regla3Fila(Sudoku, N, X, J1, 9, [], ListaAux),
    regla3Columna(ListaAux, N, X, J2, 9, [], ListaAux2),
    regla3Cuadrante(ListaAux2, N, X, 3, 3, J1, J2, [], ListaAux3),
    N1 is N - 1,
    regla3Aux(ListaAux3, N1, Resultado),!.
%-------------------------------------------------------------------------------
regla3Fila(Final, _, _, _, 0, _, Final).

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 3))),
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count, ListaAux),!.

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    nth1(3, X, Y3),
    member(Y3, Sig),
    (
    (length(Count, 0),
    Count1 = [Index | Count],
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count1, ListaAux),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Index | Count]],
    borrarTrioFila(Sudoku, Count1, Fila, 9, ListaSin),
    regla3Fila(ListaSin, Pos, X, Fila, N1, Count1, ListaAux),!)
    ).

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux):-
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count, ListaAux),!.
%-------------------------------------------------------------------------------
regla3Columna(Final, _, _, _, 0, _, Final).

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 3))),
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count, ListaAux),!.

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    nth1(3, X, Y3),
    member(Y3, Sig),
    (
    (length(Count, 0),
    Count1 = [Index | Count],
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count1, ListaAux),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Index | Count]],
    borrarTrioColumna(Sudoku, Count1, Columna, 9, ListaSin),
    regla3Columna(ListaSin, Pos, X, Columna, N1, Count1, ListaAux),!)
    ).

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux):-
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count, ListaAux),!.

%-------------------------------------------------------------------------------
regla3Cuadrante(Final, _, _, _, 0, _, _, _, Final).

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    N is 0,
    M1 is M - 1,
    regla3Cuadrante(Sudoku, Pos, X, 3, M1, Fila, Columna, Count, ListaAux),!.

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    (Pos is Posicion;
    number(Elem);
    (not(number(Elem)),
    length(Elem, Longitud),
    not(Longitud is 3) )),
    N >= 1,
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count, ListaAux),!.

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    nth1(3, X, Y3),
    member(Y3, Sig),
    (
    (length(Count, 0),
    Count1 = [Posicion | Count],
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count1, ListaAux),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Posicion | Count]],
    borrarTrioCuadrante(Sudoku, Count1, Fila, Columna, 3, 3, ListaSin),
    regla3Cuadrante(ListaSin, Pos, X, N1, M, Fila, Columna, Count1, ListaAux),!)    %Cambio cuestionable <---------------------------------------
    ).

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count, ListaAux),!.
%-------------------------------------------------------------------------------
borrarTrioFila(Final, _, _, 0, Final).

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count , Pos3),
    (Pos1 is Index;
    Pos2 is Index;
    Pos3 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarTrioFila(Sudoku, Count, Fila, N1, SudokuAux),!.

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(1, Count, Pos1),
    borrarTrioGeneral(Sudoku, Index, Pos1, 3, SudokuSin),
    N1 is N - 1,
    borrarTrioFila(SudokuSin, Count, Fila, N1, SudokuAux),!.

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux):-
    N1 is N - 1,
    borrarTrioFila(Sudoku, Count, Fila, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
borrarTrioColumna(Final, _, _, 0, Final).

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count , Pos3),
    (Pos1 is Index;
    Pos2 is Index;
    Pos3 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarTrioColumna(Sudoku, Count, Columna, N1, SudokuAux),!.

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(1, Count, Pos1),
    borrarTrioGeneral(Sudoku, Index, Pos1, 3, SudokuSin),
    N1 is N - 1,
    borrarTrioColumna(SudokuSin, Count, Columna, N1, SudokuAux),!.

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux):-
    N1 is N - 1,
    borrarTrioColumna(Sudoku, Count, Columna, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
borrarTrioCuadrante(Final, _, _, _, _, 0, Final).

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux):-
    N is 0,
    M1 is M - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, 3, M1, SudokuAux),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count, Pos3),
    (Pos1 is Posicion;
    Pos2 is Posicion;
    Pos3 is Posicion;
    number(Elem)),
    N1 is N - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N1, M, SudokuAux),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(1, Count, Pos1),
    borrarTrioGeneral(Sudoku, Posicion, Pos1, 3, SudokuSin),
    N1 is N - 1,
    borrarTrioCuadrante(SudokuSin, Count, Fila, Columna, N1, M, SudokuAux),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux):-
    N1 is N - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N1, M, SudokuAux),!.
%-------------------------------------------------------------------------------
borrarTrioGeneral(Final, _, _, 0, Final).

borrarTrioGeneral(Sudoku, Index, Pos1, N, SudokuAux):-
    nth1(Index, Sudoku, Elem),
    nth1(Pos1, Sudoku, Pos),
    nth1(N, Pos, X),
    (
    (member(X, Elem),
    select(X, Elem, Borrada),
    sustituir_elemento(Sudoku, Index, Borrada, SudokuSin),
    N1 is N - 1,
    borrarTrioGeneral(SudokuSin, Index, Pos1, N1, SudokuAux),!)
    ;
    (N1 is N - 1,
    borrarTrioGeneral(Sudoku, Index, Pos1, N1, SudokuAux),!)
    ).
%-------------------------------------------------------------------------------
buscar(Sudoku, N, Resultado):-
    buscar_posibilidades(Sudoku, SudokuAux),
    buscar_casos(SudokuAux, N, Resultado).

buscar_casos(Final, 0, Final).

buscar_casos(Sudoku, N, Resultado):-
    regla0(Sudoku, ListaAux),
    regla1(ListaAux, ListaAux2),
    regla2(ListaAux2, ListaAux3),
    regla3(ListaAux3, ListaAux4),
    N1 is N - 1,
    buscar_casos(ListaAux4, N1, Resultado),!.
%-------------------------------------------------------------------------------
trampa(Sudoku, Resultado):-
    sustituir_elemento(Sudoku, 61, [2, 3, 6], SudokuSin),
    sustituir_elemento(SudokuSin, 55, [2, 3, 8], SudokuSin2),
    sustituir_elemento(SudokuSin2, 56, [6, 7], SudokuSin3),
    sustituir_elemento(SudokuSin3, 62, [2, 3, 6, 9], SudokuSin4),
    Resultado = SudokuSin4.
%-------------------------------------------------------------------------------



