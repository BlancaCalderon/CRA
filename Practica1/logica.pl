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

%Busca los distintos n�meros que pueden ir en una pisci�n maracada por .
buscar_posibilidades(L, RP):-
    buscar_posibilidades_aux([],P, L, 1),       %Llama a funci�n auxiliar empezando en la posicion 1 del sudoku, L el sudoku , empezando lista vacia
    reverse(P, RP).
    %mostrar_sudoku(RP).

buscar_posibilidades_aux(F, F, _, 82).          %Cuando se llega al final del sudoku (pos 82)

buscar_posibilidades_aux(F, P, L, N):-
    nth1(N, L, X),                                      %obtiene de L el elemento N y lo mete en X L --> sudoku N --> numero de la posicion del que queremos pillar elemento
    not('.'==X),                                        %Si elemento actual no es un .
    N1 is N + 1,                                        %inicializa variable N con valor N + 1(is cuando es un constante compara y cuando e suna variable es comparacion)
    buscar_posibilidades_aux([X | F], P, L, N1), !.     %a�ade elemento X F--> lista que se va a llenar en la recursividad, P --> retorno(posibilidades) N1 -->siguiente posicion

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
    obtenerFilaAux(Sudoku, Fila, 8, [], ValoresFila).       %Recorre primera fila (posici�n 8)

obtenerFilaAux(_, _, -1, P, P).        %Para terminar la ejecuci�n cuando posici�n es -1

obtenerFilaAux(Sudoku, Fila, N, Lista, ValoresFila):-   %Funcion auxiliar para encontrar los valores de una fila
    Index is (Fila - 1) * 9 + N,                        %Calcula la posici�n del index correspondiente al elemento
    nth0(Index, Sudoku, Valor),                         %Obtiene el valor de index de sudoku y lo introduce en valor
    NuevosValores = [Valor | Lista],                    %Introduce el nuevo valor en la lista
    N1 is N - 1,                                        %Recorre posici�n anterior
    obtenerFilaAux(Sudoku, Fila, N1, NuevosValores, ValoresFila).       %Llamada recursiva comprobando siguiente valor
%-------------------------------------------------------------------------------
obtenerColumna(Sudoku, Columna, ValoresColumna) :-                  %Obtiene los valores posibles de una columna
    obtenerColumnaAux(Sudoku, Columna, 8, [], ValoresColumna).      %Primera llamada a la funci�n auxiliar empezando en pos 8
    
obtenerColumnaAux(_, _, -1, P, P).        %Para terminar la ejecuci�n de la funci�n cuando posici�n es -1

obtenerColumnaAux(Sudoku, Columna, N, Lista, ValoresColumna):-    %Funcion auxiliar que saca los posibles valores de una columna
    Index is (Columna - 1) + N * 9,                               %Calcula el index correspondiente
    nth0(Index, Sudoku, Valor),                                   %Saca el valor correspondiente al idnex del sudoku y lo introduce en valor
    NuevosValores = [Valor | Lista],                                                  %Introduce el valor en la lista de nuevso valores
    N1 is N - 1,                                                                      %Para recorrer siguiente elemento
    obtenerColumnaAux(Sudoku, Columna, N1, NuevosValores, ValoresColumna).            %Llamada recursiva para sacar los valores de la siguiente posicion
%-------------------------------------------------------------------------------
obtenerBloque(Vector, Fila, Columna, ValoresBloque) :-        %Funcion que obtiene los valores podibles del bloque en el que nos encontramos
    InicioFila is (Fila - 1) // 3 * 3,                        %Calcula posici�n en la que comienza la fila
    InicioColumna is (Columna - 1) // 3 * 3,                  %Calcula la posici�n en el que comienza la columna
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

%Meter funcion de actualizar

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
    regla0Aux(Sudoku, 81, [], Resultado).

regla1Aux(_, 0, Resultado, Resultado).

regla1Aux(_, 0, Resultado, Resultado):-
    write('hola'), nl.




