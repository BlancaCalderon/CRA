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
    NuevosValores = [Valor | Lista],                              %Introduce el valor en la lista de nuevso valores
    N1 is N - 1,                                                  %Para recorrer siguiente elemento
    obtenerColumnaAux(Sudoku, Columna, N1, NuevosValores, ValoresColumna).             %Llamada recursiva para sacar los valores de la siguiente posicion
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
%Meter funcion de actualizar

actualizar_sudoku(Sudoku, Resultado):-        %Funcion que actualiza el sudoku eliminando elementos repetidos en una fila, columna, bloque
    actualizarSudokuAux(Sudoku, 81, [], Resultado).    %Llamada recursiva comenzando por posicion final
    
actualizarSudokuAux(_, 0, Resultado, Resultado).       %Funci�n para terminar la recursividad

actualizarSudokuAux(Sudoku, N, Lista, Resultado):-
    nth1(N, Sudoku, X),                                %Saca posici�n 1 del sudoku y lo guarda en X
    valoresPosibles(Num),                              %Guarda en Num todos los valores posibles ya definidos
    member(X, Num),
    obtener_ejes(N, J1, J2),                           %Obtiene los ejes correpondientes al elemento N
    actualizarFila(Sudoku, J1, X, ListaAux),           %Llama a la funci�n de acrualizar fila pasandole el valor del eje X
    length(ListaAux, Tamm),
    write('Indice = '), write(Tamm),nl,
    mostrar_sudoku(ListaAux),
    write('Resulatado = '), write(ListaAux), nl,
    actualizarColumna(),
    actualizarBloque(),
    N1 is N -1,                                         %Pasa al siguiente elemento a actualizar
    actualizarSudokuAux(ListaAux, N1, Lista, Resultado), !.     %Llamada recursiva para actualizar el siguiente elemento
    
actualizarSudokuAux(Sudoku, N, Lista, Resultado):-
    N1 is N - 1,
    actualizarSudokuAux(Sudoku, N1, Lista, Resultado), !.
    
%-------------------------------------------------------------------------------
actualizarFila(Sudoku, Fila, Elem, ListaAux):-
    actualizarFilaAux(Sudoku, Fila, Elem, 9, [], ListaAux).
    %write(ListaAux),nl.
    
actualizarFilaAux(_, _, _ , 0, ListaAux, ListaAux).

actualizarFilaAux(Sudoku, Fila, Elem, N, Lista, ListaAux):-
    N is Fila,
    quitarElementoFila(Sudoku, N, 9, Elem, [], Listita),
    N1 is N - 1,
    actualizarFilaAux(Sudoku, Fila, Elem, N1, [Listita | Lista], ListaAux),!.

actualizarFilaAux(Sudoku, Fila, Elem, N, Lista, ListaAux):-
    %write('Elemento de entrada' ), write(Elem), nl,
    recorrerFila(Sudoku, N, 9, [], Listita),
    %No se puede modificar argumentos creo que es necesario crear una nueva variable
    %write('Salida del recorrido '), write(Lista), nl,
    N1 is N - 1,
    actualizarFilaAux(Sudoku, Fila, Elem, N1, [Listita | Lista], ListaAux),!.
    
recorrerFila(_, _, 0, Final, Final).

recorrerFila(Sudoku, N, M, L1, Final):-
    %write('M '), write(M), write(' -- N'), write(N), nl,
    Elemento is (N - 1) * 9 + M,
    %write('Elemento '), write(Elemento), nl,
    nth1(Elemento, Sudoku, X),
    %write('Vamos a ver si va bien '), write(X), nl,
    M1 is M - 1,
    recorrerFila(Sudoku, N, M1, [X | L1], Final),!.
    
quitarElementoFila(_, _, 0, _, Final, Final).

quitarElementoFila(Sudoku, N, M, Elem, L1, Final):-
    %write('M '), write(M), write(' -- N'), write(N), nl,
    Elemento is (N - 1) * 9 + M,
    valoresPosibles(Num),
    %write('Elemento '), write(Elemento), nl,
    nth1(Elemento, Sudoku, X),
    write('Si estoy AQUIIIIIIIIIIIIIIIIIIIIII'), nl,
    member(X, Num),
    write('Vamos a ver si va bien '), write(X), nl,
    M1 is M - 1,
    recorrerFila(Sudoku, N, M1, Elem, [X | L1], Final),!.

quitarElementoFila(Sudoku, N, M, Elem, L1, Final):-
    Elemento is (N - 1) * 9 + M,
    write('No puedo continuar'), nl,
    nth1(Elemento, Sudoku, X),
    write('DEBUGGGGGGGGGGGGGGGG'), write(Elem),nl,
    write(X),nl,
    member(Elem, X),
    select(Elem, X, Resultado),
    write(Resultado),nl,
    M1 is M - 1,
    recorrerFila(Sudoku, N, M1, [Resultado | L1], Final),!.

quitarElementoFila(Sudoku, N, M, Elem, L1, Final):-
    M1 is M - 1,
    recorrerFila(Sudoku, N, M1, [Resultado | L1], Final),!.

%-------------------------------------------------------------------------------
actualizarColumna().


actualizarBloque().


%-------------------------------------------------------------------------------
%Reglas de simplificacion
%Regla 0 --> Si hay un lugar donde solo cabe un numero, lo escribimos en el lugar correspondiente y lo eliminamos de los lugares en los que aparezca de los que son conflictivos
regla0(Sudoku, Resultado):-
    regla0Aux(Sudoku, 81, [], Resultado).

regla0Aux(_, 0, Resultado, Resultado).

regla0Aux(Sudoku, N, Lista, Resultado):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),
    N1 is N - 1,
    regla0Aux(Sudoku, N1, [X | Lista], Resultado),!.

regla0Aux(Sudoku, N, Lista, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Tam),
    Tam > 1,
    N1 is N - 1,
    regla0Aux(Sudoku, N1, [X | Lista], Resultado),!.

regla0Aux(Sudoku, N, Lista, Resultado):-
    nth1(N, Sudoku, X),
    N1 is N - 1,
    nth1(1, X, X1),
    regla0Aux(Sudoku, N1, [X1 | Lista], Resultado),!.

%-------------------------------------------------------------------------------
regla1(Sudoku, Resultado):-
    regla0Aux(Sudoku, 81, [], Resultado).

regla1Aux(_, 0, Resultado, Resultado).

regla1Aux(_, 0, Resultado, Resultado):-
    write('hola'), nl.




