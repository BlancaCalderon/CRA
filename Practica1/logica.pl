%Predicados que muestran el sudoku separando cada linea con _ _ _ y cada colunna con |
mostrar_sudoku([]):-
    nl, write('-------------------'), nl.

mostrar_sudoku([X|L]):-
    length(L, N),
    0 is mod((N+1), 9),                                                         %Determina si se ha pasado a la siguiente línea
    nl, write('-------------------'), nl,
    write('|'), write(X), write('|'),
    mostrar_sudoku(L),!.

mostrar_sudoku([X|L]):-
    write(X), write('|'),
    mostrar_sudoku(L).
%-------------------------------------------------------------------------------
%Predicados para buscar las distintas posibilidades de las posiciones que contienen . de sudoku
%-------------------------------------------------------------------------------
filas([1, 2, 3, 4, 5, 6, 7, 8, 9]).
columnas([1, 2, 3, 4, 5, 6, 7, 8, 9]).

%Busca los distintos números que pueden ir en una posición maracada por .
buscar_posibilidades(L, RP):-
    buscar_posibilidades_aux([],P, L, 1),                                       %Llama a función auxiliar empezando en la posicion 1 del sudoku
    reverse(P, RP).

buscar_posibilidades_aux(F, F, _, 82).                                          %Ejeucición termina al llegar al final del sudoku (pos 82)

buscar_posibilidades_aux(F, P, L, N):-
    nth1(N, L, X),
    not('.'==X),                                                                %Comprueba si el elemento no es un . (si no lo es se pasa a revisar siguiente posición)
    N1 is N + 1,
    buscar_posibilidades_aux([X | F], P, L, N1), !.                             %Llamada recursiva para revisar la siguiente posición

buscar_posibilidades_aux(F, P, L, N):-
    obtener_ejes(N, J1, J2),                                                    %Obtiene los ejes correspondientes en los que se encuentra nuestro elemento (J1 --> eje x, j2 --> Eke y)
    N1 is N + 1,
    posiblesNumeros(L, J1, J2, OP),
    buscar_posibilidades_aux([OP | F], P, L, N1), !.                            %llamada recursiva pasando al siguiente elemento

%Predicado que obtiene los ejes x,y de un elemento N del sudoku
obtener_ejes(N, J1, J2):-
    columnas(C), filas(F),
    member(J1, C), member(J2, F),
    N is (J1 - 1) * 9 + J2.
%-------------------------------------------------------------------------------
valoresPosibles([1, 2, 3, 4, 5, 6, 7, 8, 9]).

%Busca posibilidades para una celda, sacando los valores por fila,columna y cuadrante y quitandoselo a los valores posibles iniciales
posiblesNumeros(Vector, Fila, Columna, Posibles) :-
        valoresPosibles(ValoresPosibles),
        obtenerFila(Vector, Fila, ValoresFila),!,
        obtenerColumna(Vector, Columna, ValoresColumna),
        obtenerBloque(Vector, Fila, Columna, ValoresBloque),
        union(ValoresFila, ValoresColumna, ValoresTemp),
        union(ValoresTemp, ValoresBloque, ValoresAux),                          %Une todos los valores encontrados anteriormenete en valoresAux(valores que aparecen)
        subtract(ValoresPosibles, ValoresAux, Posibles).                        %Quita valores encontrados a los valores posibles iniciales y devuelve las posibilidades para la posición
%-------------------------------------------------------------------------------
%Obtiene los números ya presentes en una fila
obtenerFila(Sudoku, Fila, ValoresFila) :-
    obtenerFilaAux(Sudoku, Fila, 8, [], ValoresFila).                           %Recorre la fila en orden inverso

obtenerFilaAux(_, _, -1, P, P).                                                 %Lega al final de la fila terminando ejecución

obtenerFilaAux(Sudoku, Fila, N, Lista, ValoresFila):-
    Index is (Fila - 1) * 9 + N,
    nth0(Index, Sudoku, Valor),
    NuevosValores = [Valor | Lista],
    N1 is N - 1,
    obtenerFilaAux(Sudoku, Fila, N1, NuevosValores, ValoresFila).               %Llamada recursiva comprobando siguiente valor
%-------------------------------------------------------------------------------
%Obtiene los números ya presentes en una columna
obtenerColumna(Sudoku, Columna, ValoresColumna) :-
    obtenerColumnaAux(Sudoku, Columna, 8, [], ValoresColumna).                  %Primera llamada para recorrer columna empezando en pos 8

obtenerColumnaAux(_, _, -1, P, P).                                              %Termina la ejecución de la función cuando posición es -1

obtenerColumnaAux(Sudoku, Columna, N, Lista, ValoresColumna):-
    Index is (Columna - 1) + N * 9,
    nth0(Index, Sudoku, Valor),
    NuevosValores = [Valor | Lista],
    N1 is N - 1,
    obtenerColumnaAux(Sudoku, Columna, N1, NuevosValores, ValoresColumna).
%-------------------------------------------------------------------------------
%Obtiene los números ya presentes en un cuadrante
obtenerBloque(Vector, Fila, Columna, ValoresBloque) :-
    InicioFila is (Fila - 1) // 3 * 3,                                          %Calcula posición en la que comienza la fila
    InicioColumna is (Columna - 1) // 3 * 3,                                    %Calcula la posición en el que comienza la columna
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
%Predicados para sustituir y actualizar los elementos del sudoku
%-------------------------------------------------------------------------------
%Sustituye un elemento del sudoku
sustituir_elemento(Vector, Indice, NuevoElemento, NuevoVector) :-
    nth1(Indice, Vector, _, Resto),
    nth1(Indice, NuevoVector, NuevoElemento, Resto).

%Actualiza el sudoku (actualiza la fila, columna y cuadrante correspondiente a la posición pasada)
actualizar_sudoku(Sudoku, N, Resultado):-
     obtener_ejes(N, J1, J2),
     nth1(N, Sudoku, X),
     actualizarFila(Sudoku, X, 9, J1, ListaAux),
     actualizarColumna(ListaAux, X, 9, J2, ListaAux2),
     actualizarCuadrado(ListaAux2, X, J1, J2, 3, 3, ListaAux3),
     Resultado = ListaAux3.
%-------------------------------------------------------------------------------
%Actualiza elementos de la fila correspondiente
actualizarFila(Final, _, 0, _, Final).

actualizarFila(Sudoku, X, N, Fila, ListaFinal):-
     Index is (Fila - 1) * 9 + N,
     nth1(Index, Sudoku, Y),
     member(X, Y),                                                              %Comprueba si elemento de la posición N es igual al pasado
     select(X, Y, Borrada),                                                     %Elimina el elemento si es igual
     sustituir_elemento(Sudoku, Index, Borrada, SudokuAux),
     N1 is N - 1,
     actualizarFila(SudokuAux, X, N1, Fila, ListaFinal),!.                      %Llamada recursiva para revisar el siguiente elemento

%Si el elemnto no es igual al pasado no se borra y se pasa a revisar la siguiente posición
actualizarFila(Sudoku, X, N, Fila, ListaFinal):-
     N1 is N - 1,
     actualizarFila(Sudoku, X, N1, Fila, ListaFinal),!.                        %Llamada recursiva para revisar el siguiente elemento
%-------------------------------------------------------------------------------
%Actualiza elementos de la columna correspondiente
actualizarColumna(Final, _, -1, _, Final).

actualizarColumna(Sudoku, X, N, Columna, ListaFinal):-
     Index is (Columna) + N * 9,
     nth1(Index, Sudoku, Y),                                                    %Comprueba si elemento de la posición N es igual al pasado
     member(X, Y),                                                              %Elimina el elemento si es igual
     select(X, Y, Borrada),
     sustituir_elemento(Sudoku, Index, Borrada, SudokuAux),
     N1 is N - 1,
     actualizarColumna(SudokuAux, X, N1, Columna, ListaFinal),!.                %Llamada recursiva para revisar el siguiente elemento

%Si el elemnto no es igual al pasado no se borra y se pasa a revisar la siguiente posición
actualizarColumna(Sudoku, X, N, Columna, ListaFinal):-
     N1 is N - 1,
     actualizarColumna(Sudoku, X, N1, Columna, ListaFinal),!.                   %Llamada recursiva para revisar el siguiente elemento
%-------------------------------------------------------------------------------
%Actualiza elementos del cuadrante correspondiente
actualizarCuadrado(Final, _, _, _, _, 0, Final).

actualizarCuadrado(Sudoku, X, Fila, Columna, ContF, ContC, ListaFinal):-
    ContF is 0,                                                                 %Contador de la fila comienza en 0
    N1 is ContC - 1,                                                            %ContC es el contador de las columnas re le resta uno para revisar siguiente elemento(siguiente columna de la misma fila)
    actualizarCuadrado(Sudoku, X, Fila, Columna, 3, N1, ListaFinal),!.

actualizarCuadrado(Sudoku, X, Fila, Columna, ContF, ContC, ListaFinal):-
     InicioFila is ((Fila - 1) // 3 * 3) + ContF - 1,
     InicioColumna is ((Columna - 1) // 3 * 3) + ContC,
     Posicion is InicioFila * 9 + InicioColumna,
     valoresPosibles(Num),
     nth1(Posicion, Sudoku, Y),
     not(member(Y, Num)),
     select(X, Y, Borrada),
     sustituir_elemento(Sudoku, Posicion, Borrada, SudokuAux),
     N1 is ContF - 1,
     actualizarCuadrado(SudokuAux, X, Fila, Columna, N1, ContC, ListaFinal),!.

actualizarCuadrado(Sudoku, X, Fila, Columna, ContF, ContC, ListaFinal):-
    N1 is ContF - 1,
    actualizarCuadrado(Sudoku, X, Fila, Columna, N1, ContC, ListaFinal),!.

%-------------------------------------------------------------------------------
%Predicados para llevar a cabo las reglas de simplificacion
%-------------------------------------------------------------------------------
%Regla 0 --> Si hay un lugar donde solo cabe un numero, lo escribimos en el lugar correspondiente y lo eliminamos de los lugares en los que aparezca de los que son conflictivos
%-------------------------------------------------------------------------------
regla0(Sudoku, Resultado):-
    regla0Aux(Sudoku, 81, Resultado).                                           %Se recorre el sudoku al revés
                                                                                %Termina recorrido del sudoku
regla0Aux(Resultado, 0, Resultado).

regla0Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),                                                             %Comprueba si elemento actual es uno de los valores posibles(es un número y no una lista de posibilidades)
    N1 is N - 1,
    regla0Aux(Sudoku, N1, Resultado),!.                                         %lLamada recursiva para revisar siguiente posición

regla0Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Tam),
    Tam > 1,                                                                    %Si tamaño del elemento actual es mayor que 1(hay mas de una posibilidad en la lista)
    N1 is N - 1,                                                                %Se pasa a revisar siguiente elemento al no cumplir la regla
    regla0Aux(Sudoku, N1, Resultado),!.                                         %lLamada recursiva para revisar siguiente posición

%Cuando tamaño de la lista de posibilidades es 1 y no es un número ya establecido se sustituye elemento y actualiza sudoku
regla0Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    N1 is N - 1,
    nth1(1, X, X1),
    sustituir_elemento(Sudoku, N, X1, SudokuAux),
    actualizar_sudoku(SudokuAux, N, Prueba),
    regla0Aux(Prueba, N1, Resultado),!.                                         %lLamada recursiva para revisar siguiente posición

%-------------------------------------------------------------------------------
%Regla 1 -> Si hay un numero que aparece en una sola de las listas que aparecen en una fila, columna o cuadro, cambiamos la lista por el numero y borramos el numero del resto de listas de la fila, columna o cuadro.
%-------------------------------------------------------------------------------
regla1(Sudoku, Resultado):-
    regla1Aux(Sudoku, 81, Resultado).                                           %Se recorre el sudoku al revés

regla1Aux(Resultado, 0, Resultado).                                             %Termina recorrido del sudoku

regla1Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),                                                             %Comprueba si elemento actual es uno de los valores posibles(es un número y no una lista de posibilidades)
    N1 is N - 1,
    regla1Aux(Sudoku, N1, Resultado),!.                                         %lLamada recursiva para revisar siguiente posición

%Si el elemento no es un número ya establecido se comprueba si es el único en la fila, columna y cuadrante (se cumple la regla)
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
%Recorre cuadrante correspondiente a la posición pasda comprobando si el elemento no se repite en ese cuadrante
regla1Cuadrante(Final, _, _, 0, _, _, _, _, Final).

%Si se llega al final del cuadrante sin haber encontrado otro elemento igual se sustituye y actualiza sudoku
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
    Pos is Posicion,                                                            %Si nos encontramos en la posición pasada y no es la última se pasa a revisar siguiente posición
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
    member(Y, Sig),                                                             %Si una posibilidad del elemento a revisar es igual que el del pasado pasamos a comprobar siguiente valor posible
    Tam1 is Tam - 1,                                                            %Disminuye tamaño para revisar siguiente posibilidad de la lista
    regla1Cuadrante(Sudoku, Pos, X, Tam1, 3, 3, Fila, Columna, ListaAux),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux):-
    N1 is N - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, N1, M, Fila, Columna, ListaAux),!.
%-------------------------------------------------------------------------------
%Recorre columna correspondiente a la posición pasda comprobando si el elemento no se repite en ese cuadrante
regla1Columna(Final, _, _, 0, _, _, Final).

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    Pos is Index,                                                               %Si nos encontramos en la posición pasada (nosotros mismos) y no es la última se pasa a revisar siguiente posición
    N1 is N - 1,
    regla1Columna(Sudoku, Pos, X, Tam, N1, Columna, ListaAux),!.
    
%Si se llega al final de la columna sin haber encontrado otro elemento igual se sustituye y actualiza sudoku
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
    member(Y, Sig),                                                             %Si una posibilidad del elemento a revisar es igual que el del pasado pasamos a comprobar siguiente valor posible
    Tam1 is Tam - 1,
    regla1Columna(Sudoku, Pos, X, Tam1, 9, Columna, ListaAux),!.

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux):-
    N1 is N - 1,
    regla1Columna(Sudoku, Pos, X, Tam, N1, Columna, ListaAux),!.
%-------------------------------------------------------------------------------
%Recorre Fila correspondiente a la posición pasda comprobando si el elemento no se repite en ese cuadrante
regla1Fila(Final, _, _, 0, _, _, Final).

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux):-
    Index is (Fila - 1) * 9 + N,                                                %Si nos encontramos en la posición pasada y no es la última se pasa a revisar siguiente posición
    Pos is Index,
    N1 is N - 1,
    regla1Fila(Sudoku, Pos, X, Tam, N1, Fila, ListaAux),!.

%Si se llega al final de la fila sin haber encontrado otro elemento igual se sustituye y actualiza sudoku
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
    member(Y, Sig),                                                             %Si una posibilidad del elemento a revisar es igual que el del pasado pasamos a comprobar siguiente valor posible
    Tam1 is Tam - 1,
    regla1Fila(Sudoku, Pos, X, Tam1, 9, Fila, ListaAux),!.

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux):-
    N1 is N - 1,
    regla1Fila(Sudoku, Pos, X, Tam, N1, Fila, ListaAux),!.
%-------------------------------------------------------------------------------
%Regla 2 -> Si dos numeros aparecen solos en dos lugares distintos de una fila, columna o cuadro, los borramos del resto de lugares de la fila, columna o cuadro correspondiente.
%-------------------------------------------------------------------------------
regla2(Sudoku, Resultado):-
    regla2Aux(Sudoku, 81, Resultado).                                           %Se recorre el sudoku al revés

regla2Aux(Final, 0, Final).                                                     %Termina recorrido del sudoku

regla2Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    number(X),                                                                  %Comprueba si el elemento actual es un número ya establecido y si lo es pasa a revisar siguiente posición
    N1 is N - 1,
    regla2Aux(Sudoku, N1, Resultado),!.

regla2Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Longitud),
    not(Longitud is 2),                                                         %Comprueba si la longitud del elemento actual no es 2 (no cumple regla) pasando a revisar el siguiente
    N1 is N - 1,
    regla2Aux(Sudoku, N1, Resultado),!.

%Si el elemento no es un número y tiene longitud 2 cumpliendo requisitos de la regla se revisa si hay otro elemento igual en la fila, columna o cuadrante al que pertenece cumpliendo la regla
regla2Aux(Sudoku, N, Resultado):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    regla2Fila(Sudoku, N, X, J1, 9, ListaAux),
    regla2Columna(ListaAux, N, X, J2, 9, ListaAux2),
    regla2Cuadrante(ListaAux2, N, X, 3, 3, J1, J2, ListaAux3),
    N1 is N - 1,
    regla2Aux(ListaAux3, N1, Resultado),!.
%-------------------------------------------------------------------------------
%Recorre cuadrante comprobando si hay un elemento más igual al pasado (si se cumple regla)
regla2Cuadrante(Final, _, _, _, 0, _, _, Final).

regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    N is 0,
    M1 is M - 1,                                                                %Se revisa siguiente columna de la misma fila
    regla2Cuadrante(Sudoku, Pos, X, 3, M1, Fila, Columna, ListaAux),!.
    
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    (Pos is Posicion;                                                           %Si es la misma posición que la pasada (nosotros mismos), un numero o la longitud del elemento es mayor que 2 pasamos a revisar siguiente elementos ya que actual no es igual al pasado
    number(Elem);
    (not(
    number(Elem)),
    length(Elem, Longitud),
    Longitud > 2)),
    N >= 1,
    N1 is N - 1,
    regla2Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, ListaAux),!.

%Si el elemento actual no es un número y su longitud es 2(cumple condiciones de la regla) comprobamos si es igual al elemento pasado
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    borrarParejaCuadrante(Sudoku, Pos, Posicion, Fila, Columna, 3, 3, ListaSin),%Si los elementos son iguales se borran los elementos del cuadrante que tenga posibilidades iguales a las de la pareja
    N1 is N - 1,
    regla2Cuadrante(ListaSin, Pos, X, N1, M, Fila, Columna, ListaAux),!.
    
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux):-                %Si no son iguales se revisa siguiente elemento
    N1 is N - 1,
    regla2Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, ListaAux),!.
%-------------------------------------------------------------------------------
%Recorre colunmna comprobando si hay un elemento más igual al pasado (si se cumple regla)
regla2Columna(Final, _, _, _, 0, Final).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;                                                              %Si es la misma posición que la pasada (nosotros mismos), un numero o la longitud del elemento es mayor que 2 pasamos a revisar siguiente elementos ya que actual no es igual al pasado
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 2))),
    N1 is N - 1,
    regla2Columna(Sudoku, Pos, [X | Y], Columna, N1, ListaAux).
    
%Si el elemento actual no es un número y su longitud es 2(cumple condiciones de la regla) comprobamos si es igual al elemento pasado
regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    member(X, Elem),
    nth1(1, Y, Y1),
    member(Y1, Elem),
    borrarParejaColumna(Sudoku, Pos, Index, Columna, 9, ListaSin),              %Si los elementos son iguales se borran los elementos de la columna que tenga posibilidades iguales a las de la pareja
    N1 is N - 1,
    regla2Columna(ListaSin, Pos, [X | Y], Columna, N1, ListaAux).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux):-
    N1 is N - 1,                                                                %Si no son iguales se revisa siguiente elemento
    regla2Columna(Sudoku, Pos, [X | Y], Columna, N1, ListaAux).
%-------------------------------------------------------------------------------
%Recorre fila comprobando si hay un elemento más igual al pasado (si se cumple regla)
regla2Fila(Final, _, _, _, 0, Final).

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;                                                              %Si es la misma posición que la pasada (nosotros mismos), un numero o la longitud del elemento es mayor que 2 pasamos a revisar siguiente elementos ya que actual no es igual al pasado
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 2))),
    N1 is N - 1,
    regla2Fila(Sudoku, Pos, [X | Y], Fila, N1, ListaAux),!.

%Si el elemento actual no es un número y su longitud es 2(cumple condiciones de la regla) comprobamos si es igual al elemento pasado
regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    member(X, Elem),
    nth1(1, Y, Y1),
    member(Y1, Elem),
    borrarParejaFila(Sudoku, Pos, Index, Fila, 9, ListaSin),                    %Si los elementos son iguales se borran los elementos de la columna que tenga posibilidades iguales a las de la pareja
    N1 is N - 1,
    regla2Fila(ListaSin, Pos, [X | Y], Fila, N1, ListaAux),!. %X --> [X | Y]

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux):-
    N1 is N - 1,                                                                %Si no son iguales se revisa siguiente elemento
    regla2Fila(Sudoku, Pos, [X | Y], Fila, N1, ListaAux),!.
%-------------------------------------------------------------------------------
%Borra las posibilidades de los elementos del cuadrante que sean iguales que las del elemento pasado
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
    (Pos1 is Posicion;                                                          %Si la posición a revisar no es una de las dos encontradas al aplicar la regla ni un número ya establecido no hacemos nada y pasamos a revisar la siguiente
    Pos2 is Posicion;
    number(Elem)),
    N1 is N - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux),!.
    
borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    borrarGeneral(Sudoku, Posicion, Pos1, 2, SudokuSin),                    %Llama a predicado que borra la posibilidad del elemento que sea igual a alguna del elemento pasado
    N1 is N - 1,
    borrarParejaCuadrante(SudokuSin, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux),!.
    
borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux):-
    N1 is N - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux),!.
%-------------------------------------------------------------------------------
%Borra las posibilidades de los elementos de la columna que sean iguales que las del elemento pasado
borrarParejaColumna(Final, _, _, _, 0, Final).

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos1 is Index;                                                             %Si la posición a revisar es una de las dos encontradas al aplicar la regla ni un número ya establecido no hacemos nada y pasamos a revisar la siguiente
    Pos2 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N1, SudokuAux),!.

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    borrarGeneral(Sudoku, Index, Pos1, 2, SudokuSin),                           %Llama a predicado que borra la posibilidad del elemento que sea igual a alguna del elemento pasado
    N1 is N - 1,
    borrarParejaColumna(SudokuSin, Pos1, Pos2, Columna, N1, SudokuAux),!.

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux):-
    N1 is N - 1,
    borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
%Borra las posibilidades de los elementos de la fila que sean iguales que las del elemento pasado
borrarParejaFila(Final, _, _, _, 0, Final).

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos1 is Index;
    Pos2 is Index;
    number(Elem)),                                                              %Si la posición a revisar es una de las dos encontradas al aplicar la regla ni un número ya establecido no hacemos nada y pasamos a revisar la siguiente
    N1 is N - 1,
    borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N1, SudokuAux),!.

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    borrarGeneral(Sudoku, Index, Pos1, 2, SudokuSin),                           %Llama a predicado que borra la posibilidad del elemento que sea igual a alguna del elemento pasado
    N1 is N - 1,
    borrarParejaFila(SudokuSin, Pos1, Pos2, Fila, N1, SudokuAux),!.

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux):-
    N1 is N - 1,
    borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
%Regla 3 -> Si en tres lugares de una fila, columna o cuadro solo aparecen tres numeros distintos, borramos los numeros de las restantes listas de la fila, columna o cuadro.
%-------------------------------------------------------------------------------
regla3(Sudoku, Resultado):-
    regla3Aux(Sudoku, 81, Resultado).                                           %Recorre sudoku

regla3Aux(Final, 0, Final).                                                     %Termina recorrido

regla3Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    number(X),                                                                  %Comprueba si el elemento actual es un numero y si lo es pasa a revisar siguiente posicion
    N1 is N - 1,
    regla3Aux(Sudoku, N1, Resultado),!.

regla3Aux(Sudoku, N, Resultado):-
    nth1(N, Sudoku, X),
    length(X, Longitud),
    not(Longitud is 3),                                                         %Si la longitud del elemento actual no es 3 (no cumple la regla) pasamos a revisar siguiente elemento
    N1 is N - 1,
    regla3Aux(Sudoku, N1, Resultado),!.

%Si elemento actual no es un número y su longitud es 3(cumple requisitos de la regla) comprobamos si en su fila, columna o cuadrante hay dos elementos mas iguales a el
regla3Aux(Sudoku, N, Resultado):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    regla3Fila(Sudoku, N, X, J1, 9, [], ListaAux),
    regla3Columna(ListaAux, N, X, J2, 9, [], ListaAux2),
    regla3Cuadrante(ListaAux2, N, X, 3, 3, J1, J2, [], ListaAux3),
    N1 is N - 1,
    regla3Aux(ListaAux3, N1, Resultado),!.
%-------------------------------------------------------------------------------
%Recorre la fila correspondiente revisando si hay dos elementos más igual al pasado
regla3Fila(Final, _, _, _, 0, _, Final).

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;                                                              %Si no somos nosotros mismos, el elemento no es un número y su longitud no es 3(no cumple condiciones de la regla) se pasa arevisar siguiente posición
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 3))),
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count, ListaAux),!.

%Si la longitud del elemento es 3 revisamos si es igual al pasado
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
    Count1 = [Index | Count],                                                   %Guarda posición del elemento igual encontrado
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count1, ListaAux),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Index | Count]],
    borrarTrioFila(Sudoku, Count1, Fila, 9, ListaSin),                          %Llama a predicado para borrar las posibilidades de los elementos de la fila que sean iguales a los del elemento encontrado
    regla3Fila(ListaSin, Pos, X, Fila, N1, Count1, ListaAux),!)
    ).

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux):-
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count, ListaAux),!.
%-------------------------------------------------------------------------------
%Recorre la columna correspondiente revisando si hay dos elementos más igual al pasado
regla3Columna(Final, _, _, _, 0, _, Final).

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;
    number(Elem);
    (length(Elem, Longitud),                                                    %Si no somos nosotros mismos, el elemento no es un número y su longitud no es 3(no cumple condiciones de la regla) se pasa arevisar siguiente posición
    not(Longitud is 3))),
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count, ListaAux),!.

%Si la longitud del elemento es 3 revisamos si es igual al pasado
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
    (length(Count, 0),                                                          %Guarda posición del elemento igual encontrado(igual al pasado)
    Count1 = [Index | Count],
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count1, ListaAux),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Index | Count]],
    borrarTrioColumna(Sudoku, Count1, Columna, 9, ListaSin),                    %Llama a predicado para borrar las posibilidades de los elementos de la columna que sean iguales a los del elemento encontrado
    regla3Columna(ListaSin, Pos, X, Columna, N1, Count1, ListaAux),!)
    ).

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux):-
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count, ListaAux),!.

%-------------------------------------------------------------------------------
%Recorre el cuadrante correspondiente revisando si hay dos elementos más igual al pasado
regla3Cuadrante(Final, _, _, _, 0, _, _, _, Final).

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    N is 0,
    M1 is M - 1,
    regla3Cuadrante(Sudoku, Pos, X, 3, M1, Fila, Columna, Count, ListaAux),!.

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),                                               %Si no somos nosotros mismos, el elemento no es un número y su longitud no es 3(no cumple condiciones de la regla) se pasa arevisar siguiente posición
    (Pos is Posicion;
    number(Elem);
    (not(number(Elem)),
    length(Elem, Longitud),
    not(Longitud is 3) )),
    N >= 1,
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count, ListaAux),!.

%Si la longitud del elemento es 3 revisamos si es igual al pasado
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
    (length(Count, 0),                                                          %Guarda posición del elemento igual encontrado(igual al pasado)
    Count1 = [Posicion | Count],
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count1, ListaAux),!)  %Llama a predicado para borrar las posibilidades de los elementos del cuadrante que sean iguales a los del elemento encontrado
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Posicion | Count]],
    borrarTrioCuadrante(Sudoku, Count1, Fila, Columna, 3, 3, ListaSin),
    regla3Cuadrante(ListaSin, Pos, X, N1, M, Fila, Columna, Count1, ListaAux),!)
    ).

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux):-
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count, ListaAux),!.
%-------------------------------------------------------------------------------
%Borra los elementos de la fila que tengas posibilidades iguales al del elemento encontrado
borrarTrioFila(Final, _, _, 0, Final).

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count , Pos3),
    (Pos1 is Index;                                                             %Si la posición actual es igual de una de las tres encontradas o un numero ya establecido se pasa a revisar la siguiente
    Pos2 is Index;
    Pos3 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarTrioFila(Sudoku, Count, Fila, N1, SudokuAux),!.

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux):-
    Index is (Fila - 1) * 9 + N,
    nth1(1, Count, Pos1),
    borrarGeneral(Sudoku, Index, Pos1, 3, SudokuSin),                           %Comprueba si alguna posibilidad es igual a alguna de los del elemento pasado y la borra si lo es
    N1 is N - 1,
    borrarTrioFila(SudokuSin, Count, Fila, N1, SudokuAux),!.

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux):-
    N1 is N - 1,
    borrarTrioFila(Sudoku, Count, Fila, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
%Borra los elementos de la columna que tengas posibilidades iguales al del elemento encontrado
borrarTrioColumna(Final, _, _, 0, Final).

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count , Pos3),
    (Pos1 is Index;                                                             %Si la posición actual es igual de una de las tres encontradas o un numero ya establecido se pasa a revisar la siguiente
    Pos2 is Index;
    Pos3 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarTrioColumna(Sudoku, Count, Columna, N1, SudokuAux),!.

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux):-
    Index is (Columna) + (N - 1) * 9,
    nth1(1, Count, Pos1),                                                       %Comprueba si alguna posibilidad es igual a alguna de los del elemento pasado y la borra si lo es
    borrarGeneral(Sudoku, Index, Pos1, 3, SudokuSin),
    N1 is N - 1,
    borrarTrioColumna(SudokuSin, Count, Columna, N1, SudokuAux),!.

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux):-
    N1 is N - 1,
    borrarTrioColumna(Sudoku, Count, Columna, N1, SudokuAux),!.
%-------------------------------------------------------------------------------
%Borra los elementos delcuadrante que tengas posibilidades iguales al del elemento encontrado
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
    Pos2 is Posicion;                                                           %Si la posición actual es igual de una de las tres encontradas o un numero ya establecido se pasa a revisar la siguiente
    Pos3 is Posicion;
    number(Elem)),
    N1 is N - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N1, M, SudokuAux),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(1, Count, Pos1),                                                       %Comprueba si alguna posibilidad es igual a alguna de los del elemento pasado y la borra si lo es
    borrarGeneral(Sudoku, Posicion, Pos1, 3, SudokuSin),
    N1 is N - 1,
    borrarTrioCuadrante(SudokuSin, Count, Fila, Columna, N1, M, SudokuAux),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux):-
    N1 is N - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N1, M, SudokuAux),!.
%-------------------------------------------------------------------------------
%Comprueba si el elemento actual tiene alguna posibilidad igual al del elemento que cumple la regla y si lo hay lo elimina sustituyendo elemento
borrarGeneral(Final, _, _, 0, Final).

borrarGeneral(Sudoku, Index, Pos1, N, SudokuAux):-
    nth1(Index, Sudoku, Elem),
    nth1(Pos1, Sudoku, Pos),
    nth1(N, Pos, X),
    (
    (member(X, Elem),                                                           %Si son iguales lo elimina
    select(X, Elem, Borrada),
    sustituir_elemento(Sudoku, Index, Borrada, SudokuSin),
    N1 is N - 1,
    borrarGeneral(SudokuSin, Index, Pos1, N1, SudokuAux),!)
    ;
    (N1 is N - 1,
    borrarGeneral(Sudoku, Index, Pos1, N1, SudokuAux),!)
    ).
%-------------------------------------------------------------------------------

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



