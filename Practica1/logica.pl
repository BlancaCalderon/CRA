%Predicados que muestran el sudoku separando cada linea con _ _ _ y cada colunna con |
mostrar_sudoku([]):-
    nl, write('-------------------'), nl.

mostrar_sudoku([X|L]):-
    length(L, N),
    0 is mod((N+1), 9),                                                         %Determina si se ha pasado a la siguiente l�nea
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

%Busca los distintos n�meros que pueden ir en una posici�n maracada por .
buscar_posibilidades(L, RP):-
    buscar_posibilidades_aux([],P, L, 1),                                       %Llama a funci�n auxiliar empezando en la posicion 1 del sudoku
    reverse(P, RP).

buscar_posibilidades_aux(F, F, _, 82).                                          %Ejeucici�n termina al llegar al final del sudoku (pos 82)

buscar_posibilidades_aux(F, P, L, N):-
    nth1(N, L, X),
    not('.'==X),                                                                %Comprueba si el elemento no es un . (si no lo es se pasa a revisar siguiente posici�n)
    N1 is N + 1,
    buscar_posibilidades_aux([X | F], P, L, N1), !.                             %Llamada recursiva para revisar la siguiente posici�n

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
        subtract(ValoresPosibles, ValoresAux, Posibles).                        %Quita valores encontrados a los valores posibles iniciales y devuelve las posibilidades para la posici�n
%-------------------------------------------------------------------------------
%Obtiene los n�meros ya presentes en una fila
obtenerFila(Sudoku, Fila, ValoresFila) :-
    obtenerFilaAux(Sudoku, Fila, 8, [], ValoresFila).                           %Recorre la fila en orden inverso

obtenerFilaAux(_, _, -1, P, P).                                                 %Lega al final de la fila terminando ejecuci�n

obtenerFilaAux(Sudoku, Fila, N, Lista, ValoresFila):-
    Index is (Fila - 1) * 9 + N,
    nth0(Index, Sudoku, Valor),
    NuevosValores = [Valor | Lista],
    N1 is N - 1,
    obtenerFilaAux(Sudoku, Fila, N1, NuevosValores, ValoresFila).               %Llamada recursiva comprobando siguiente valor
%-------------------------------------------------------------------------------
%Obtiene los n�meros ya presentes en una columna
obtenerColumna(Sudoku, Columna, ValoresColumna) :-
    obtenerColumnaAux(Sudoku, Columna, 8, [], ValoresColumna).                  %Primera llamada para recorrer columna empezando en pos 8

obtenerColumnaAux(_, _, -1, P, P).                                              %Termina la ejecuci�n de la funci�n cuando posici�n es -1

obtenerColumnaAux(Sudoku, Columna, N, Lista, ValoresColumna):-
    Index is (Columna - 1) + N * 9,
    nth0(Index, Sudoku, Valor),
    NuevosValores = [Valor | Lista],
    N1 is N - 1,
    obtenerColumnaAux(Sudoku, Columna, N1, NuevosValores, ValoresColumna).
%-------------------------------------------------------------------------------
%Obtiene los n�meros ya presentes en un cuadrante
obtenerBloque(Vector, Fila, Columna, ValoresBloque) :-
    InicioFila is (Fila - 1) // 3 * 3,                                          %Calcula posici�n en la que comienza la fila
    InicioColumna is (Columna - 1) // 3 * 3,                                    %Calcula la posici�n en el que comienza la columna
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

%Actualiza el sudoku (actualiza la fila, columna y cuadrante correspondiente a la posici�n pasada)
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
     member(X, Y),                                                              %Comprueba si elemento de la posici�n N es igual al pasado
     select(X, Y, Borrada),                                                     %Elimina el elemento si es igual
     sustituir_elemento(Sudoku, Index, Borrada, SudokuAux),
     N1 is N - 1,
     actualizarFila(SudokuAux, X, N1, Fila, ListaFinal),!.                      %Llamada recursiva para revisar el siguiente elemento

%Si el elemnto no es igual al pasado no se borra y se pasa a revisar la siguiente posici�n
actualizarFila(Sudoku, X, N, Fila, ListaFinal):-
     N1 is N - 1,
     actualizarFila(Sudoku, X, N1, Fila, ListaFinal),!.                        %Llamada recursiva para revisar el siguiente elemento
%-------------------------------------------------------------------------------
%Actualiza elementos de la columna correspondiente
actualizarColumna(Final, _, -1, _, Final).

actualizarColumna(Sudoku, X, N, Columna, ListaFinal):-
     Index is (Columna) + N * 9,
     nth1(Index, Sudoku, Y),                                                    %Comprueba si elemento de la posici�n N es igual al pasado
     member(X, Y),                                                              %Elimina el elemento si es igual
     select(X, Y, Borrada),
     sustituir_elemento(Sudoku, Index, Borrada, SudokuAux),
     N1 is N - 1,
     actualizarColumna(SudokuAux, X, N1, Columna, ListaFinal),!.                %Llamada recursiva para revisar el siguiente elemento

%Si el elemnto no es igual al pasado no se borra y se pasa a revisar la siguiente posici�n
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
regla0(Sudoku, Resultado, Parada, Parada1):-
    regla0Aux(Sudoku, 81, Resultado, Parada, Parada1).                          %Se recorre el sudoku al rev�s
                                                                                %Termina recorrido del sudoku
regla0Aux(Resultado, 0, Resultado, Cond, Cond).

regla0Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),                                                             %Comprueba si elemento actual es uno de los valores posibles(es un n�mero y no una lista de posibilidades)
    N1 is N - 1,
    regla0Aux(Sudoku, N1, Resultado, Parada, Parada1),!.                        %lLamada recursiva para revisar siguiente posici�n

regla0Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    length(X, Tam),
    Tam > 1,                                                                    %Si tama�o del elemento actual es mayor que 1(hay mas de una posibilidad en la lista)
    N1 is N - 1,                                                                %Se pasa a revisar siguiente elemento al no cumplir la regla
    regla0Aux(Sudoku, N1, Resultado, Parada, Parada1),!.                                         %lLamada recursiva para revisar siguiente posici�n

%Cuando tama�o de la lista de posibilidades es 1 y no es un n�mero ya establecido se sustituye elemento y actualiza sudoku
regla0Aux(Sudoku, N, Resultado, _, Parada1):-
    nth1(N, Sudoku, X),
    N1 is N - 1,
    nth1(1, X, X1),
    sustituir_elemento(Sudoku, N, X1, SudokuAux),
    actualizar_sudoku(SudokuAux, N, Prueba),
    regla0Aux(Prueba, N1, Resultado, 0, Parada1),!.                                         %lLamada recursiva para revisar siguiente posici�n

%-------------------------------------------------------------------------------
%Regla 1 -> Si hay un numero que aparece en una sola de las listas que aparecen en una fila, columna o cuadro, cambiamos la lista por el numero y borramos el numero del resto de listas de la fila, columna o cuadro.
%-------------------------------------------------------------------------------
regla1(Sudoku, Resultado, Parada, Parada1):-
    regla1Aux(Sudoku, 81, Resultado, Parada, Parada1).                                           %Se recorre el sudoku al rev�s

regla1Aux(Resultado, 0, Resultado, Cond, Cond).                                             %Termina recorrido del sudoku

regla1Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    valoresPosibles(Num),
    member(X, Num),                                                             %Comprueba si elemento actual es uno de los valores posibles(es un n�mero y no una lista de posibilidades)
    N1 is N - 1,
    regla1Aux(Sudoku, N1, Resultado, Parada, Parada1),!.                                         %lLamada recursiva para revisar siguiente posici�n

%Si el elemento no es un n�mero ya establecido se comprueba si es el �nico en la fila, columna y cuadrante (se cumple la regla)
regla1Aux(Sudoku, N, Resultado, Parada, Parada1):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    length(X, Tam),
    regla1Fila(Sudoku, N, X, Tam, 9, J1, ListaAux, Parada, Parada2),
    regla1Columna(ListaAux, N, X, Tam, 9, J2, ListaAux2, Parada2, Parada3),
    regla1Cuadrante(ListaAux2, N, X, Tam, 3, 3, J1, J2, ListaAux3, Parada3, Parada4),
    N1 is N - 1,
    regla1Aux(ListaAux3, N1, Resultado, Parada4, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre cuadrante correspondiente a la posici�n pasda comprobando si el elemento no se repite en ese cuadrante
regla1Cuadrante(Final, _, _, 0, _, _, _, _, Final, Cond, Cond).

%Si se llega al final del cuadrante sin haber encontrado otro elemento igual se sustituye y actualiza sudoku
regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux, _, Parada1):-
    M is 0,
    N is 0,
    nth1(Tam, X, Y),
    sustituir_elemento(Sudoku, Pos, Y, SudokuAux),
    actualizar_sudoku(SudokuAux, Pos, Resultado),
    regla1Cuadrante(Resultado, Pos, X, 0, N, M, Fila, Columna, ListaAux, 0, Parada1),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    Pos is Posicion,                                                            %Si nos encontramos en la posici�n pasada y no es la �ltima se pasa a revisar siguiente posici�n
    N > 0,
    N1 is N - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, N1, M, Fila, Columna, ListaAux, Parada, Parada1),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    N is 0,
    M1 is M - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, 3, M1, Fila, Columna, ListaAux, Parada, Parada1),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Tam, X, Y),
    nth1(Posicion, Sudoku, Sig),
    member(Y, Sig),                                                             %Si una posibilidad del elemento a revisar es igual que el del pasado pasamos a comprobar siguiente valor posible
    Tam1 is Tam - 1,                                                            %Disminuye tama�o para revisar siguiente posibilidad de la lista
    regla1Cuadrante(Sudoku, Pos, X, Tam1, 3, 3, Fila, Columna, ListaAux, Parada, Parada1),!.

regla1Cuadrante(Sudoku, Pos, X, Tam, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    N1 is N - 1,
    regla1Cuadrante(Sudoku, Pos, X, Tam, N1, M, Fila, Columna, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre columna correspondiente a la posici�n pasda comprobando si el elemento no se repite en ese cuadrante
regla1Columna(Final, _, _, 0, _, _, Final, Cond, Cond).

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    Pos is Index,                                                               %Si nos encontramos en la posici�n pasada (nosotros mismos) y no es la �ltima se pasa a revisar siguiente posici�n
    N1 is N - 1,
    regla1Columna(Sudoku, Pos, X, Tam, N1, Columna, ListaAux, Parada, Parada1),!.
    
%Si se llega al final de la columna sin haber encontrado otro elemento igual se sustituye y actualiza sudoku
regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux, _, Parada1):-
    N is 0,
    nth1(Tam, X, Y),
    sustituir_elemento(Sudoku, Pos, Y, SudokuAux),
    actualizar_sudoku(SudokuAux, Pos, Resultado),
    regla1Columna(Resultado, Pos, X, 0, 9, Columna, ListaAux, 0, Parada1),!.

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Sig),
    nth1(Tam, X, Y),
    member(Y, Sig),                                                             %Si una posibilidad del elemento a revisar es igual que el del pasado pasamos a comprobar siguiente valor posible
    Tam1 is Tam - 1,
    regla1Columna(Sudoku, Pos, X, Tam1, 9, Columna, ListaAux, Parada, Parada1),!.

regla1Columna(Sudoku, Pos, X, Tam, N, Columna, ListaAux, Parada, Parada1):-
    N1 is N - 1,
    regla1Columna(Sudoku, Pos, X, Tam, N1, Columna, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre Fila correspondiente a la posici�n pasda comprobando si el elemento no se repite en ese cuadrante
regla1Fila(Final, _, _, 0, _, _, Final, Cond, Cond).

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,                                                %Si nos encontramos en la posici�n pasada y no es la �ltima se pasa a revisar siguiente posici�n
    Pos is Index,
    N1 is N - 1,
    regla1Fila(Sudoku, Pos, X, Tam, N1, Fila, ListaAux, Parada, Parada1),!.

%Si se llega al final de la fila sin haber encontrado otro elemento igual se sustituye y actualiza sudoku
regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux, _, Parada1):-
    N is 0,
    nth1(Tam, X, Y),
    sustituir_elemento(Sudoku, Pos, Y, SudokuAux),
    actualizar_sudoku(SudokuAux, Pos, Resultado),
    regla1Fila(Resultado, Pos, X, 0, 9, Fila, ListaAux, 0, Parada1),!.

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Sig),
    nth1(Tam, X, Y),
    member(Y, Sig),                                                             %Si una posibilidad del elemento a revisar es igual que el del pasado pasamos a comprobar siguiente valor posible
    Tam1 is Tam - 1,
    regla1Fila(Sudoku, Pos, X, Tam1, 9, Fila, ListaAux, Parada, Parada1),!.

regla1Fila(Sudoku, Pos, X, Tam, N, Fila, ListaAux, Parada, Parada1):-
    N1 is N - 1,
    regla1Fila(Sudoku, Pos, X, Tam, N1, Fila, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Regla 2 -> Si dos numeros aparecen solos en dos lugares distintos de una fila, columna o cuadro, los borramos del resto de lugares de la fila, columna o cuadro correspondiente.
%-------------------------------------------------------------------------------
regla2(Sudoku, Resultado, Parada, Parada1):-
    regla2Aux(Sudoku, 81, Resultado, Parada, Parada1).                                           %Se recorre el sudoku al rev�s

regla2Aux(Final, 0, Final, Cond, Cond).                                                     %Termina recorrido del sudoku

regla2Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    number(X),                                                                  %Comprueba si el elemento actual es un n�mero ya establecido y si lo es pasa a revisar siguiente posici�n
    N1 is N - 1,
    regla2Aux(Sudoku, N1, Resultado, Parada, Parada1),!.

regla2Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    length(X, Longitud),
    not(Longitud is 2),                                                         %Comprueba si la longitud del elemento actual no es 2 (no cumple regla) pasando a revisar el siguiente
    N1 is N - 1,
    regla2Aux(Sudoku, N1, Resultado, Parada, Parada1),!.

%Si el elemento no es un n�mero y tiene longitud 2 cumpliendo requisitos de la regla se revisa si hay otro elemento igual en la fila, columna o cuadrante al que pertenece cumpliendo la regla
regla2Aux(Sudoku, N, Resultado, Parada, Parada1):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    regla2Fila(Sudoku, N, X, J1, 9, ListaAux, Parada, Parada2),
    regla2Columna(ListaAux, N, X, J2, 9, ListaAux2, Parada2, Parada3),
    regla2Cuadrante(ListaAux2, N, X, 3, 3, J1, J2, ListaAux3, Parada3, Parada4),
    N1 is N - 1,
    regla2Aux(ListaAux3, N1, Resultado, Parada4, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre cuadrante comprobando si hay un elemento m�s igual al pasado (si se cumple regla)
regla2Cuadrante(Final, _, _, _, 0, _, _, Final, Cond, Cond).

regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    N is 0,
    M1 is M - 1,                                                                %Se revisa siguiente columna de la misma fila
    regla2Cuadrante(Sudoku, Pos, X, 3, M1, Fila, Columna, ListaAux, Parada, Parada1),!.
    
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    (Pos is Posicion;                                                           %Si es la misma posici�n que la pasada (nosotros mismos), un numero o la longitud del elemento es mayor que 2 pasamos a revisar siguiente elementos ya que actual no es igual al pasado
    number(Elem);
    (not(
    number(Elem)),
    length(Elem, Longitud),
    Longitud > 2)),
    N >= 1,
    N1 is N - 1,
    regla2Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, ListaAux, Parada, Parada1),!.

%Si el elemento actual no es un n�mero y su longitud es 2(cumple condiciones de la regla) comprobamos si es igual al elemento pasado
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    borrarParejaCuadrante(Sudoku, Pos, Posicion, Fila, Columna, 3, 3, ListaSin, Parada, Parada2),%Si los elementos son iguales se borran los elementos del cuadrante que tenga posibilidades iguales a las de la pareja
    N1 is N - 1,
    regla2Cuadrante(ListaSin, Pos, X, N1, M, Fila, Columna, ListaAux, Parada2, Parada1),!.
    
regla2Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, ListaAux, Parada, Parada1):-                %Si no son iguales se revisa siguiente elemento
    N1 is N - 1,
    regla2Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre colunmna comprobando si hay un elemento m�s igual al pasado (si se cumple regla)
regla2Columna(Final, _, _, _, 0, Final, Cond, Cond).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;                                                              %Si es la misma posici�n que la pasada (nosotros mismos), un numero o la longitud del elemento es mayor que 2 pasamos a revisar siguiente elementos ya que actual no es igual al pasado
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 2))),
    N1 is N - 1,
    regla2Columna(Sudoku, Pos, [X | Y], Columna, N1, ListaAux, Parada, Parada1).
    
%Si el elemento actual no es un n�mero y su longitud es 2(cumple condiciones de la regla) comprobamos si es igual al elemento pasado
regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    member(X, Elem),
    nth1(1, Y, Y1),
    member(Y1, Elem),
    borrarParejaColumna(Sudoku, Pos, Index, Columna, 9, ListaSin, Parada, Parada2),              %Si los elementos son iguales se borran los elementos de la columna que tenga posibilidades iguales a las de la pareja
    N1 is N - 1,
    regla2Columna(ListaSin, Pos, [X | Y], Columna, N1, ListaAux, Parada2, Parada1).

regla2Columna(Sudoku, Pos, [X | Y], Columna, N, ListaAux, Parada, Parada1):-
    N1 is N - 1,                                                                %Si no son iguales se revisa siguiente elemento
    regla2Columna(Sudoku, Pos, [X | Y], Columna, N1, ListaAux, Parada, Parada1).
%-------------------------------------------------------------------------------
%Recorre fila comprobando si hay un elemento m�s igual al pasado (si se cumple regla)
regla2Fila(Final, _, _, _, 0, Final, Cond, Cond).

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;                                                              %Si es la misma posici�n que la pasada (nosotros mismos), un numero o la longitud del elemento es mayor que 2 pasamos a revisar siguiente elementos ya que actual no es igual al pasado
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 2))),
    N1 is N - 1,
    regla2Fila(Sudoku, Pos, [X | Y], Fila, N1, ListaAux, Parada, Parada1),!.

%Si el elemento actual no es un n�mero y su longitud es 2(cumple condiciones de la regla) comprobamos si es igual al elemento pasado
regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    member(X, Elem),
    nth1(1, Y, Y1),
    member(Y1, Elem),
    borrarParejaFila(Sudoku, Pos, Index, Fila, 9, ListaSin, Parada, Parada2),                    %Si los elementos son iguales se borran los elementos de la columna que tenga posibilidades iguales a las de la pareja
    N1 is N - 1,
    regla2Fila(ListaSin, Pos, [X | Y], Fila, N1, ListaAux, Parada2, Parada1),!. %X --> [X | Y]

regla2Fila(Sudoku, Pos, [X | Y], Fila, N, ListaAux, Parada, Parada1):-
    N1 is N - 1,                                                                %Si no son iguales se revisa siguiente elemento
    regla2Fila(Sudoku, Pos, [X | Y], Fila, N1, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Borra las posibilidades de los elementos del cuadrante que sean iguales que las del elemento pasado
borrarParejaCuadrante(Final, _, _, _, _, _, 0, Final, Cond, Cond).

borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    N is 0,
    M1 is M - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, 3, M1, SudokuAux, Parada, Parada1),!.

borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    (Pos1 is Posicion;                                                          %Si la posici�n a revisar no es una de las dos encontradas al aplicar la regla ni un n�mero ya establecido no hacemos nada y pasamos a revisar la siguiente
    Pos2 is Posicion;
    number(Elem)),
    N1 is N - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux, Parada, Parada1),!.
    
borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    borrarGeneral(Sudoku, Posicion, Pos1, 2, SudokuSin, Parada, Parada2),                    %Llama a predicado que borra la posibilidad del elemento que sea igual a alguna del elemento pasado
    N1 is N - 1,
    borrarParejaCuadrante(SudokuSin, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux, Parada2, Parada1),!.
    
borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    N1 is N - 1,
    borrarParejaCuadrante(Sudoku, Pos1, Pos2, Fila, Columna, N1, M, SudokuAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Borra las posibilidades de los elementos de la columna que sean iguales que las del elemento pasado
borrarParejaColumna(Final, _, _, _, 0, Final, Cond, Cond).

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos1 is Index;                                                             %Si la posici�n a revisar es una de las dos encontradas al aplicar la regla ni un n�mero ya establecido no hacemos nada y pasamos a revisar la siguiente
    Pos2 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N1, SudokuAux, Parada, Parada1),!.

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    borrarGeneral(Sudoku, Index, Pos1, 2, SudokuSin, Parada, Parada2),                           %Llama a predicado que borra la posibilidad del elemento que sea igual a alguna del elemento pasado
    N1 is N - 1,
    borrarParejaColumna(SudokuSin, Pos1, Pos2, Columna, N1, SudokuAux, Parada2, Parada1),!.

borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N, SudokuAux, Parada, Parada1):-
    N1 is N - 1,
    borrarParejaColumna(Sudoku, Pos1, Pos2, Columna, N1, SudokuAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Borra las posibilidades de los elementos de la fila que sean iguales que las del elemento pasado
borrarParejaFila(Final, _, _, _, 0, Final, Cond, Cond).

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos1 is Index;
    Pos2 is Index;
    number(Elem)),                                                              %Si la posici�n a revisar es una de las dos encontradas al aplicar la regla ni un n�mero ya establecido no hacemos nada y pasamos a revisar la siguiente
    N1 is N - 1,
    borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N1, SudokuAux, Parada, Parada1),!.

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    borrarGeneral(Sudoku, Index, Pos1, 2, SudokuSin, Parada, Parada2),                           %Llama a predicado que borra la posibilidad del elemento que sea igual a alguna del elemento pasado
    N1 is N - 1,
    borrarParejaFila(SudokuSin, Pos1, Pos2, Fila, N1, SudokuAux, Parada2, Parada1),!.

borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N, SudokuAux, Parada, Parada1):-
    N1 is N - 1,
    borrarParejaFila(Sudoku, Pos1, Pos2, Fila, N1, SudokuAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Regla 3 -> Si en tres lugares de una fila, columna o cuadro solo aparecen tres numeros distintos, borramos los numeros de las restantes listas de la fila, columna o cuadro.
%-------------------------------------------------------------------------------
regla3(Sudoku, Resultado, Parada, Parada1):-
    regla3Aux(Sudoku, 81, Resultado, Parada, Parada1).                                           %Recorre sudoku

regla3Aux(Final, 0, Final, Cond, Cond).                                                     %Termina recorrido

regla3Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    number(X),                                                                  %Comprueba si el elemento actual es un numero y si lo es pasa a revisar siguiente posicion
    N1 is N - 1,
    regla3Aux(Sudoku, N1, Resultado, Parada, Parada1),!.

regla3Aux(Sudoku, N, Resultado, Parada, Parada1):-
    nth1(N, Sudoku, X),
    length(X, Longitud),
    not(Longitud is 3),                                                         %Si la longitud del elemento actual no es 3 (no cumple la regla) pasamos a revisar siguiente elemento
    N1 is N - 1,
    regla3Aux(Sudoku, N1, Resultado, Parada, Parada1),!.

%Si elemento actual no es un n�mero y su longitud es 3(cumple requisitos de la regla) comprobamos si en su fila, columna o cuadrante hay dos elementos mas iguales a el
regla3Aux(Sudoku, N, Resultado, Parada, Parada1):-
    obtener_ejes(N, J1, J2),
    nth1(N, Sudoku, X),
    regla3Fila(Sudoku, N, X, J1, 9, [], ListaAux, Parada, Parada2),
    regla3Columna(ListaAux, N, X, J2, 9, [], ListaAux2, Parada2, Parada3),
    regla3Cuadrante(ListaAux2, N, X, 3, 3, J1, J2, [], ListaAux3, Parada3, Parada4),
    N1 is N - 1,
    regla3Aux(ListaAux3, N1, Resultado, Parada4, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre la fila correspondiente revisando si hay dos elementos m�s igual al pasado
regla3Fila(Final, _, _, _, 0, _, Final, Cond, Cond).

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;                                                              %Si no somos nosotros mismos, el elemento no es un n�mero y su longitud no es 3(no cumple condiciones de la regla) se pasa arevisar siguiente posici�n
    number(Elem);
    (length(Elem, Longitud),
    not(Longitud is 3))),
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count, ListaAux, Parada, Parada1),!.

%Si la longitud del elemento es 3 revisamos si es igual al pasado
regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux, Parada, Parada1):-
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
    Count1 = [Index | Count],                                                   %Guarda posici�n del elemento igual encontrado
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count1, ListaAux, Parada, Parada1),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Index | Count]],
    borrarTrioFila(Sudoku, Count1, Fila, 9, ListaSin, Parada, Parada2),                          %Llama a predicado para borrar las posibilidades de los elementos de la fila que sean iguales a los del elemento encontrado
    regla3Fila(ListaSin, Pos, X, Fila, N1, Count1, ListaAux, Parada2, Parada1),!)
    ).

regla3Fila(Sudoku, Pos, X, Fila, N, Count, ListaAux, Parada, Parada1):-
    N1 is N - 1,
    regla3Fila(Sudoku, Pos, X, Fila, N1, Count, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Recorre la columna correspondiente revisando si hay dos elementos m�s igual al pasado
regla3Columna(Final, _, _, _, 0, _, Final, Cond, Cond).

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    (Pos is Index;
    number(Elem);
    (length(Elem, Longitud),                                                    %Si no somos nosotros mismos, el elemento no es un n�mero y su longitud no es 3(no cumple condiciones de la regla) se pasa arevisar siguiente posici�n
    not(Longitud is 3))),
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count, ListaAux, Parada, Parada1),!.

%Si la longitud del elemento es 3 revisamos si es igual al pasado
regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Sig),
    nth1(1, X, Y),
    member(Y, Sig),
    nth1(2, X, Y2),
    member(Y2, Sig),
    nth1(3, X, Y3),
    member(Y3, Sig),
    (
    (length(Count, 0),                                                          %Guarda posici�n del elemento igual encontrado(igual al pasado)
    Count1 = [Index | Count],
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count1, ListaAux, Parada, Parada1),!)
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Index | Count]],
    borrarTrioColumna(Sudoku, Count1, Columna, 9, ListaSin, Parada, Parada2),                    %Llama a predicado para borrar las posibilidades de los elementos de la columna que sean iguales a los del elemento encontrado
    regla3Columna(ListaSin, Pos, X, Columna, N1, Count1, ListaAux, Parada2, Parada1),!)
    ).

regla3Columna(Sudoku, Pos, X, Columna, N, Count, ListaAux, Parada, Parada1):-
    N1 is N - 1,
    regla3Columna(Sudoku, Pos, X, Columna, N1, Count, ListaAux, Parada, Parada1),!.

%-------------------------------------------------------------------------------
%Recorre el cuadrante correspondiente revisando si hay dos elementos m�s igual al pasado
regla3Cuadrante(Final, _, _, _, 0, _, _, _, Final, Cond, Cond).

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux, Parada, Parada1):-
    N is 0,
    M1 is M - 1,
    regla3Cuadrante(Sudoku, Pos, X, 3, M1, Fila, Columna, Count, ListaAux, Parada, Parada1),!.

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),                                               %Si no somos nosotros mismos, el elemento no es un n�mero y su longitud no es 3(no cumple condiciones de la regla) se pasa arevisar siguiente posici�n
    (Pos is Posicion;
    number(Elem);
    (not(number(Elem)),
    length(Elem, Longitud),
    not(Longitud is 3) )),
    N >= 1,
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count, ListaAux, Parada, Parada1),!.

%Si la longitud del elemento es 3 revisamos si es igual al pasado
regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux, Parada, Parada1):-
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
    (length(Count, 0),                                                          %Guarda posici�n del elemento igual encontrado(igual al pasado)
    Count1 = [Posicion | Count],
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count1, ListaAux, Parada, Parada1),!)  %Llama a predicado para borrar las posibilidades de los elementos del cuadrante que sean iguales a los del elemento encontrado
    ;
    (N1 is N - 1,
    Count1 = [Pos | [Posicion | Count]],
    borrarTrioCuadrante(Sudoku, Count1, Fila, Columna, 3, 3, ListaSin, Parada, Parada2),
    regla3Cuadrante(ListaSin, Pos, X, N1, M, Fila, Columna, Count1, ListaAux, Parada2, Parada1),!)
    ).

regla3Cuadrante(Sudoku, Pos, X, N, M, Fila, Columna, Count, ListaAux, Parada, Parada1):-
    N1 is N - 1,
    regla3Cuadrante(Sudoku, Pos, X, N1, M, Fila, Columna, Count, ListaAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Borra los elementos de la fila que tengas posibilidades iguales al del elemento encontrado
borrarTrioFila(Final, _, _, 0, Final, Cond, Cond).

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(Index, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count , Pos3),
    (Pos1 is Index;                                                             %Si la posici�n actual es igual de una de las tres encontradas o un numero ya establecido se pasa a revisar la siguiente
    Pos2 is Index;
    Pos3 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarTrioFila(Sudoku, Count, Fila, N1, SudokuAux, Parada, Parada1),!.

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux, Parada, Parada1):-
    Index is (Fila - 1) * 9 + N,
    nth1(1, Count, Pos1),
    borrarGeneral(Sudoku, Index, Pos1, 3, SudokuSin, Parada, Parada2),                           %Comprueba si alguna posibilidad es igual a alguna de los del elemento pasado y la borra si lo es
    N1 is N - 1,
    borrarTrioFila(SudokuSin, Count, Fila, N1, SudokuAux, Parada2, Parada1),!.

borrarTrioFila(Sudoku, Count, Fila, N, SudokuAux, Parada, Parada1):-
    N1 is N - 1,
    borrarTrioFila(Sudoku, Count, Fila, N1, SudokuAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Borra los elementos de la columna que tengas posibilidades iguales al del elemento encontrado
borrarTrioColumna(Final, _, _, 0, Final, Cond, Cond).

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(Index, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count , Pos3),
    (Pos1 is Index;                                                             %Si la posici�n actual es igual de una de las tres encontradas o un numero ya establecido se pasa a revisar la siguiente
    Pos2 is Index;
    Pos3 is Index;
    number(Elem)),
    N1 is N - 1,
    borrarTrioColumna(Sudoku, Count, Columna, N1, SudokuAux, Parada, Parada1),!.

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux, Parada, Parada1):-
    Index is (Columna) + (N - 1) * 9,
    nth1(1, Count, Pos1),                                                       %Comprueba si alguna posibilidad es igual a alguna de los del elemento pasado y la borra si lo es
    borrarGeneral(Sudoku, Index, Pos1, 3, SudokuSin, Parada, Parada2),
    N1 is N - 1,
    borrarTrioColumna(SudokuSin, Count, Columna, N1, SudokuAux, Parada2, Parada1),!.

borrarTrioColumna(Sudoku, Count, Columna, N, SudokuAux, Parada, Parada1):-
    N1 is N - 1,
    borrarTrioColumna(Sudoku, Count, Columna, N1, SudokuAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Borra los elementos delcuadrante que tengas posibilidades iguales al del elemento encontrado
borrarTrioCuadrante(Final, _, _, _, _, 0, Final, Cond, Cond).

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    N is 0,
    M1 is M - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, 3, M1, SudokuAux, Parada, Parada1),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(Posicion, Sudoku, Elem),
    nth1(1, Count, Pos1),
    nth1(2, Count, Pos2),
    nth1(3, Count, Pos3),
    (Pos1 is Posicion;
    Pos2 is Posicion;                                                           %Si la posici�n actual es igual de una de las tres encontradas o un numero ya establecido se pasa a revisar la siguiente
    Pos3 is Posicion;
    number(Elem)),
    N1 is N - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N1, M, SudokuAux, Parada, Parada1),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    InicioFila is ((Fila - 1) // 3 * 3) + N - 1,
    InicioColumna is ((Columna - 1) // 3 * 3) + M,
    Posicion is InicioFila * 9 + InicioColumna,
    nth1(1, Count, Pos1),                                                       %Comprueba si alguna posibilidad es igual a alguna de los del elemento pasado y la borra si lo es
    borrarGeneral(Sudoku, Posicion, Pos1, 3, SudokuSin, Parada, Parada2),
    N1 is N - 1,
    borrarTrioCuadrante(SudokuSin, Count, Fila, Columna, N1, M, SudokuAux, Parada2, Parada1),!.

borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N, M, SudokuAux, Parada, Parada1):-
    N1 is N - 1,
    borrarTrioCuadrante(Sudoku, Count, Fila, Columna, N1, M, SudokuAux, Parada, Parada1),!.
%-------------------------------------------------------------------------------
%Comprueba si el elemento actual tiene alguna posibilidad igual al del elemento que cumple la regla y si lo hay lo elimina sustituyendo elemento
borrarGeneral(Final, _, _, 0, Final, Cond, Cond).

borrarGeneral(Sudoku, Index, Pos1, N, SudokuAux, Parada, Parada1):-
    nth1(Index, Sudoku, Elem),
    nth1(Pos1, Sudoku, Pos),
    nth1(N, Pos, X),
    (
    (member(X, Elem),                                                           %Si son iguales lo elimina
    select(X, Elem, Borrada),
    sustituir_elemento(Sudoku, Index, Borrada, SudokuSin),
    N1 is N - 1,
    borrarGeneral(SudokuSin, Index, Pos1, N1, SudokuAux, 0, Parada1),!)
    ;
    (N1 is N - 1,
    borrarGeneral(Sudoku, Index, Pos1, N1, SudokuAux, Parada, Parada1),!)
    ).
%-------------------------------------------------------------------------------
%Predicados que llama a buscar_posibilidades y a las cuatro reglas implementadas hasta no poder aplicarlas m�s o resolver eel sudoku, mostrandolo por pantalla
%-------------------------------------------------------------------------------
simplificar_sudoku(Sudoku, Resultado):-
    mostrar_sudoku(Sudoku),
    buscar_posibilidades(Sudoku, SudokuAux),                                    %Busca posibilidades del sudoku
    mostrar_sudoku(SudokuAux),
    buscar_casos(SudokuAux, 0, SudokuAux2),                                     %Aplica las 4 reglas hasta que no se pueda mas
    Resultado = SudokuAux2,
    dibujar_sudoku(SudokuAux2).
%-------------------------------------------------------------------------------
%Llama a las cuatro reglas de simplificaci�n hasta que no se pueda aplicar ninguna de ellas (el contador sea 1)
buscar_casos(Final, 1, Final).

buscar_casos(Sudoku, _, Resultado):-
    Parada is 1,                                                                %Condici�n de parada se inicializa a 1
    regla0(Sudoku, ListaAux, Parada, Parada1),
    regla1(ListaAux, ListaAux2, Parada1, Parada2),
    regla2(ListaAux2, ListaAux3, Parada2, Parada3),
    regla3(ListaAux3, ListaAux4, Parada3, Parada4),
    (                                                                           %Si contador es 1 tras aplicar las 4 reglas significa que ninguna se a cumplido por lo que se termina la ejecuci�n
    (Parada4 is 1,
    buscar_casos(ListaAux4, Parada4, Resultado),!)
    ;
    (buscar_casos(ListaAux4, 0, Resultado),!)                                   %Si al menos una regla se ha cumplido se hace una nueva iteraci�n
    ).
%-------------------------------------------------------------------------------

