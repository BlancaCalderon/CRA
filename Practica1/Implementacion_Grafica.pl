:- use_module(library(tabular)).
:- use_module(library(autowin)).

%Predicado que se encarga de la implementación gráfica
dibujar_sudoku(Sudoku):-
   new(Ventana, auto_sized_picture('Sudoku')),
   send(Ventana, display, new(Tabla, tabular)),
   send(Tabla, border, 1),
   send(Tabla, cell_spacing, -1),
   send(Tabla, cell_padding, 10),
   send(Tabla, rules, all),
   recorrer_sudoku_visual(Sudoku,LR),
   cadenas_a_terminos(LR, LRFormateado),
   send_list(Tabla, LRFormateado),
   send(Ventana, open).

%Predicado que se encarga de iniciar el formateo del sudoku a formato de lista para interfaz
recorrer_sudoku_visual(Sudoku, ListaResultado) :-
   recorrer_sudoku_visual_aux(Sudoku, 0, [], ListaAcumulada),
   reverse(ListaAcumulada, ListaResultado).


%Predicado auxliar que se encarga de iniciar el formateo del sudoku a formato de lista para interfaz
recorrer_sudoku_visual_aux(Sudoku, Contador, ListaAcumulada, ListaResultado) :-
   (Contador < 81 ->
      nth0(Contador, Sudoku, Elemento),
      (is_list(Elemento) ->
         lista_a_numero(Elemento, ElementoKK)
      ;
         ElementoKK = Elemento
      ),
      ContadorMod is Contador mod 9,
      (ContadorMod == 0, Contador \= 0 ->
         agregar_elemento_lista('next_row', ListaAcumulada, ListaAcumuladaAux)
         ;
         ListaAcumuladaAux = ListaAcumulada
      ),
      string_concat('append(', ElementoKK, ElementoAux),
      string_concat(ElementoAux, ')', ElementoAux2),
      agregar_elemento_lista(ElementoAux2, ListaAcumuladaAux, ListaAcumuladaAux3),
      ContadorAux is Contador+1,
      recorrer_sudoku_visual_aux(Sudoku, ContadorAux, ListaAcumuladaAux3, ListaResultado)
      ;
      ListaResultado = ListaAcumulada
   ).

%Predicado auxiliar para concatenar listas
agregar_elemento_lista(X, Lista, [X|Lista]).

%Predicado para convertir una cadena en un término sin comillas
cadena_a_termino(Cadena, Termino) :-
   term_to_atom(Termino, Cadena).

%Predicado para convertir una lista de cadenas en una lista de términos sin comillas
cadenas_a_terminos([], []).
cadenas_a_terminos([Cadena|RestoCadenas], [Termino|RestoTerminos]) :-
   cadena_a_termino(Cadena, Termino),
   cadenas_a_terminos(RestoCadenas, RestoTerminos).

%Predicado que transforma una lista en un numero en conjunto
lista_a_numero(Lista, NumeroResultado) :-
   length(Lista, Largo),
   (Largo>1 ->
      lista_a_numero_aux(Lista, NumeroRes),
      atom_number(NumeroRes, NumeroResultado)
      ;
      nth0(0, Lista, Elemento),
      NumeroResultado = Elemento
   ).
   
%Predicado auxiliar que transforma una lista en un numero en conjunto
lista_a_numero_aux([E1, E2|Resto], NumeroResultado) :-
    string_concat(E1, E2, ElementoRes),
    (Resto = [] ->
       NumeroResultado = ElementoRes
       ;
       lista_a_numero_aux([ElementoRes|Resto], NumeroResultado)
    ).
