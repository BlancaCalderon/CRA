%Simplificacion
%-------------------------------------------------------------------------------
obtener(Oracion, L):-
   obtener_oracion(Oracion, 1, [], L).
   
obtener_oracion(_, 0, Fin, Fin).
    
obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    (
    o(_, _) = X,
    Y = [X | Lista],
    obtener_oracion(Oracion, 0, Y, Final),!
    ;
    o(Predicado) = X,
    nth1(1, Lista, Elem),
    Z = Elem,
    o(Sujeto, _) = Z,
    Y = [o(Sujeto, Predicado) | Lista],
    obtener_oracion(Oracion, 0, Y, Final),!
    ).
    
obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    oc(Simple, _, Simple2) = X,
    obtener_oracion(Simple, 1, Lista, Lista1),!,
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.
    
obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    or(Simple, _, Simple2) = X,
    obtener_oracion(Simple, 1, Lista, Lista1),!,
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.
    
obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    (
    ocm(Simple, Simple2) = X
    ;
    ocm(Simple, _, Simple2) = X
    ),
    obtener_oracion(Simple, 1, Lista, Lista1),!,
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.

arbol_a_lista(Arbol, Lista) :-
    term_to_atom(Arbol, Atom),
    atomic_list_concat(Atoms, ',', Atom),
    maplist(atom_string, Atoms, Lista).


limpiar_oracion(Oracion):-
    term_to_atom(Oracion, Atom),
    atomic_list_concat(Atoms, ',', Atom),
    maplist(atom_string, Atoms, Lista),
    procesar_elementos(Lista, [], Resultado),
    atomic_list_concat(Resultado, ' ', Cadena),
    write(Cadena).
    
procesar_elementos([ ], Final, Final).

procesar_elementos([Head|Tail], Final, Aux):-
    split_string(Head, "(", "", Lista),
    length(Lista, Tam),
    nth1(Tam, Lista, Elem),
    eliminar_caracter(Elem, ')', Resultado),
    eliminar_caracter(Resultado, '\'', Resultado2),
    append(Final, [Resultado2], ListaFinal),
    procesar_elementos(Tail, ListaFinal, Aux).


eliminar_caracter(Cadena, Caracter, NuevaCadena) :-
    atom_chars(Cadena, ListaCaracteres),
    delete(ListaCaracteres, Caracter, ListaSinCaracter),
    atom_chars(NuevaCadena, ListaSinCaracter).




