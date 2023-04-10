%Simplificacion
%-------------------------------------------------------------------------------
palabras(['which', 'that', 'whose', 'who', 'whom', 'those', 'this', 'what']).

%obtener_oraciones():-
 %   String = "ocm(oc(o(gn(np(Jose)),gv(v(eats),gn(n(french fries)))),conjc(but),o(gn(np(Maria)),gv(v(prefers),gn(n(paella))))),conj(although),oc(o(gn(np(Hector)),gv(v(drinks),gn(n(coffee)))),conjc(and),o(gn(np(Irene)),gv(v(reads),gn(det(a),n(novel))))))",
  %  add_quotes_to_string(String, Processed).
  
obtener(Oracion, L):-
   obtener_oracion(Oracion, 1, [], L).
   
   
   
obtener_oracion(_, 0, Fin, Fin).
    
obtener_oracion(Oracion, 1, Lista, Final):-
    write('Base'), write(Lista),nl, write(Oracion),nl, write(Final), nl, nl,
    X = Oracion,
    (
    o(_, _) = X,
    write('Simple'), write(X), nl,
    Y = [X | Lista],
    write(Y), nl,
    obtener_oracion(Oracion, 0, Y, Final),!
    ;
    write('si paso por aqui'), nl,
    o(Predicado) = X,
    write('Simple sin sujeto'),
    write(Lista), nl,
    length(Lista, Tam),
    nth1(Tam, Lista, Elem),
    Z = Elem,
    o(Sujeto, _) = Z,
    Y = [o(Sujeto, Predicado) | Lista],
    obtener_oracion(Oracion, 0, Y, Final),!
    ).
    
obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    oc(Simple, _, Simple2) = X,
    write('Oracion coordinada'), nl,
    obtener_oracion(Simple, 1, Lista, Lista1),!,
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.
    
obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    or(Simple, _, Simple2) = X,
    write('Oracion subordinada'), nl,
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



 %Palabra == 'hola' ->
  %      write('¡Hola! Has acertado.')
   %     ;
    %    write('No has acertado.').

