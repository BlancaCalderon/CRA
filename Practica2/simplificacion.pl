%Simplificacion
%-------------------------------------------------------------------------------
palabras(['which', 'that', 'whose', 'who', 'whom', 'those', 'this', 'what']).

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


