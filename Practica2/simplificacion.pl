%Simplificacion
%-------------------------------------------------------------------------------
palabras(['which', 'that', 'whose', 'who', 'whom', 'those', 'this', 'what']).

obtener_oraciones():-
    X = ocm(o(gn(det('The'), gn(n(man), gn(n(we)))), gv(v(saw), gn(n(yesterday)))), o(gv(v(was), gn(det(my), n(neighbor))))),
    ocm(o(G_NOMINAL , _), o(FREE, _)) = X,
    write(FREE), nl.




