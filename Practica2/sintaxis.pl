%_______________________________________________________________________________
%_______________________________________________________________________________
%                              Gramatica
%_______________________________________________________________________________
%_______________________________________________________________________________

%Oraciones generales
%-------------------------------------------------------------------------------
%oracion(O) --> oracion_simple(O).
oracion(O) --> oracion_coordinada(O).
%oracion(O) --> oracion_compuesta(O).

%-------------------------------------------------------------------------------
%Definición de las oraciones simples
%-------------------------------------------------------------------------------
oracion_simple(o(GN, GV)) --> g_nominal(GN, _, _), g_verbal(GV, _, _).                                                          %Generalmente formadas por un grupo nomial seguido de un verbal(sujeto y predicado)
%oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV), g_nominal(GN), signo(Sig).                                         %Se incluye que puedan terminar con un signo para tener en cuenta oraciones interrogativas y exclamativas
%oracion_simple(o(GV)) --> g_verbal(GV).                                                                             %Tambien pueden estar formadas solo por el grupo verbal(oraciones con sujeto omitido)

%-------------------------------------------------------------------------------
%Definición de las oraciones coordinadas(Dos oraciones simples separadas por una conjuncion)
%-------------------------------------------------------------------------------
oracion_coordinada(oc(O1, Conj, O2)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2).

%-------------------------------------------------------------------------------
%Definición de las oraciones subordinadas (pueden empeza con un nexo)
%-------------------------------------------------------------------------------
oracion_subordinada(or(GN, GV)) --> g_nominal(GN, _, _), g_verbal(GV).
oracion_subordinada(or(NX, GV)) --> nexo(NX), g_verbal(GV).
oracion_subordinada(or(NX, GN, GV)) --> nexo(NX), g_nominal(GN, _, _), g_verbal(GV).

%-------------------------------------------------------------------------------
%Definición de las oraciones compuestas (union de varias oraciones coordinadas, de dos o mas simples o de coordinadas y simples)
%-------------------------------------------------------------------------------
oracion_compuesta(ocm(OC, Conj, OS)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OC1, Conj, OC2)) --> oracion_coordinada(OC1), conjuncion(Conj), oracion_coordinada(OC2).
oracion_compuesta(ocm(OC, Conj, OCM)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_compuesta(OCM).
oracion_compuesta(ocm(OS, Conj, OC)) --> oracion_simple(OS), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, Conj, OCM)) --> oracion_simple(OS), conjuncion(Conj), oracion_compuesta(OCM).

oracion_compuesta(ocm(OS, OC)) --> oracion_simple(OS), oracion_coordinada(OC).
oracion_compuesta(ocm(OC1, OC2)) --> oracion_coordinada(OC1), oracion_coordinada(OC2).
oracion_compuesta(ocm(OC, OS)) --> oracion_coordinada(OC), oracion_simple(OS).

oracion_compuesta(ocm(OS, OCM)) --> oracion_simple(OS), oracion_compuesta(OCM).
oracion_compuesta(ocm(OC, OCM)) --> oracion_coordinada(OC), oracion_compuesta(OCM).

%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Definición de los grupos sintácticos
%_______________________________________________________________________________
%_______________________________________________________________________________

%-------------------------------------------------------------------------------
%Definición del grupo nominal(nombre, nombre propio o determinante seguido de un nombre)
%-------------------------------------------------------------------------------
g_nominal(gn(N), Gen, Num) --> nombre(N, Gen, Num).
%g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).
g_nominal(gn(NP), Gen, Num) --> nombre_propio(NP, Gen, Num).
g_nominal(gn(NP, OR), Gen, Num) --> nombre_propio(NP, Gen, Num), oracion_subordinada(OR).                                               %Dentro de un grupo nominal puede haber una oracion subordinada
%g_nominal(gn(NP, GN)) --> nombre_propio(NP), g_nominal(GN).
%g_nominal(gn(GAdv, N)) --> g_adverbial(GAdv), nombre(N).                                                            %Un grupo nominal puede ser acompañado por un adverbio
%g_nominal(gn(GAdv, NP)) --> g_adverbial(GAdv), nombre_propio(NP).
g_nominal(gn(Det, N), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num).
g_nominal(gn(Det, N, OR), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num), oracion_subordinada(OR).
%g_nominal(gn(Det, N, GN)) --> determinante(Det), nombre(N), g_nominal(GN).
g_nominal(gn(Det, GAdj, N), Gen, Num) --> determinante(Det, Gen, Num), g_adjetival(GAdj, Gen, Num), nombre(N, Gen, Num).                                    %Un grupo nominal puede ser acompañado por un adjetivo
%g_nominal(gn(Det, GAdv, N)) --> determinante(Det), g_adverbial(GAdv), nombre(N).
g_nominal(gn(Det, GAdv, GN), Gen, Num) --> determinante(Det, Gen, Num), g_adverbial(GAdv), g_nominal(GN, Gen, Num).
g_nominal(gn(GAdj, N), Gen, Num) --> g_adjetival(GAdj, Gen, Num), nombre(N, Gen, Num).
%g_nominal(gn(GAdj, NP)) --> g_adjetival(GAdj), nombre_propio(NP).
g_nominal(gn(GAdv, GAdj, N), Gen, Num) --> g_adverbial(GAdv), g_adjetival(GAdj, Gen, Num), nombre(N, Gen, Num).
%g_nominal(gn(GAdv, GAdj, NP)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP).
%g_nominal(gn(GPrep, GN)) --> g_preposicional(GPrep), g_nominal(GN).

%g_nominal(gn(N, Conj, GN)) --> nombre(N), conjuncion(Conj), g_nominal(GN).                                           %Puede haber doble sujeto (separado por y)
g_nominal(gn(NP, Conj, GN), Gen, Num) --> nombre_propio(NP, Gen, Num), conjuncion(Conj), g_nominal(GN, _, _).
%g_nominal(gn(GAdv, N, Conj, GN)) --> g_adverbial(GAdv), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(GAdv, NP, Conj, GN)) --> g_adverbial(GAdv), nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(Det, N, Conj, GN)) --> determinante(Det), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(Det, GAdj, N, Conj, GN)) --> determinante(Det), g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(Det, GAdv, N, Conj, GN)) --> determinante(Det), g_adverbial(GAdv), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(Det, GAdv, GAdj, N, Conj, GN)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(GAdj, N, Conj, GN)) --> g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(GAdj, NP, Conj, GN)) --> g_adjetival(GAdj), nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(GAdv, GAdj, N, Conj, GN)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(GAdv, GAdj, NP, Conj, GN)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
%g_nominal(gn(GPrep, GN1, Conj, GN2)) --> g_preposicional(GPrep), g_nominal(GN1), conjuncion(Conj), g_nominal(GN2).

%-------------------------------------------------------------------------------
%Definición del grupo verbal(verbo)
%-------------------------------------------------------------------------------
g_verbal(gv(V), Gen, Num) --> verbo(V, Gen, Num).
g_verbal(gv(V, GN), Gen, Num) --> verbo(V, Gen, Num), g_nominal(GN, _, _).                                                                     %Un verbo puede estar acompañado por un grupo adjetival
g_verbal(gv(V, GAdj), Gen, Num) --> verbo(V, Gen, Num), g_adjetival(GAdj, _, _).                                                               %Un verbo puede estar acompañado por un grupo adverbial
%g_verbal(gv(V, GAdv)) --> verbo(V), g_adverbial(GAdv).                                                               %Un verbo puede estar acompañado por un grupo preposicional
g_verbal(gv(V, GPrep), Gen, Num) --> verbo(V, Gen, Num), g_preposicional(GPrep).
g_verbal(gv(GAdv, GV), Gen, Num) --> g_adverbial(GAdv), g_verbal(GV, Gen, Num).
%g_verbal(gv(GPrep, GV)) --> g_preposicional(GPrep), g_verbal(GV).
%g_verbal(gv(V, GAdj, GPrep)) --> verbo(V), g_adjetival(GAdj), g_preposicional(GPrep).
g_verbal(gv(V, GAdv, GAdj), Gen, Num) --> verbo(V, Gen, Num), g_adverbial(GAdv), (GAdj, _, _).
%g_verbal(gv(V, GV, GPrep)) --> verbo(V), g_verbal(GV), g_preposicional(GPrep).
%g_verbal(gv(V, GN, GPrep)) --> verbo(V), g_nominal(GN), g_preposicional(GPrep).
%g_verbal(gv(GAdv, GV, GPrep)) --> g_adverbial(GAdv), g_verbal(GV), g_preposicional(GPrep).
%g_verbal(gv(GPrep1, GV, GPrep2)) --> g_preposicional(GPrep1), g_verbal(GV), g_preposicional(GPrep2).

%-------------------------------------------------------------------------------
%Definición del grupo ajetival (un solo adjetivo o varios separados por una conjuncion)
%-------------------------------------------------------------------------------
g_adjetival(gadj(Adj), Gen, Num) --> adjetivo(Adj, Gen, Num).
g_adjetival(gadj(Adj, Conj, GAdj), Gen, Num) --> adjetivo(Adj, Gen, Num), conjuncion(Conj), g_adjetival(GAdj, Gen, Num).

%-------------------------------------------------------------------------------
%Definición del grupo adverbial (un solo adverbio o varios)
%-------------------------------------------------------------------------------
g_adverbial(gadv(Adv)) --> adverbio(Adv).
g_adverbial(gadv(Adv, GAdv)) --> adverbio(Adv), g_adverbial(GAdv).

%-------------------------------------------------------------------------------
%Definición del grupo preposicional (preposicion acompaña a un grupo nomial)
%-------------------------------------------------------------------------------
g_preposicional(gp(Prep, GN)) -->  preposicion(Prep), g_nominal(GN, _, _).
g_preposicional(gp(Prep, GN, GPrep)) -->  preposicion(Prep), g_nominal(GN, _, _), g_preposicional(GPrep).

%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Elementos de la gramatica
%_______________________________________________________________________________
%_______________________________________________________________________________

determinante(det(X), Gen, Num) --> [X], {det(X, Gen, Num)}.
nombre(n(X), Gen, Num) --> [X], {n(X, Gen, Num)}.
nombre_propio(np(X), Gen, Num) --> [X], {np(X, Gen, Num)}.
verbo(v(X), Gen, Num) --> [X], {v(X, Gen, Num)}.
adjetivo(adj(X), Gen, Num) --> [X], {adj(X, Gen, Num)}.
pronombre(pron(X)) --> [X], {pron(X)}.

adverbio(adv(X)) --> [X], {adv(X)}.
conjuncion(conj(X)) --> [X], {conj(X)}.
preposicion(prep(X)) --> [X], {prep(X)}.
signo(sig(X)) --> [X], {sig(X)}.
nexo(nx(X)) --> [X], {nx(X)}.                                                                    %Nexo se utiliza en las oraciones subordinadas
%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Diccionario
%_______________________________________________________________________________
%_______________________________________________________________________________

%-------------------------------------------------------------------------------
%Determinantes
%-------------------------------------------------------------------------------
det('a', _, _).
det('an', _, _).
det('the', _, _).
det('The', _, _).
det('my', _, _).
det('your', _, _).
det('that', _, _).
det('this', _, _).
det('these', _, _).
det('those', _, _).
det('his', _, _).
det('her', _, _).
det('its', _, _).
det('ours', _, _).
det('their', _, _).
det('some', _, _).
det('any', _, _).
det('no', _, _).
det('every', _, _).
det('each', _, _).
det('either', _, _).
det('neither', _, _).
det('few', _, _).
det('many', _, _).
det('several', _, _).
det('all', _, _).
det('much', _, _).
det('a lot', _, _).
det('plenty of', _, _).
det('one', _, _).
det('two', _, _).
det('three', _, _).
det('four', _, _).
det('five', _, _).
det('six', _, _).
det('seven', _, _).
det('eight', _, _).
det('nine', _, _).
det('ten', _, _).
det('first', _, _).
det('second', _, _).
det('third', _, _).

%-------------------------------------------------------------------------------
%Nombres
%-------------------------------------------------------------------------------
n('law', _, _).
n('Law', _, _).
n('philosophy', _, _).
n('Philosophy', _, _).
n('man', _, _).
n('woman', _, _).
n('coffee', _, _).
n('newspaper', _, _).
n('table', _, _).
n('french fries', _, _).
n('novel', _, _).
n('paella', _, _).
n('beer', _, _).
n('juice', _, _).
n('wall', _, _).
n('apples', _, _).
n('word processor', _, _).
n('word', _, _).
n('processor', _, _).
n('documents', _, _).
n('mouse', _, _).
n('cat', _, _).
n('neighbor', _, _).
n('evenings', _, _).
n('tool', _, _).
n('it', _, _).
n('yesterday', _, _).
n('we', _, _).

%-------------------------------------------------------------------------------
%Nombres propios
%-------------------------------------------------------------------------------
np('José', _, _).
np('María', _, _).
np('Jose', _, _).
np('Maria', _, _).
np('Hector', _, _).
np('Irene', _, _).

%-------------------------------------------------------------------------------
%Verbos
%-------------------------------------------------------------------------------
v('is', _, _).
v('is having', _, _).
v('studies', _, _).
v('having', _, _).
v('clears', _, _).
v('drinks', _, _).
v('reads', _, _).
v('eat', _, _).
v('drink', _, _).
v('eats', _, _).
v('prefers', _, _).
v('sings', _, _).
v('jumps', _, _).
v('used', _, _).
v('writing', _, _).
v('caught', _, _).
v('was', _, _).
v('saw', _, _).
v('climbs', _, _).
v('is used', _, _).

%-------------------------------------------------------------------------------
%Adjetivos
%-------------------------------------------------------------------------------
adj('dark-haired', _, _).
adj('tall', _, _).
adj('french', _, _).
adj('agile', _, _).
adj('climbing', _, _).
adj('picky', _, _).
adj('red', _, _).
adj('powerful', _, _).
adj('slow', _, _).
adj('gray', _, _).
adj('writing', _, _).

%-------------------------------------------------------------------------------
%Adverbios
%-------------------------------------------------------------------------------
adv('quite').
adv('always').
adv('never').
adv('now').
adv('then').
adv('sometimes').
adv('hard').
adv('slowly').
adv('well').
adv('easily').
adv('quickly').
adv('badly').
adv('early').
adv('late').
adv('happily').
adv('softly').
adv('very').
adv('only').

%-------------------------------------------------------------------------------
%Conjunciones
%-------------------------------------------------------------------------------
conj('and').
conj(',').
conj('but').
conj('or').
conj('so').
conj('yet').
conj('nor').
conj('after').
conj('although').
conj('as').
conj('as if').
conj('because').
conj('before').
conj('even though').
conj('if').
conj('in order that').
conj('once').
conj('provided that').
conj('rather than').
conj('than').
conj('though').
conj('when').
conj('where').
conj('wherever').
conj('while').
conj('that').

%-------------------------------------------------------------------------------
%Preposiciones
%-------------------------------------------------------------------------------
prep('above').
prep('about').
prep('across').
prep('after').
prep('against').
prep('along').
prep('among').
prep('around').
prep('at').
prep('before').
prep('behind').
prep('below').
prep('beneath').
prep('besida').
prep('between').
prep('beyond').
prep('but').
prep('by').
prep('despite').
prep('except').
prep('for').
prep('from').
prep('in').
prep('inside').
prep('into').
prep('of').
prep('outside').
prep('to').
prep('under').
prep('until').
prep('with').
prep('toward').
prep('without').

%-------------------------------------------------------------------------------
%Nexos de las oraciones subordinadas
%-------------------------------------------------------------------------------
nx('that').
nx('which').
nx('who').

%-------------------------------------------------------------------------------
%Signos
%-------------------------------------------------------------------------------
sig('?').
sig('!').

%-------------------------------------------------------------------------------
%Oraciones de prueba
%-------------------------------------------------------------------------------
oracion1(['Jose', 'is', 'dark-haired', 'and', 'Maria', 'is', 'tall']).
oracion2(['Jose', 'studies', 'Philosophy', 'but', 'Maria', 'studies', 'Law']).
oracion3(['Maria', 'is having', 'a', 'coffee', 'while', 'Jose', 'clears', 'the', 'table']).
oracion4(['Jose', 'drinks', 'coffee', 'and', 'reads', 'the', 'newspaper']).
oracion5(['Jose', 'and', 'Hector', 'eat', 'french fries', 'and', 'drink', 'beer']).
oracion6(['Jose', 'eats', 'french fries', 'but', 'Maria', 'prefers', 'paella', 'although', 'Hector', 'drinks', 'coffee', 'and', 'Irene', 'reads', 'a', 'novel']).
oracion7(['Irene', 'sings', 'and', 'jumps', 'while', 'Jose', 'studies']).
oracion8(['Hector', 'eats', 'french fries', 'and', 'drinks', 'juice', 'while', 'Jose', 'sings', 'and', 'jumps', 'although', 'Maria', 'reads', 'a', 'novel']).
oracion9(['Jose', 'who', 'is', 'agile', 'climbs', 'at', 'the', 'climbing', 'wall', 'in', 'the', 'evenings']).
oracion10(['Jose', 'who', 'is', 'very', 'picky', 'only', 'eats', 'red', 'apples']).
oracion11(['The', 'word processor', 'which', 'is', 'a', 'quite', 'powerful', 'tool', 'is used', 'for', 'writing', 'documents']).
oracion12(['The', 'word processor', 'is', 'a', 'very', 'powerful', 'tool', 'that', 'is used', 'for', 'writing', 'documents', 'but', 'it', 'is', 'quite', 'slow']).
oracion13(['The', 'mouse', 'that', 'the', 'cat', 'caught', 'was', 'gray']).
oracion14(['The', 'man', 'we', 'saw', 'yesterday', 'was', 'my', 'neighbor']).

%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Simplificacion en oraciones simples
%_______________________________________________________________________________
%_______________________________________________________________________________

%-------------------------------------------------------------------------------
%Dado el análisis de las oraciones obtener las oraciones simples que componen oracion analizada
%-------------------------------------------------------------------------------
obtener(Oracion, L):-
   obtener_oracion(Oracion, 1, [], LSucia),
   limpiar_oraciones(LSucia, [], L).

obtener_oracion(_, 0, Fin, Fin).

obtener_oracion(Oracion, 1, Lista, Final):-
    X = Oracion,
    (
    o(Sujeto, Predicado) = X,
    separar_subordinada(Sujeto, R),
    nth1(2, R, Nexo),

    (Nexo = [] ->
    (Y = [X | Lista],
    obtener_oracion(Oracion, 0, Y, Final),!)
    ;
    (
    nth1(1, R, Suj),
    Arbol = o(Suj, Predicado),
    obtener_oracion(Arbol, 1, Lista, Lista1),!,
    obtener_oracion(Nexo, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!))
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
    (ocm(Simple, Simple2) = X
    ;
    ocm(Simple, _, Simple2) = X),
    obtener_oracion(Simple, 1, Lista, Lista1),!,
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.

separar_subordinada(Lista, Salida):-
    Lista =.. [_ | Argumentos],
    separar(Argumentos, [], [], ListaDoble),
    Salida = ListaDoble.


separar([Head | Tail], Aux, Subordinada, Final):-
    Head =.. [Etiqueta | _],
    (Etiqueta = or ->
    separar(Tail, Aux, Head, Final);
    separar(Tail, [Head | Aux], Subordinada, Final)
    ).

separar([ ], Aux, Subordinada, Final):-
    reverse(Aux, Aux2),
    Sujeto =.. [gn | Aux2],
    Subordinada =.. [_ | Args],
    anadir(Args, Sujeto, [], Salida),
    Final = [Sujeto, Salida].

anadir([Head | Tail], Sujeto, Aux, Final):-
    Head =.. [Etiqueta | _],
    (Etiqueta = nx ->
    anadir(Tail, Sujeto, Aux, Final);
    anadir(Tail, Sujeto, [Head | Aux], Final)
    ).

anadir([ ], _, [ ], Final):-
    Final = [].

anadir([ ], Sujeto, Aux, Final):-
    reverse(Aux, Aux2),
    length(Aux2, Tam),
    (Tam = 1 ->
    (quitar_par(Aux2, Aux3),
    Final = o(Sujeto, Aux3));
    Final =.. [o | Aux2]).

quitar_par([Con], Sin):-
    Sin = Con.


%Metodos para limpiar oraciones
%-------------------------------------------------------------------------------
limpiar_oraciones([ ], Final, Final).

limpiar_oraciones([Head | Tail], Final, Aux):-
    limpiar_oracion(Head, Resultado),
    limpiar_oraciones(Tail, [Resultado | Final], Aux).

limpiar_oracion(Oracion, Cadena):-
    term_to_atom(Oracion, Atom),
    atomic_list_concat(Atoms, ',', Atom),
    maplist(atom_string, Atoms, Lista),
    procesar_elementos(Lista, [], Resultado),
    atomic_list_concat(Resultado, ' ', Cad),
    Cadena = Cad.

procesar_elementos([ ], Final, Final).

procesar_elementos([Head|Tail], Final, Aux):-
    split_string(Head, "(", "", Lista),
    length(Lista, Tam),
    nth1(Tam, Lista, Elem),
    eliminar_caracter(Elem, ')', Resultado),
    eliminar_caracter(Resultado, '\'', Resultado2),
    append(Final, [Resultado2], ListaFinal),
    procesar_elementos(Tail, ListaFinal, Aux).

eliminar_caracter(Cadena, Caracter, NuevaCadena):-
    atom_chars(Cadena, ListaCaracteres),
    delete(ListaCaracteres, Caracter, ListaSinCaracter),
    atom_chars(NuevaCadena, ListaSinCaracter).

