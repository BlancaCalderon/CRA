%-------------------------------------------------------------------------------
%Oraciones
%-------------------------------------------------------------------------------
oracion(O) --> oracion_simple(O).
oracion(O) --> oracion_coordinada(O).
oracion(O) --> oracion_subordinada(O).
oracion(O) --> oracion_compuesta(O).

oracion_simple(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).
oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV), g_nominal(GN), signo(Sig).
oracion_aux(o(GV)) --> g_verbal(GV).

oracion_coordinada(oc(O1, CC, O2)) --> oracion_simple(O1), conjuncion_coordinada(CC), oracion_simple(O2).
oracion_coordinada(oc(O1, CC, OA)) --> oracion_simple(O1), conjuncion_coordinada(CC), oracion_aux(OA).

oracion_subordinada(or(O1, CS, O2)) --> oracion_simple(O1), conjuncion_subordinada(CS), oracion_simple(O2).

%Oraciones compuestas
oracion_compuesta(ocm(OS1, Conj, OS2)) --> oracion_simple(OS1), conjuncion(Conj), oracion_simple(OS2).
oracion_compuesta(ocm(OS, Conj, OC)) --> oracion_simple(OS), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, Conj, OR)) --> oracion_simple(OS), conjuncion(Conj), oracion_subordinada(OR).
oracion_compuesta(ocm(OC1, Conj, OC2)) --> oracion_coordinada(OC1), conjuncion(Conj), oracion_coordinada(OC2).
oracion_compuesta(ocm(OC, Conj, OS)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OC, Conj, OR)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_subordinada(OR).
oracion_compuesta(ocm(OR1, Conj, OR2)) --> oracion_subordinada(OR1), conjuncion(Conj), oracion_subordinada(OR2).
oracion_compuesta(ocm(OR, Conj, OS)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OR, Conj, OC)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, Conj, OCM)) --> oracion_simple(OS), conjuncion(Conj), oracion_compuesta(OCM).
oracion_compuesta(ocm(OC, Conj, OCM)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_compuesta(OCM).
oracion_compuesta(ocm(OR, Conj, OCM)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_compuesta(OCM).

oracion_compuesta(ocm(OS1, OS2)) --> oracion_simple(OS1), oracion_simple(OS2).
oracion_compuesta(ocm(OS, OC)) --> oracion_simple(OS), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, OR)) --> oracion_simple(OS), oracion_subordinada(OR).
oracion_compuesta(ocm(OS, OA)) --> oracion_simple(OS), oracion_aux(OA).
oracion_compuesta(ocm(OC1, OC2)) --> oracion_coordinada(OC1), oracion_coordinada(OC2).
oracion_compuesta(ocm(OC, OS)) --> oracion_coordinada(OC), oracion_simple(OS).
oracion_compuesta(ocm(OC, OR)) --> oracion_coordinada(OC), oracion_subordinada(OR).
oracion_compuesta(ocm(OC, OA)) --> oracion_coordinada(OC), oracion_aux(OA).
oracion_compuesta(ocm(OR1, OR2)) --> oracion_subordinada(OR1), oracion_subordinada(OR2).
oracion_compuesta(ocm(OR, OS)) --> oracion_subordinada(OR), oracion_simple(OS).
oracion_compuesta(ocm(OR, OC)) --> oracion_subordinada(OR), oracion_coordinada(OC).
oracion_compuesta(ocm(OR, OA)) --> oracion_subordinada(OR), oracion_aux(OA).

oracion_compuesta(ocm(OS, OCM)) --> oracion_simple(OS), oracion_compuesta(OCM).
oracion_compuesta(ocm(OC, OCM)) --> oracion_coordinada(OC), oracion_compuesta(OCM).
oracion_compuesta(ocm(OR, OCM)) --> oracion_subordinada(OR), oracion_compuesta(OCM).
oracion_compuesta(ocm(OR, OA)) --> oracion_subordinada(OR), oracion_aux(OA).

%-------------------------------------------------------------------------------
%Grupos
%-------------------------------------------------------------------------------
%Grupo nominal
g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).
g_nominal(gn(NP)) --> nombre_propio(NP).
g_nominal(gn(NP, GN)) --> nombre_propio(NP), g_nominal(GN).
g_nominal(gn(GAdv, N)) --> g_adverbial(GAdv), nombre(N).
g_nominal(gn(GAdv, NP)) --> g_adverbial(GAdv), nombre_propio(NP).
g_nominal(gn(Det, N)) --> determinante(Det), nombre(N).
g_nominal(gn(Det, N, GN)) --> determinante(Det), nombre(N), g_nominal(GN).
g_nominal(gn(Det, GAdj, N)) --> determinante(Det), g_adjetival(GAdj), nombre(N).
g_nominal(gn(Det, GAdv, N)) --> determinante(Det), g_adverbial(GAdv), nombre(N).
g_nominal(gn(Det, GAdv, GAdj, N)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), nombre(N).
g_nominal(gn(Det, GAdj, GN)) --> determinante(Det), g_adjetival(GAdj), g_nominal(GN).
g_nominal(gn(Det, GAdv, GN)) --> determinante(Det), g_adverbial(GAdv), g_nominal(GN).
g_nominal(gn(Det, GAdv, GAdj, GN)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), g_nominal(GN).
g_nominal(gn(GAdj, N)) --> g_adjetival(GAdj), nombre(N).
g_nominal(gn(GAdj, NP)) --> g_adjetival(GAdj), nombre_propio(NP).
g_nominal(gn(GAdv, GAdj, N)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre(N).
g_nominal(gn(GAdv, GAdj, NP)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP).
g_nominal(gn(GPrep, GN)) --> g_preposicional(GPrep), g_nominal(GN).

g_nominal(gn(N, Conj, GN)) --> nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(NP, Conj, GN)) --> nombre_propio(NP), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GAdv, N, Conj, GN)) --> g_adverbial(GAdv), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GAdv, NP, Conj, GN)) --> g_adverbial(GAdv), nombre_propio(NP), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(Det, N, Conj, GN)) --> determinante(Det), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(Det, GAdj, N, Conj, GN)) --> determinante(Det), g_adjetival(GAdj), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(Det, GAdv, N, Conj, GN)) --> determinante(Det), g_adverbial(GAdv), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(Det, GAdv, GAdj, N, Conj, GN)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GAdj, N, Conj, GN)) --> g_adjetival(GAdj), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GAdj, NP, Conj, GN)) --> g_adjetival(GAdj), nombre_propio(NP), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GAdv, GAdj, N, Conj, GN)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre(N), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GAdv, GAdj, NP, Conj, GN)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GPrep, GN1, Conj, GN2)) --> g_preposicional(GPrep), g_nominal(GN1), conjuncion_coordinada(Conj), g_nominal(GN2).

%Grupo verbal
g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V, GV)) --> verbo(V), g_verbal(GV).
g_verbal(gv(V, GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V, GAdj)) --> verbo(V), g_adjetival(GAdj).
g_verbal(gv(V, GAdv)) --> verbo(V), g_adverbial(GAdv).
g_verbal(gv(V, GPrep)) --> verbo(V), g_preposicional(GPrep).
g_verbal(gv(GAdv, GV)) --> g_adverbial(GAdv), g_verbal(GV).
g_verbal(gv(GPrep, GV)) --> g_preposicional(GPrep), g_verbal(GV).
g_verbal(gv(V, GAdj, GPrep)) --> verbo(V), g_adjetival(GAdj), g_preposicional(GPrep).
g_verbal(gv(V, GAdv, GAdj)) --> verbo(V), g_adverbial(GAdv), g_adjetival(GAdj).
g_verbal(gv(V, GV, GPrep)) --> verbo(V), g_verbal(GV), g_preposicional(GPrep).
g_verbal(gv(V, GN, GPrep)) --> verbo(V), g_nominal(GN), g_preposicional(GPrep).
g_verbal(gv(GAdv, GV, GPrep)) --> g_adverbial(GAdv), g_verbal(GV), g_preposicional(GPrep).
g_verbal(gv(GPrep1, GV, GPrep2)) --> g_preposicional(GPrep1), g_verbal(GV), g_preposicional(GPrep2).

%Grupo adjetival
g_adjetival(gadj(Adj)) --> adjetivo(Adj).
g_adjetival(gadj(Adj, Conj, GAdj)) --> adjetivo(Adj), conjuncion_coordinada(Conj), g_adjetival(GAdj).

%Grupo adverbial
g_adverbial(gadv(Adv)) --> adverbio(Adv).
g_adverbial(gadv(Adv, GAdv)) --> adverbio(Adv), g_adverbial(GAdv).

%Grupo preposicional
g_preposicional(gp(Prep, GN)) -->  preposicion(Prep), g_nominal(GN).
g_preposicional(gp(Prep, GN, GPrep)) -->  preposicion(Prep), g_nominal(GN), g_preposicional(GPrep).

%-------------------------------------------------------------------------------
%Elementos
%-------------------------------------------------------------------------------
determinante(det(X)) --> [X], {det(X)}.
nombre(n(X)) --> [X], {n(X)}.
nombre_propio(np(X)) --> [X], {np(X)}.
verbo(v(X)) --> [X], {v(X)}.
adjetivo(adj(X)) --> [X], {adj(X)}.
adverbio(adv(X)) --> [X], {adv(X)}.
conjuncion(conj(X)) --> [X], {conj(X)}.
conjuncion_coordinada(conjc(X)) --> [X], {conjc(X)}.
conjuncion_subordinada(conjs(X)) --> [X], {conjs(X)}.
preposicion(prep(X)) --> [X], {prep(X)}.
signo(sig(X)) --> [X], {sig(X)}.
pronoun(pron(X)) --> [X], {pron(X)}.


%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
%DICCIONARIO
%-------------------------------------------------------------------------------
%Determinantes
%-------------------------------------------------------------------------------
det('a').
det('an').
det('the').
det('The').
det('my').
det('your').
det('that').
det('this').
det('these').
det('those').
det('his').
det('her').
det('its').
det('ours').
det('their').
det('some').
det('any').
det('no').
det('every').
det('each').
det('either').
det('neither').
det('few').
det('many').
det('several').
det('all').
det('much').
det('a lot').
det('plenty of').
det('one').
det('two').
det('three').
det('four').
det('five').
det('six').
det('seven').
det('eight').
det('nine').
det('ten').
det('first').
det('second').
det('third').

%-------------------------------------------------------------------------------
%Nombres
%-------------------------------------------------------------------------------
n('law').
n('Law').
n('philosophy').
n('Philosophy').
n('man').
n('woman').
n('coffee').
n('newspaper').
n('table').
n('french fries').
n('novel').
n('paella').
n('beer').
n('juice').
n('wall').
n('apples').
n('word processor').
n('word').
n('processor').
n('documents').
n('mouse').
n('cat').
n('neighbor').
n('evenings').
n('tool').
n('who').
n('which').
n('it').
n('yesterday').
n('we').

%-------------------------------------------------------------------------------
%Nombres propios
%-------------------------------------------------------------------------------
np('José').
np('María').
np('Jose').
np('Maria').
np('Hector').
np('Irene').

%-------------------------------------------------------------------------------
%Verbos
%-------------------------------------------------------------------------------
v('is').
v('is having').
v('studies').
v('having').
v('clears').
v('drinks').
v('reads').
v('eat').
v('drink').
v('eats').
v('prefers').
v('sings').
v('jumps').
v('used').
v('writing').
v('caught').
v('was').
v('saw').
v('climbs').
v('is used').

%-------------------------------------------------------------------------------
%Adjetivos
%-------------------------------------------------------------------------------
adj('dark-haired').
adj('tall').
adj('french').
adj('agile').
adj('climbing').
adj('picky').
adj('red').
adj('powerful').
adj('slow').
adj('gray').
adj('writing').

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
conj('for').
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
%Conjunciones Coordinadas
%-------------------------------------------------------------------------------
conjc('and').
conjc(',').
conjc('but').
conjc('or').
conjc('so').
conjc('yet').
conjc('for').
conjc('nor').
conjc('that').

%-------------------------------------------------------------------------------
%Conjunciones Subordinadas
%-------------------------------------------------------------------------------
conjs('after').
conjs('although').
conjs('as').
conjs('as if').
conjs('because').
conjs('before').
conjs('even though').
conjs('if').
conjs('in order that').
conjs('once').
conjs('provided that').
conjs('rather than').
conjs('than').
conjs('though').
conjs('when').
conjs('where').
conjs('wherever').
conjs('while').

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
%Signos
%-------------------------------------------------------------------------------
sig('?').
sig('!').

%-------------------------------------------------------------------------------
%Oraciones
%-------------------------------------------------------------------------------
oracion1(['Jose', 'is', 'dark-haired', 'and', 'Maria', 'is', 'tall']).
oracion2(['Jose', 'studies', 'Philosophy', 'but', 'Maria', 'studies', 'Law']).
oracion3(['Maria', 'is having', 'a', 'coffee', 'while', 'Jose', 'clears', 'the', 'table']).
oracion4(['Jose', 'drinks', 'coffee', 'and', 'reads', 'the', 'newspaper']).
oracion5(['Jose', 'and', 'Hector', 'eat', 'french fries', 'and', 'drink', 'beer']).
oracion6(['Jose', 'eats', 'french fries', 'but', 'Maria', 'prefers', 'paella', 'although', 'Hector', 'drinks', 'coffee', 'and', 'Irene', 'reads', 'a', 'novel']).
oracion7(['Irene', 'sings', 'and', 'jumps', 'while', 'Jose', 'studies']).
oracion8(['Hector', 'eats', 'french fries', 'and', 'drinks', 'juice', 'while', 'Jose', 'sings', 'and', 'jumps', 'although', 'Maria', 'reads', 'a', 'novel']).
oracion9(['Jose', 'who', 'is', 'agile', ',', 'climbs', 'at', 'the', 'climbing', 'wall', 'in', 'the', 'evenings']).
oracion10(['Jose', 'who', 'is', 'very', 'picky', ',', 'only', 'eats', 'red', 'apples']).
oracion11(['The', 'word processor', 'which', 'is', 'a', 'quite', 'powerful', 'tool', ',', 'is used', 'for', 'writing', 'documents']).
oracion12(['The', 'word processor', 'is', 'a', 'very', 'powerful', 'tool', 'that', 'is used', 'for', 'writing', 'documents', 'but', 'it', 'is', 'quite', 'slow']).
oracion13(['The', 'mouse', 'that', 'the', 'cat', 'caught', 'was', 'gray']).
oracion14(['The', 'man', 'we', 'saw', 'yesterday', 'was', 'my', 'neighbor']).


%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
%Simplificacion
%-------------------------------------------------------------------------------
obtener(Oracion, L):-
   obtener_oracion(Oracion, 1, [], LSucia),
   limpiar_oraciones(LSucia, [], L).

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


eliminar_caracter(Cadena, Caracter, NuevaCadena) :-
    atom_chars(Cadena, ListaCaracteres),
    delete(ListaCaracteres, Caracter, ListaSinCaracter),
    atom_chars(NuevaCadena, ListaSinCaracter).

