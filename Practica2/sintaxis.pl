%_______________________________________________________________________________
%_______________________________________________________________________________
%                              Gramatica
%_______________________________________________________________________________
%_______________________________________________________________________________

%Oraciones generales
%-------------------------------------------------------------------------------
oracion(O) --> oracion_simple(O).
oracion(O) --> oracion_compuesta(O).

%-------------------------------------------------------------------------------
%Definición de las oraciones simples
%-------------------------------------------------------------------------------
oracion_simple(o(GN, GV)) --> g_nominal(GN, Gen, Num, _), g_verbal(GV, Gen, Num).                                                          %Generalmente formadas por un grupo nomial seguido de un verbal(sujeto y predicado)
oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV, Gen, Num), g_nominal(GN, Gen, Num, _), signo(Sig).                                         %Se incluye que puedan terminar con un signo para tener en cuenta oraciones interrogativas y exclamativas
oracion_simple(o(GV)) --> g_verbal(GV, _, _).                                                                             %Tambien pueden estar formadas solo por el grupo verbal(oraciones con sujeto omitido)

%-------------------------------------------------------------------------------
%Definición de las oraciones coordinadas(Dos oraciones simples separadas por una conjuncion)
%-------------------------------------------------------------------------------
oracion_coordinada(oc(O1, Conj, O2)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2).
oracion_coordinada(oc(O1, Conj, O2, Conj2, O3)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2), conjuncion(Conj2), oracion_simple(O3).
oracion_coordinada(oc(O1, Conj, O2, Conj2, O3, Conj3, O4)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2), conjuncion(Conj2), oracion_simple(O3), conjuncion(Conj3), oracion_simple(O4).
oracion_coordinada(oc(O1, Conj, O2, Conj2, O3, Conj3, O4, Conj4, O5)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2), conjuncion(Conj2), oracion_simple(O3), conjuncion(Conj3), oracion_simple(O4), conjuncion(Conj4), oracion_simple(O5).

%-------------------------------------------------------------------------------
%Definición de las oraciones subordinadas (pueden empeza con un nexo)
%-------------------------------------------------------------------------------
oracion_simple_sub(o(GN, GV)) --> g_nominal_sub(GN, Gen, Num), g_verbal(GV, Gen, Num).
                               %:(
oracion_subordinada(or(GN, GV), _, _) --> g_nominal(GN, _, _, _), g_verbal(GV, _, _).
oracion_subordinada(or(NX, GV), Gen, Num) --> nexo(NX), g_verbal(GV, Gen, Num).
oracion_subordinada(or(NX, GN, GV), _, _) --> nexo(NX), g_nominal(GN, _, _, _), g_verbal(GV, _, _).

%-------------------------------------------------------------------------------
%Definición de las oraciones compuestas (union de varias oraciones coordinadas, de dos o mas simples o de coordinadas y simples)
%-------------------------------------------------------------------------------
oracion_compuesta(ocm(OC)) --> oracion_coordinada(OC).
oracion_compuesta(ocm(OR)) --> oracion_simple_sub(OR).

%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Definición de los grupos sintácticos
%_______________________________________________________________________________
%_______________________________________________________________________________

%-------------------------------------------------------------------------------
%Definición del grupo nominal(nombre, nombre propio o determinante seguido de un nombre)
%-------------------------------------------------------------------------------
%de las subordinada
g_nominal_sub(gn(NP, OR), Gen, Num) --> nombre_propio(NP, Gen, Num), oracion_subordinada(OR, Gen, Num).
g_nominal_sub(gn(GN, OR), Gen, Num) --> cd(GN, _, _), oracion_subordinada(OR, Gen, Num).
%g_nominal_sub(gn(Det, N, OR), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, _), oracion_subordinada(OR, Gen, Num).

%-------------------------------------------------------------------------------
g_nominal(gn(N), Gen, Num, _) --> nombre(N, Gen, Num, _).
g_nominal(gn(NP), Gen, Num, _) --> nombre_propio(NP, Gen, Num).
g_nominal(gn(Det, N), Gen, Num, CC) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, CC).
g_nominal(gn(Det, GAdj, N), Gen, Num, CC) --> determinante(Det, Gen, Num), g_adjetival(GAdj), nombre(N, Gen, Num, CC).
g_nominal(gn(Det, GAdv, GN), Gen, Num, _) --> determinante(Det, Gen, Num), g_adverbial(GAdv), g_nominal(GN, Gen, Num, _).
g_nominal(gn(GAdj, N), Gen, Num, CC) --> g_adjetival(GAdj), nombre(N, Gen, Num, CC).
g_nominal(gn(GAdv, GAdj, N), Gen, Num, _) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre(N, Gen, Num, _).
g_nominal(gn(NP, Conj, GN), Gen, Num, _) --> nombre_propio(NP, Gen, Num), conjuncion(Conj), g_nominal(GN, _, _, _).

%g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).                                              %Dentro de un grupo nominal puede haber una oracion subordinada
%g_nominal(gn(NP, GN)) --> nombre_propio(NP), g_nominal(GN).
%g_nominal(gn(GAdv, N)) --> g_adverbial(GAdv), nombre(N).                                                            %Un grupo nominal puede ser acompañado por un adverbio
%g_nominal(gn(GAdv, NP)) --> g_adverbial(GAdv), nombre_propio(NP).
%g_nominal(gn(Det, N, GN)) --> determinante(Det), nombre(N), g_nominal(GN).                                              %Un grupo nominal puede ser acompañado por un adjetivo
%g_nominal(gn(Det, GAdv, N)) --> determinante(Det), g_adverbial(GAdv), nombre(N).
%g_nominal(gn(GAdj, NP)) --> g_adjetival(GAdj), nombre_propio(NP).
%g_nominal(gn(GAdv, GAdj, NP)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP).
%g_nominal(gn(GPrep, GN)) --> g_preposicional(GPrep), g_nominal(GN).
%g_nominal(gn(N, Conj, GN)) --> nombre(N), conjuncion(Conj), g_nominal(GN).                                           %Puede haber doble sujeto (separado por y)
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
g_verbal(gv(V), _, Num) --> verbo(V, _, Num).
g_verbal(gv(V, GN), _, Num) --> verbo(V, _, Num), cd(GN, _, _).                                                             %Un verbo puede estar acompañado por un grupo adjetival
g_verbal(gv(V, GAdjAtr), _, Num) --> verbo(V, _, Num), g_adjetival_atributo(GAdjAtr).                                                             %Un verbo puede estar acompañado por un grupo adverbial
g_verbal(gv(V, GPrep), _, Num) --> verbo(V, _, Num), g_preposicional(GPrep).
g_verbal(gv(V, GPrep, GPrep2), _, Num) --> verbo(V, _, Num), g_preposicional(GPrep), g_preposicional(GPrep2).
g_verbal(gv(GAdv, GV), _, Num) --> g_adverbial(GAdv), g_verbal(GV, _, Num).

%g_verbal(gv(V, GAdv)) --> verbo(V), g_adverbial(GAdv).
%g_verbal(gv(GPrep, GV)) --> g_preposicional(GPrep), g_verbal(GV).
%g_verbal(gv(V, GAdj, GPrep)) --> verbo(V), g_adjetival(GAdj), g_preposicional(GPrep).
%g_verbal(gv(V, GAdv, GAdj), Gen, Num) --> verbo(V, Gen, Num), g_adverbial(GAdv), g_adjetival(GAdj).
%g_verbal(gv(V, GV, GPrep)) --> verbo(V), g_verbal(GV), g_preposicional(GPrep).
%g_verbal(gv(V, GN, GPrep)) --> verbo(V), g_nominal(GN), g_preposicional(GPrep).
%g_verbal(gv(GAdv, GV, GPrep)) --> g_adverbial(GAdv), g_verbal(GV), g_preposicional(GPrep).
%g_verbal(gv(GPrep1, GV, GPrep2)) --> g_preposicional(GPrep1), g_verbal(GV), g_preposicional(GPrep2).

%-------------------------------------------------------------------------------
%Definición del grupo ajetival (un solo adjetivo o varios separados por una conjuncion)
%-------------------------------------------------------------------------------
g_adjetival(gadj(Adj)) --> adjetivo(Adj).
g_adjetival(gadj(GAdv, Adj)) --> g_adverbial(GAdv), adjetivo(Adj).
g_adjetival(gadj(Adj, Conj, GAdj)) --> adjetivo(Adj), conjuncion(Conj), g_adjetival(GAdj).

g_adjetival_atributo(gadj_atr(Adj)) --> adjetivo(Adj).
g_adjetival_atributo(gadj_atr(GAdv, Adj)) --> g_adverbial(GAdv), adjetivo(Adj).
g_adjetival_atributo(gadj_atr(Adj, Conj, GAdj)) --> adjetivo(Adj), conjuncion(Conj), g_adjetival(GAdj).

%-------------------------------------------------------------------------------
%Definición del grupo adverbial (un solo adverbio o varios)
%-------------------------------------------------------------------------------
g_adverbial(gadv(Adv)) --> adverbio(Adv).
g_adverbial(gadv(Adv, GAdv)) --> adverbio(Adv), g_adverbial(GAdv).

%-------------------------------------------------------------------------------
%Definición del grupo preposicional (preposicion acompaña a un grupo nomial)
%-------------------------------------------------------------------------------
g_preposicional(gp_ccl(Prep, GN)) -->  preposicion(Prep, _), g_nominal(GN, _, _, lugar).
g_preposicional(gp_cct(Prep, GN)) -->  preposicion(Prep, _), g_nominal(GN, _, _, tiempo).
g_preposicional(gp_ccf(Prep, GN)) -->  preposicion(Prep, finalidad), g_nominal(GN, _, _, _).
%g_preposicional(gp(Prep, GN)) -->  preposicion(Prep, _), g_nominal(GN, _, _, _).
%g_preposicional(gp(Prep, GN, Prep2, GN2)) -->  preposicion(Prep), g_nominal(GN, _, _), preposicion(Prep2), g_nominal(GN2, _, _).

%---
%CD
%---
cd(gn_atributo(Det, N), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, atributo).
cd(gn_atributo(Det, GAdj, N), Gen, Num) --> determinante(Det, Gen, Num), g_adjetival(GAdj), nombre(N, Gen, Num, _).
cd(gn_cct(N), Gen, Num) --> nombre(N, Gen, Num, tiempo).
cd(gn_cct(Det, N), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, lugar).
cd(gn_cd(N), Gen, Num) --> nombre(N, Gen, Num, nombre).
cd(gn_cd(Det, N), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, nombre).
cd(gn_cd(GAdj, N), Gen, Num) --> g_adjetival(GAdj), nombre(N, Gen, Num, nombre).

%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Elementos de la gramatica
%_______________________________________________________________________________
%_______________________________________________________________________________

determinante(det(X), Gen, Num) --> [X], {det(X, Gen, Num)}.
nombre(n(X), Gen, Num, CC) --> [X], {n(X, Gen, Num, CC)}.
nombre_propio(np(X), Gen, Num) --> [X], {np(X, Gen, Num)}.
verbo(v(X), Cop, Num) --> [X], {v(X, Cop, Num)}.
adjetivo(adj(X)) --> [X], {adj(X)}.
pronombre(pron(X)) --> [X], {pron(X)}.

adverbio(adv(X)) --> [X], {adv(X)}.
conjuncion(conj(X)) --> [X], {conj(X)}.
preposicion(prep(X), CC) --> [X], {prep(X, CC)}.
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
det('a', _, singular).
det('an', _, singular).
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
det('each', _, singular).
det('either', _, _).
det('neither', _, _).
det('few', _, plural).
det('many', _, plural).
det('several', _, plural).
det('all', _, _).
det('much', _, plural).
det('a lot of', _, _).
det('plenty of', _, plural).
det('one', _, singular).
det('two', _, plural).
det('three', _, plural).
det('four', _, plural).
det('five', _, plural).
det('six', _, plural).
det('seven', _, plural).
det('eight', _, plural).
det('nine', _, plural).
det('ten', _, plural).
det('first', _, _).
det('second', _, _).
det('third', _, _).

%-------------------------------------------------------------------------------
%Nombres
%-------------------------------------------------------------------------------
n('law', femenino, singular, nombre).
n('Law', femenino, singular, nombre).
n('philosophy', femenino, singular, nombre).
n('Philosophy', femenino, singular, nombre).
n('man', masculino, singular, nombre).
n('woman', femenino, singular, nombre).
n('coffee', masculino, singular, nombre).
n('newspaper', masculino, singular, nombre).
n('table', femenino, singular, lugar).
n('french fries', femenino, plural, nombre).
n('novel', femenino, singular, nombre).
n('paella', femenino, singular, nombre).
n('beer', femenino, singular, nombre).
n('juice', masculino, singular, nombre).
n('wall', femenino, singular, lugar).
n('apples', femenino, plural, nombre).
n('word processor', masculino, singular, nombre).
n('word', femenino, singular, nombre).
n('processor', masculino, singular, nombre).
n('documents', masculino, plural, nombre).
n('mouse', _, singular, nombre).
n('cat', _, singular, nombre).
n('neighbor', _, singular, atributo).
n('evenings', femenino, plural, tiempo).
n('tool', femenino, singular, nombre).
n('it', _, singular, nombre).
n('yesterday', masculino, singular, tiempo).
n('we', _, plural, nombre).

%-------------------------------------------------------------------------------
%Nombres propios
%-------------------------------------------------------------------------------
np('José', masculino, singular).
np('María', femenino, singular).
np('Jose', masculino, singular).
np('Maria', femenino, singular).
np('Hector', masculino, singular).
np('Irene', femenino, singular).

%-------------------------------------------------------------------------------
%Verbos
%-------------------------------------------------------------------------------
v('is', copulativo, singular).
v('are', copulativo, plural).
v('is having', no, singular).
v('studies', no, singular).
v('having', no, _).
v('clears', no, singular).
v('drinks', no, singular).
v('reads', no, singular).
v('eat', no, _).
v('drink', no, _).
v('eats', no, singular).
v('prefers', no, singular).
v('sings', no, singular).
v('jumps', no, singular).
v('used', no, _).
v('writing', no, _).
v('caught', no, _).
v('was', copulativo, singular).
v('saw', no, _).
v('climbs', no, singular).
v('is used', no, _).

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
prep('above', _).
prep('about', _).
prep('across', lugar).
prep('after', tiempo).
prep('against', _).
prep('along', compania).
prep('among', _).
prep('around', _).
prep('at', _).
prep('before', tiempo).
prep('behind', lugar).
prep('below', lugar).
prep('beneath', _).
prep('besides', _).
prep('between', _).
prep('beyond', _).
prep('but', _).
prep('by', _).
prep('despite', _).
prep('except', _).
prep('for', finalidad).
prep('from', _).
prep('in', _).
prep('inside', lugar).
prep('into', _).
prep('of', _).
prep('outside', lugar).
prep('to', _).
prep('under', lugar).
prep('until', tiempo).
prep('with', _).
prep('toward', _).
prep('without', _).

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
   obtener_oracion(Oracion, [], LSucia),
   limpiar_oraciones(LSucia, [], L).
   
obtener_oracion([ ], Fin, Fin).

obtener_oracion(Oracion, _, Final):-
    Oracion =.. [Etiqueta | _],
    Etiqueta = o,
    formar_oracion([Oracion], Fin),
    Final = Fin.

obtener_oracion(Oracion, _, Final):-
    Oracion =.. [Etiqueta | Args],
    Etiqueta = oc,
    procesar_simple(Args, [], ListaOr),
    reverse(ListaOr, ListaOr2),
    formar_oracion(ListaOr2, Fin),
    Final = Fin.

obtener_oracion(Oracion, Lista, Final):-
    Oracion =.. [Etiqueta | Args],
    Etiqueta = ocm,
    quitar_par(Args, Args2),
    obtener_oracion(Args2, Lista, Final2),
    Final = Final2.

procesar_simple([ ], Final, Final).

procesar_simple([Head | Tail], Lista, Fin):-
    Head =.. [Etiqueta | _],
    Etiqueta = o,
    procesar_simple(Tail, [Head | Lista], Fin),!.
    
procesar_simple([_ | Tail], Lista, Fin):-
    procesar_simple(Tail, Lista, Fin),!.
    
formar_oracion(Oraciones, Formadas):-
    formar_oracion_aux(Oraciones, [], Formadas).
    
formar_oracion_aux([ ], Fin, Fin).

formar_oracion_aux([Head | Tail], Lista, Fin):-
    o(Suj, Predicado) = Head,
    separar_subordinada(Suj, Resultado),
    nth1(2, Resultado, Elem),
    (Elem = [] ->
    (formar_oracion_aux(Tail, [Head | Lista], Fin),!)
    ;
    (nth1(1, Resultado, S),
    Nueva = o(S, Predicado),
    formar_oracion_aux(Tail, [Nueva, Elem], Fin),!)).
    
formar_oracion_aux([Head | Tail], Lista, Fin):-
    o(Predicado) = Head,
    nth1(1, Lista, Elem),
    o(Suj, _) = Elem,
    Nueva = o(Suj, Predicado),
    formar_oracion_aux(Tail, [Nueva| Lista], Fin),!.

%-------------------------------------------------------------------------------
separar_subordinada(Lista, Salida):-
    Lista =.. [_ | Argumentos],
    separar(Argumentos, [], [], ListaDoble),
    Salida = ListaDoble.


separar([Head | Tail], Aux, Subordinada, Final):-
    Head =.. [Etiqueta | _],
    (Etiqueta = or ->
    separar(Tail, Aux, Head, Final),!;
    separar(Tail, [Head | Aux], Subordinada, Final),!
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
    anadir(Tail, Sujeto, Aux, Final),!;
    anadir(Tail, Sujeto, [Head | Aux], Final),!
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

