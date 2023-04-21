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
oracion_simple(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).                                                          %Generalmente formadas por un grupo nomial seguido de un verbal(sujeto y predicado)
oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV), g_nominal(GN), signo(Sig).                                         %Se incluye que puedan terminar con un signo para tener en cuenta oraciones interrogativas y exclamativas
oracion_simple(o(GV)) --> g_verbal(GV).                                                                             %Tambien pueden estar formadas solo por el grupo verbal(oraciones con sujeto omitido)

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
oracion_simple_sub(o(GN, GV)) --> g_nominal_sub(GN), g_verbal(GV).
                               %:(
oracion_subordinada(or(GN, GV)) --> g_nominal(GN), g_verbal(GV).
oracion_subordinada(or(NX, GV)) --> nexo(NX), g_verbal(GV).
oracion_subordinada(or(NX, GN, GV)) --> nexo(NX), g_nominal(GN), g_verbal(GV).

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
g_nominal_sub(gn(NP, OR)) --> nombre_propio(NP), oracion_subordinada(OR).
g_nominal_sub(gn(GN, OR)) --> g_nominal(GN), oracion_subordinada(OR).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).
g_nominal(gn(NP)) --> nombre_propio(NP).                                             %Dentro de un grupo nominal puede haber una oracion subordinada
g_nominal(gn(NP, GN)) --> nombre_propio(NP), g_nominal(GN).
g_nominal(gn(GAdv, N)) --> g_adverbial(GAdv), nombre(N).                                                            %Un grupo nominal puede ser acompañado por un adverbio
g_nominal(gn(GAdv, NP)) --> g_adverbial(GAdv), nombre_propio(NP).
g_nominal(gn(Det, N)) --> determinante(Det), nombre(N).
g_nominal(gn(Det, N, GN)) --> determinante(Det), nombre(N), g_nominal(GN).
g_nominal(gn(Det, GAdj, N)) --> determinante(Det), g_adjetival(GAdj), nombre(N).                                    %Un grupo nominal puede ser acompañado por un adjetivo
g_nominal(gn(Det, GAdv, N)) --> determinante(Det), g_adverbial(GAdv), nombre(N).
g_nominal(gn(Det, GAdv, GN)) --> determinante(Det), g_adverbial(GAdv), g_nominal(GN).
g_nominal(gn(GAdj, N)) --> g_adjetival(GAdj), nombre(N).
g_nominal(gn(GAdj, NP)) --> g_adjetival(GAdj), nombre_propio(NP).
g_nominal(gn(GAdv, GAdj, N)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre(N).
g_nominal(gn(GAdv, GAdj, NP)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP).
g_nominal(gn(GPrep, GN)) --> g_preposicional(GPrep), g_nominal(GN).

g_nominal(gn(N, Conj, GN)) --> nombre(N), conjuncion(Conj), g_nominal(GN).                                           %Puede haber doble sujeto (separado por y)
g_nominal(gn(NP, Conj, GN)) --> nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GAdv, N, Conj, GN)) --> g_adverbial(GAdv), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GAdv, NP, Conj, GN)) --> g_adverbial(GAdv), nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(Det, N, Conj, GN)) --> determinante(Det), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(Det, GAdj, N, Conj, GN)) --> determinante(Det), g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(Det, GAdv, N, Conj, GN)) --> determinante(Det), g_adverbial(GAdv), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(Det, GAdv, GAdj, N, Conj, GN)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GAdj, N, Conj, GN)) --> g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GAdj, NP, Conj, GN)) --> g_adjetival(GAdj), nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GAdv, GAdj, N, Conj, GN)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre(N), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GAdv, GAdj, NP, Conj, GN)) --> g_adverbial(GAdv), g_adjetival(GAdj), nombre_propio(NP), conjuncion(Conj), g_nominal(GN).
g_nominal(gn(GPrep, GN1, Conj, GN2)) --> g_preposicional(GPrep), g_nominal(GN1), conjuncion(Conj), g_nominal(GN2).

%-------------------------------------------------------------------------------
%Definición del grupo verbal(verbo)
%-------------------------------------------------------------------------------
g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V, GN)) --> verbo(V), g_nominal(GN).                                                                     %Un verbo puede estar acompañado por un grupo adjetival
g_verbal(gv(V, GAdj)) --> verbo(V), g_adjetival(GAdj).                                                               %Un verbo puede estar acompañado por un grupo adverbial
g_verbal(gv(V, GAdv)) --> verbo(V), g_adverbial(GAdv).                                                               %Un verbo puede estar acompañado por un grupo preposicional
g_verbal(gv(V, GPrep)) --> verbo(V), g_preposicional(GPrep).
g_verbal(gv(GAdv, GV)) --> g_adverbial(GAdv), g_verbal(GV).
g_verbal(gv(GPrep, GV)) --> g_preposicional(GPrep), g_verbal(GV).
g_verbal(gv(V, GAdj, GPrep)) --> verbo(V), g_adjetival(GAdj), g_preposicional(GPrep).
g_verbal(gv(V, GAdv, GAdj)) --> verbo(V), g_adverbial(GAdv), g_adjetival(GAdj).
g_verbal(gv(V, GV, GPrep)) --> verbo(V), g_verbal(GV), g_preposicional(GPrep).
g_verbal(gv(V, GN, GPrep)) --> verbo(V), g_nominal(GN), g_preposicional(GPrep).
g_verbal(gv(GAdv, GV, GPrep)) --> g_adverbial(GAdv), g_verbal(GV), g_preposicional(GPrep).
g_verbal(gv(GPrep1, GV, GPrep2)) --> g_preposicional(GPrep1), g_verbal(GV), g_preposicional(GPrep2).

%-------------------------------------------------------------------------------
%Definición del grupo ajetival (un solo adjetivo o varios separados por una conjuncion)
%-------------------------------------------------------------------------------
g_adjetival(gadj(Adj)) --> adjetivo(Adj).
g_adjetival(gadj(GAdv, Adj)) --> g_adverbial(GAdv), adjetivo(Adj).
g_adjetival(gadj(Adj, Conj, GAdj)) --> adjetivo(Adj), conjuncion(Conj), g_adjetival(GAdj).

%-------------------------------------------------------------------------------
%Definición del grupo adverbial (un solo adverbio o varios)
%-------------------------------------------------------------------------------
g_adverbial(gadv(Adv)) --> adverbio(Adv).
g_adverbial(gadv(Adv, GAdv)) --> adverbio(Adv), g_adverbial(GAdv).

%-------------------------------------------------------------------------------
%Definición del grupo preposicional (preposicion acompaña a un grupo nomial)
%-------------------------------------------------------------------------------
g_preposicional(gp(Prep, GN)) -->  preposicion(Prep), g_nominal(GN).
g_preposicional(gp(Prep, GN, GPrep)) -->  preposicion(Prep), g_nominal(GN), g_preposicional(GPrep).

%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Elementos de la gramatica
%_______________________________________________________________________________
%_______________________________________________________________________________

determinante(det(X)) --> [X], {det(X)}.
nombre(n(X)) --> [X], {n(X)}.
nombre_propio(np(X)) --> [X], {np(X)}.
verbo(v(X)) --> [X], {v(X)}.
adjetivo(adj(X)) --> [X], {adj(X)}.
adverbio(adv(X)) --> [X], {adv(X)}.
conjuncion(conj(X)) --> [X], {conj(X)}.
preposicion(prep(X)) --> [X], {prep(X)}.
signo(sig(X)) --> [X], {sig(X)}.
nexo(nx(X)) --> [X], {nx(X)}.                                                                                        %Nexo se utiliza en las oraciones subordinadas
%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Diccionario
%_______________________________________________________________________________
%_______________________________________________________________________________

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
