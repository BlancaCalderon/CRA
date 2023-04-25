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
oracion_simple(o(GV)) --> g_verbal(GV, _, _).                                                                                              %Tambien pueden estar formadas solo por el grupo verbal(oraciones con sujeto omitido)

%-------------------------------------------------------------------------------
%Definición de las oraciones coordinadas(Dos o más oraciones simples separadas por una conjuncion)
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
%Definición de las oraciones compuestas (oraciones coordinadas y subordinadas)
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

%g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).
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
g_verbal(gv(V, GN), _, Num) --> verbo(V, _, Num), cd(GN, _, _).                                                                                      %Verbo seguido de un grupo nominal que actua como complemento directo
g_verbal(gv(V, GAdjAtr), _, Num) --> verbo(V, _, Num), g_adjetival_atributo(GAdjAtr).                                                                %Un verbo puede estar acompañado por un grupo adjetival que hará función de atributo
g_verbal(gv(V, GPrep), _, Num) --> verbo(V, _, Num), g_preposicional(GPrep).
g_verbal(gv(V, GPrep, GPrep2), _, Num) --> verbo(V, _, Num), g_preposicional(GPrep), g_preposicional(GPrep2).
g_verbal(gv(GAdv, GV), _, Num) --> g_adverbial(GAdv), g_verbal(GV, _, Num).                                                                          %Un verbo puede estar acompañado por un grupo adverbial

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
%Definición del grupo preposicional (preposicion acompaña a un grupo nominal) y pueden actuar como complementos circunstanciales
%-------------------------------------------------------------------------------                                                                     %Complemento cirscunstancial de lugar
g_preposicional(gp_ccl(Prep, GN)) -->  preposicion(Prep, _), g_nominal(GN, _, _, lugar).                                                             %Complemento cirscunstancial de tiempo
g_preposicional(gp_cct(Prep, GN)) -->  preposicion(Prep, _), g_nominal(GN, _, _, tiempo).                                                            %Complemento cirscunstancial de finalidad
g_preposicional(gp_ccf(Prep, GN)) -->  preposicion(Prep, finalidad), g_nominal(GN, _, _, _).
%g_preposicional(gp(Prep, GN)) -->  preposicion(Prep, _), g_nominal(GN, _, _, _).
%g_preposicional(gp(Prep, GN, Prep2, GN2)) -->  preposicion(Prep), g_nominal(GN, _, _), preposicion(Prep2), g_nominal(GN2, _, _).

%-------------------------------------------------------------------------------
%Definición de las funciones que puede tener un grupo nominal (complemento directo, atributo o complemento circunstancial)
%-------------------------------------------------------------------------------
cd(gn_atributo(Det, N), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, atributo).                                                    %Grupo nominal como atributo
cd(gn_atributo(Det, GAdj, N), Gen, Num) --> determinante(Det, Gen, Num), g_adjetival(GAdj), nombre(N, Gen, Num, _).                                  %Grupo nominal con adjetivo como atributo
cd(gn_cct(N), Gen, Num) --> nombre(N, Gen, Num, tiempo).                                                                                             %Grupo nominal como complemento cirscunstancial de tiempo
cd(gn_cct(Det, N), Gen, Num) --> determinante(Det, Gen, Num), nombre(N, Gen, Num, lugar).
cd(gn_cd(N), Gen, Num) --> nombre(N, Gen, Num, nombre).                                                                                              %grupo nomial como complemento directo
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
adverbio(adv(X)) --> [X], {adv(X)}.
conjuncion(conj(X)) --> [X], {conj(X)}.
preposicion(prep(X), CC) --> [X], {prep(X, CC)}.
signo(sig(X)) --> [X], {sig(X)}.
nexo(nx(X)) --> [X], {nx(X)}.                                                                    %Nexo se utiliza en las oraciones subordinadas
%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Diccionario (se le ha añadido el género y número de cada palabra)
%_______________________________________________________________________________
%_______________________________________________________________________________

%-------------------------------------------------------------------------------
%Determinantes  (determinante, genero, numero)
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
%Nombres   (nombre, genero, numero, tipo) tipo puede ser de tiempo, lugar, nombre normal etc y se usa para determinar que función realiza en al oración (tipo de complemento)
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
%Nombres propios    (nombre, genero, numero)
%-------------------------------------------------------------------------------
np('José', masculino, singular).
np('María', femenino, singular).
np('Jose', masculino, singular).
np('Maria', femenino, singular).
np('Hector', masculino, singular).
np('Irene', femenino, singular).

%-------------------------------------------------------------------------------
%Verbos   (verbo, tipo, numero) el genero de los verbos es el mismo que el del sujeto y su tipo indica su función, verbos copulativos (to be) indican una caracteristica del sujeto y estan seguidos de un atributo
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
%Adverbios (no tienen genero ni número)
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
%Conjunciones (no tienen genero ni número)
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
%Preposiciones (preposicion, tipo) el tipo indica su funcion dentro de la oracion determinando que complemento circunstancial puede indicar (tiempo, lugra, modo, finalidad etc)
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
   obtener_oracion(Oracion, LSucia),                                                                             %Método que dada una oración la divide en oraciones simples correspondientes
   limpiar_oraciones(LSucia, [], L).                                                                                 %Método que dada las oraciones simples obtenidas con el método anterior las limpia quitando las etiqeutas y (

obtener_oracion(Oracion, Final):-                                                                                 %Obtiene la etiqueta que indica que tipo de oraciobn es (o,oc,or,ocm)
    Oracion =.. [Etiqueta | _],                                                                                      %Si es una oracion simple (etiqueta es o) llama a método formar_oracion
    Etiqueta = o,
    formar_oracion([Oracion], Fin),                                                                                  %Devuelve oracion simple obtenida
    Final = Fin.

obtener_oracion(Oracion, Final):-                                                                                 %Si la oración pasada es coordinada (su etiqueta es oc)
    Oracion =.. [Etiqueta | Args],
    Etiqueta = oc,
    procesar_simple(Args, [], ListaOr),                                                                              %Llama a procesar_simple para obtener las oraciones simples que forman la coordinada
    reverse(ListaOr, ListaOr2),                                                                                      %El método anterior devuelve las oraciones al revés por lo que se les debe dar la vuelta
    formar_oracion(ListaOr2, Fin),
    Final = Fin.

obtener_oracion(Oracion, Final):-                                                                             %Si oracion pasada es un oracion compuesta (etiqueta ocm)
    Oracion =.. [Etiqueta | Args],
    Etiqueta = ocm,
    quitar_par(Args, Args2),                                                                                         %Método que convertir oracion como cola en compound
    obtener_oracion(Args2, Final2),                                                                           %Llamada recursiva al método recursivo con la segunda oración del par para obtener sus simples respectivas
    Final = Final2.

procesar_simple([ ], Final, Final).                                                                                  %Caso de parada

procesar_simple([Head | Tail], Lista, Fin):-
    Head =.. [Etiqueta | _],
    Etiqueta = o,                                                                                                    %Si la oración pasada es simple(etiqueta o)
    procesar_simple(Tail, [Head | Lista], Fin),!.                                                                    %Llamada recursiva con la cola(por si dentro de la oración hay una subordinada)

procesar_simple([_ | Tail], Lista, Fin):-                                                                            %Si la oración no es una simple se quita el primer elemnto haciendo llamada recursiva con la cola para avanzar con el análisis
    procesar_simple(Tail, Lista, Fin),!.

formar_oracion(Oraciones, Formadas):-                                                                                %Llama a metodo auxiliar
    formar_oracion_aux(Oraciones, [], Formadas).

formar_oracion_aux([ ], Fin, Fin).
                                                                                                                     %Si la oración tiene tanto sujeto como predicado (no hay sujeto omitido)
formar_oracion_aux([Head | Tail], Lista, Fin):-
    o(Suj, Predicado) = Head,
    separar_subordinada(Suj, Resultado),                                                                             %Método que si dentro de una oración (dentro del gn) hay una subordinada la saca y devuelve
    nth1(2, Resultado, Elem),                                                                                        %Si el segundo elemento devuelvo del metodo anterior es vacio es que no hay oracion subordinada
    (Elem = [] ->
    (formar_oracion_aux(Tail, [Head | Lista], Fin),!)                                                                %Llamada recursiva
    ;
    (nth1(1, Resultado, S),                                                                                          %Si no vacio hay subordinada guardandola en la lista de oraciones simples
    Nueva = o(S, Predicado),
    formar_oracion_aux(Tail, [Nueva, Elem], Fin),!)).                                                                %Llamada recursiva

formar_oracion_aux([Head | Tail], Lista, Fin):-                                                                      %Si la oración solo tiene predicado (sujeto omitido) se obtiene el sujeto de la oración simple anterior de la lista de oraciones y se le pone  a la actual
    o(Predicado) = Head,
    nth1(1, Lista, Elem),
    o(Suj, _) = Elem,
    Nueva = o(Suj, Predicado),                                                                                       %Oración simple incluyendo el sujeto
    formar_oracion_aux(Tail, [Nueva| Lista], Fin),!.                                                                 %Llamada recursiva guardando la nueva oración simple generada

separar_subordinada(Lista, Salida):-                                                                                 %Método que obtiene la oracións ubordinada dentro de una simple
    Lista =.. [_ | Argumentos],
    separar(Argumentos, [], [], ListaDoble),
    Salida = ListaDoble.


separar([Head | Tail], Aux, Subordinada, Final):-
    Head =.. [Etiqueta | _],
    (Etiqueta = or ->                                                                                                %Si oracion es subordinada (etiqueta or) hace llamada recursiva sacandola
    separar(Tail, Aux, Head, Final),!;                                                                               %Si no es subordindada la devuelve metiendo la cabeza de la lista en Aux  y pasando la cola
    separar(Tail, [Head | Aux], Subordinada, Final),!
    ).

separar([ ], Aux, Subordinada, Final):-
    reverse(Aux, Aux2),                                                                                              %Da la vuelta a la lista para ordenarla ya que previamente se ha invertido
    Sujeto =.. [gn | Aux2],                                                                                          %Obtiene sujeto de la oración (etiqeuta gn)
    Subordinada =.. [_ | Args],                                                                                      %Obtiene la oración subordinada
    anadir(Args, Sujeto, [], Salida),                                                                                %Llama a metodo añadir
    Final = [Sujeto, Salida].

anadir([Head | Tail], Sujeto, Aux, Final):-
    Head =.. [Etiqueta | _],                                                                                         %Saca la eqiqueta, si es nexo (etiqueta nx) de la oración subordinada se quita de la llamada recursiva
    (Etiqueta = nx ->
    anadir(Tail, Sujeto, Aux, Final),!;                                                                              %Si no hay nexo no quita la cabeza de la lista manteniendolo en la llamada recursiva
    anadir(Tail, Sujeto, [Head | Aux], Final),!
    ).

anadir([ ], _, [ ], Final):-                                                                                         %Si oracion pasada y Aux es vacia devuelve otra vacia
    Final = [].

anadir([ ], Sujeto, Aux, Final):-                                                                                    %Si solo la oracion pasada es vacia pero Aux no
    reverse(Aux, Aux2),                                                                                              %Da la vuelta a Aux ya que en pasos anteriores se ha invertido
    length(Aux2, Tam),
    (Tam = 1 ->                                                                                                      %Si el tamaño de Aux es 1 llamamos a quitar_par
    (quitar_par(Aux2, Aux3),
    Final = o(Sujeto, Aux3));
    Final =.. [o | Aux2]).

quitar_par([Con], Sin):-                                                                                             %Convierte oracion simple como cola a un compound
    Sin = Con.


%-------------------------------------------------------------------------------
%Metodos para limpiar oraciones quitando paréntesis y etiquetas
%-------------------------------------------------------------------------------
limpiar_oraciones([ ], Final, Final).                                                                                %Caso de parada

limpiar_oraciones([Head | Tail], Final, Aux):-
    limpiar_oracion(Head, Resultado),                                                                                %Llama a método limpiarOracion que elimina etiqeutas de la cabeza de la lista (primer elemento de la oración)
    limpiar_oraciones(Tail, [Resultado | Final], Aux).
                                                                                                                     %Llamada recursiva para limpiar el resto de la oracion
limpiar_oracion(Oracion, Cadena):-
    term_to_atom(Oracion, Atom),                                                                                     %Limpia elemento pasado de la oracón pasandolo a tipo Atom
    atomic_list_concat(Atoms, ',', Atom),
    maplist(atom_string, Atoms, Lista),
    procesar_elementos(Lista, [], Resultado),                                                                        %Llama a método procesar_elementos que devuelve lista limpia sin etiquetas ni parentesis
    atomic_list_concat(Resultado, ' ', Cad),
    Cadena = Cad.                                                                                                    %Se concatena resultado de la enterior(lista ya limpia)

procesar_elementos([ ], Final, Final).                                                                               %Caso base

procesar_elementos([Head|Tail], Final, Aux):-
    split_string(Head, "(", "", Lista),                                                                              %Separa la lista por (
    length(Lista, Tam),                                                                                              %Saca el tamaño de la lista separada
    nth1(Tam, Lista, Elem),                                                                                          %Guarda en Elem ultimo elemento de la lista separada
    eliminar_caracter(Elem, ')', Resultado),                                                                         %Llama a metodo eliminar caracteres para devolver elemento pasado sin paréntesis
    eliminar_caracter(Resultado, '\'', Resultado2),                                                                  %Llama a metodo eliminar caracteres para devolver elemento pasado sin comillas simples
    append(Final, [Resultado2], ListaFinal),                                                                         %Junta elemento limpio con lista Final
    procesar_elementos(Tail, ListaFinal, Aux).                                                                       %Llamada recursiva para limpiar siguiente elemento

eliminar_caracter(Cadena, Caracter, NuevaCadena):-                                                                   %Método que elimina de una cadena pasada el caracter indicado
    atom_chars(Cadena, ListaCaracteres),                                                                             %Convierte cadena en lista de caracteres
    delete(ListaCaracteres, Caracter, ListaSinCaracter),                                                             %Borra de la lista de caracteres el caracter pasado
    atom_chars(NuevaCadena, ListaSinCaracter).                                                                       %Vuelve a convertir resultado en una cadena
    
    
%-------------------------------------------------------------------------------
%Pruebas
%-------------------------------------------------------------------------------
arbol1():-
    oracion1(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol2():-
    oracion2(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol3():-
    oracion3(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol4():-
    oracion4(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol5():-
    oracion5(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol6():-
    oracion6(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol7():-
    oracion7(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).

arbol8():-
    oracion8(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol9():-
    oracion9(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol10():-
    oracion10(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol11():-
    oracion11(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol12():-
    oracion12(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol13():-
    oracion13(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    
arbol14():-
    oracion14(L),
    oracion(X, L, []),
    draw(X),
    obtener_oracion(X, Lista),
    dibujar_arboles(Lista).
    

dibujar_lista([ ]).

dibujar_arboles([Head | Tail]):-
    draw(Head),
    dibujar_arboles(Tail).

%-------------------------------------------------------------------------------
mostrar_oraciones1():-
    oracion1(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones2():-
    oracion2(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones3():-
    oracion3(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones4():-
    oracion4(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones5():-
    oracion5(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones6():-
    oracion6(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones7():-
    oracion7(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones8():-
    oracion8(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).

mostrar_oraciones9():-
    oracion9(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones10():-
    oracion10(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones11():-
    oracion11(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones12():-
    oracion12(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones13():-
    oracion13(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).
    
mostrar_oraciones14():-
    oracion14(L),
    oracion(X, L, []),
    obtener(X, S),
    write(S).

