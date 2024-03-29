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
%Definici�n de las oraciones simples
%-------------------------------------------------------------------------------
oracion_simple(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).                                                          %Generalmente formadas por un grupo nominal seguido de un verbal(sujeto y predicado)
oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV), g_nominal(GN), signo(Sig).                                         %Se incluye que puedan terminar con un signo para tener en cuenta oraciones interrogativas y exclamativas
oracion_simple(o(GV)) --> g_verbal(GV).                                                                             %Tambien pueden estar formadas solo por el grupo verbal(oraciones con sujeto omitido)

%-------------------------------------------------------------------------------
%Definici�n de las oraciones coordinadas(Dos o m�s oraciones simples separadas por una conjuncion)
%-------------------------------------------------------------------------------
oracion_coordinada(oc(O1, Conj, O2)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2).
oracion_coordinada(oc(O1, Conj, O2, Conj2, O3)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2), conjuncion(Conj2), oracion_simple(O3).
oracion_coordinada(oc(O1, Conj, O2, Conj2, O3, Conj3, O4)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2), conjuncion(Conj2), oracion_simple(O3), conjuncion(Conj3), oracion_simple(O4).
oracion_coordinada(oc(O1, Conj, O2, Conj2, O3, Conj3, O4, Conj4, O5)) --> oracion_simple(O1), conjuncion(Conj), oracion_simple(O2), conjuncion(Conj2), oracion_simple(O3), conjuncion(Conj3), oracion_simple(O4), conjuncion(Conj4), oracion_simple(O5).

%-------------------------------------------------------------------------------
%Definici�n de las oraciones subordinadas (pueden empeza con un nexo)
%-------------------------------------------------------------------------------
oracion_simple_sub(o(GN, GV)) --> g_nominal_sub(GN), g_verbal(GV).
oracion_subordinada(or(GN, GV)) --> g_nominal(GN), g_verbal(GV).
oracion_subordinada(or(NX, GV)) --> nexo(NX), g_verbal(GV).
oracion_subordinada(or(NX, GN, GV)) --> nexo(NX), g_nominal(GN), g_verbal(GV).

%-------------------------------------------------------------------------------
%Definici�n de las oraciones compuestas (oraciones coordinadas y subordinadas)
%-------------------------------------------------------------------------------
oracion_compuesta(ocm(OC)) --> oracion_coordinada(OC).
oracion_compuesta(ocm(OR)) --> oracion_simple_sub(OR).
%_______________________________________________________________________________
%_______________________________________________________________________________
                 %Definici�n de los grupos sint�cticos
%_______________________________________________________________________________
%_______________________________________________________________________________

%-------------------------------------------------------------------------------
%Definici�n del grupo nominal(nombre, nombre propio o determinante seguido de un nombre)
%-------------------------------------------------------------------------------
g_nominal_sub(gn(NP, OR)) --> nombre_propio(NP), oracion_subordinada(OR).
g_nominal_sub(gn(GN, OR)) --> g_nominal(GN), oracion_subordinada(OR).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).
g_nominal(gn(NP)) --> nombre_propio(NP).
g_nominal(gn(NP, GN)) --> nombre_propio(NP), g_nominal(GN).
g_nominal(gn(GAdv, N)) --> g_adverbial(GAdv), nombre(N).                                                            %Un grupo nominal puede ser acompa�ado por un adverbio
g_nominal(gn(GAdv, NP)) --> g_adverbial(GAdv), nombre_propio(NP).
g_nominal(gn(Det, N)) --> determinante(Det), nombre(N).
g_nominal(gn(Det, N, GN)) --> determinante(Det), nombre(N), g_nominal(GN).
g_nominal(gn(Det, GAdj, N)) --> determinante(Det), g_adjetival(GAdj), nombre(N).                                    %Un grupo nominal puede ser acompa�ado por un adjetivo
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
%Definici�n del grupo verbal(verbo)
%-------------------------------------------------------------------------------
g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V, GN)) --> verbo(V), g_nominal(GN).                                                                     %Un verbo puede estar acompa�ado por un grupo adjetival
g_verbal(gv(V, GAdj)) --> verbo(V), g_adjetival(GAdj).                                                               %Un verbo puede estar acompa�ado por un grupo adverbial
g_verbal(gv(V, GAdv)) --> verbo(V), g_adverbial(GAdv).                                                               %Un verbo puede estar acompa�ado por un grupo preposicional
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
%Definici�n del grupo ajetival (un solo adjetivo o varios separados por una conjuncion)
%-------------------------------------------------------------------------------
g_adjetival(gadj(Adj)) --> adjetivo(Adj).
g_adjetival(gadj(GAdv, Adj)) --> g_adverbial(GAdv), adjetivo(Adj).
g_adjetival(gadj(Adj, Conj, GAdj)) --> adjetivo(Adj), conjuncion(Conj), g_adjetival(GAdj).

%-------------------------------------------------------------------------------
%Definici�n del grupo adverbial (un solo adverbio o varios)
%-------------------------------------------------------------------------------
g_adverbial(gadv(Adv)) --> adverbio(Adv).
g_adverbial(gadv(Adv, GAdv)) --> adverbio(Adv), g_adverbial(GAdv).

%-------------------------------------------------------------------------------
%Definici�n del grupo preposicional (preposicion acompa�a a un grupo nomial)
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
np('Jos�').
np('Mar�a').
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
%Dado el an�lisis de las oraciones obtener las oraciones simples que componen oracion analizada
%-------------------------------------------------------------------------------
obtener(Oracion, L):-
   obtener_oracion(Oracion, LSucia),                                                                             %M�todo que dada una oraci�n la divide en oraciones simples correspondientes
   limpiar_oraciones(LSucia, [], L).                                                                                 %M�todo que dada las oraciones simples obtenidas con el m�todo anterior las limpia quitando las etiqeutas y (
                                                                                      %Condici�n de parada cuando oracion original es vacia (se han obtenido todas las simples posibles)
obtener_oracion(Oracion, Final):-                                                                                 %Obtiene la etiqueta que indica que tipo de oraciobn es (o,oc,or,ocm)
    Oracion =.. [Etiqueta | _],                                                                                      %Si es una oracion simple (etiqueta es o) llama a m�todo formar_oracion
    Etiqueta = o,
    formar_oracion([Oracion], Fin),                                                                                  %Devuelve oracion simple obtenida
    Final = Fin.

obtener_oracion(Oracion, Final):-                                                                                 %Si la oraci�n pasada es coordinada (su etiqueta es oc)
    Oracion =.. [Etiqueta | Args],
    Etiqueta = oc,
    procesar_simple(Args, [], ListaOr),                                                                              %Llama a procesar_simple para obtener las oraciones simples que forman la coordinada
    reverse(ListaOr, ListaOr2),                                                                                      %El m�todo anterior devuelve las oraciones al rev�s por lo que se les debe dar la vuelta
    formar_oracion(ListaOr2, Fin),
    Final = Fin.

obtener_oracion(Oracion, Final):-                                                                             %Si oracion pasada es un oracion compuesta (etiqueta ocm)
    Oracion =.. [Etiqueta | Args],
    Etiqueta = ocm,
    quitar_par(Args, Args2),                                                                                         %M�todo que convertir oracion como cola en compound
    obtener_oracion(Args2, Final2),                                                                           %Llamada recursiva al m�todo recursivo con la segunda oraci�n del par para obtener sus simples respectivas
    Final = Final2.

procesar_simple([ ], Final, Final).                                                                                  %Caso de parada

procesar_simple([Head | Tail], Lista, Fin):-
    Head =.. [Etiqueta | _],
    Etiqueta = o,                                                                                                    %Si la oraci�n pasada es simple(etiqueta o)
    procesar_simple(Tail, [Head | Lista], Fin),!.                                                                    %Llamada recursiva con la cola(por si dentro de la oraci�n hay una subordinada)

procesar_simple([_ | Tail], Lista, Fin):-                                                                            %Si la oraci�n no es una simple se quita el primer elemnto haciendo llamada recursiva con la cola para avanzar con el an�lisis
    procesar_simple(Tail, Lista, Fin),!.

formar_oracion(Oraciones, Formadas):-                                                                                %Llama a metodo auxiliar
    formar_oracion_aux(Oraciones, [], Formadas).

formar_oracion_aux([ ], Fin, Fin).
                                                                                                                     %Si la oraci�n tiene tanto sujeto como predicado (no hay sujeto omitido)
formar_oracion_aux([Head | Tail], Lista, Fin):-
    o(Suj, Predicado) = Head,
    separar_subordinada(Suj, Resultado),                                                                             %M�todo que si dentro de una oraci�n (dentro del gn) hay una subordinada la saca y devuelve
    nth1(2, Resultado, Elem),                                                                                        %Si el segundo elemento devuelvo del metodo anterior es vacio es que no hay oracion subordinada
    (Elem = [] ->
    (formar_oracion_aux(Tail, [Head | Lista], Fin),!)                                                                %Llamada recursiva
    ;
    (nth1(1, Resultado, S),                                                                                          %Si no vacio hay subordinada guardandola en la lista de oraciones simples
    Nueva = o(S, Predicado),
    formar_oracion_aux(Tail, [Nueva, Elem], Fin),!)).                                                                %Llamada recursiva

formar_oracion_aux([Head | Tail], Lista, Fin):-                                                                      %Si la oraci�n solo tiene predicado (sujeto omitido) se obtiene el sujeto de la oraci�n simple anterior de la lista de oraciones y se le pone  a la actual
    o(Predicado) = Head,
    nth1(1, Lista, Elem),
    o(Suj, _) = Elem,
    Nueva = o(Suj, Predicado),                                                                                       %Oraci�n simple incluyendo el sujeto
    formar_oracion_aux(Tail, [Nueva| Lista], Fin),!.                                                                 %Llamada recursiva guardando la nueva oraci�n simple generada

separar_subordinada(Lista, Salida):-                                                                                 %M�todo que obtiene la oraci�ns ubordinada dentro de una simple
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
    Sujeto =.. [gn | Aux2],                                                                                          %Obtiene sujeto de la oraci�n (etiqeuta gn)
    Subordinada =.. [_ | Args],                                                                                      %Obtiene la oraci�n subordinada
    anadir(Args, Sujeto, [], Salida),                                                                                %Llama a metodo a�adir
    Final = [Sujeto, Salida].

anadir([Head | Tail], Sujeto, Aux, Final):-
    Head =.. [Etiqueta | _],                                                                                         %Saca la eqiqueta, si es nexo (etiqueta nx) de la oraci�n subordinada se quita de la llamada recursiva
    (Etiqueta = nx ->
    anadir(Tail, Sujeto, Aux, Final),!;                                                                              %Si no hay nexo no quita la cabeza de la lista manteniendolo en la llamada recursiva
    anadir(Tail, Sujeto, [Head | Aux], Final),!
    ).

anadir([ ], _, [ ], Final):-                                                                                         %Si oracion pasada y Aux es vacia devuelve otra vacia
    Final = [].

anadir([ ], Sujeto, Aux, Final):-                                                                                    %Si solo la oracion pasada es vacia pero Aux no
    reverse(Aux, Aux2),                                                                                              %Da la vuelta a Aux ya que en pasos anteriores se ha invertido
    length(Aux2, Tam),
    (Tam = 1 ->                                                                                                      %Si el tama�o de Aux es 1 llamamos a quitar_par
    (quitar_par(Aux2, Aux3),
    Final = o(Sujeto, Aux3));
    Final =.. [o | Aux2]).

quitar_par([Con], Sin):-                                                                                             %Convierte oracion simple como cola a un compound
    Sin = Con.


%-------------------------------------------------------------------------------
%Metodos para limpiar oraciones quitando par�ntesis y etiquetas
%-------------------------------------------------------------------------------
limpiar_oraciones([ ], Final, Final).                                                                                %Caso de parada

limpiar_oraciones([Head | Tail], Final, Aux):-
    limpiar_oracion(Head, Resultado),                                                                                %Llama a m�todo limpiarOracion que elimina etiqeutas de la cabeza de la lista (primer elemento de la oraci�n)
    limpiar_oraciones(Tail, [Resultado | Final], Aux).
                                                                                                                     %Llamada recursiva para limpiar el resto de la oracion
limpiar_oracion(Oracion, Cadena):-
    term_to_atom(Oracion, Atom),                                                                                     %Limpia elemento pasado de la orac�n pasandolo a tipo Atom
    atomic_list_concat(Atoms, ',', Atom),
    maplist(atom_string, Atoms, Lista),
    procesar_elementos(Lista, [], Resultado),                                                                        %Llama a m�todo procesar_elementos que devuelve lista limpia sin etiquetas ni parentesis
    atomic_list_concat(Resultado, ' ', Cad),
    Cadena = Cad.                                                                                                    %Se concatena resultado de la enterior(lista ya limpia)

procesar_elementos([ ], Final, Final).                                                                               %Caso base

procesar_elementos([Head|Tail], Final, Aux):-
    split_string(Head, "(", "", Lista),                                                                              %Separa la lista por (
    length(Lista, Tam),                                                                                              %Saca el tama�o de la lista separada
    nth1(Tam, Lista, Elem),                                                                                          %Guarda en Elem ultimo elemento de la lista separada
    eliminar_caracter(Elem, ')', Resultado),                                                                         %Llama a metodo eliminar caracteres para devolver elemento pasado sin par�ntesis
    eliminar_caracter(Resultado, '\'', Resultado2),                                                                  %Llama a metodo eliminar caracteres para devolver elemento pasado sin comillas simples
    append(Final, [Resultado2], ListaFinal),                                                                         %Junta elemento limpio con lista Final
    procesar_elementos(Tail, ListaFinal, Aux).                                                                       %Llamada recursiva para limpiar siguiente elemento

eliminar_caracter(Cadena, Caracter, NuevaCadena):-                                                                   %M�todo que elimina de una cadena pasada el caracter indicado
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
    
