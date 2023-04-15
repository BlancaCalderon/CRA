
%_______________________________________________________________________________
%_______________________________________________________________________________
%                              Gramatica
%_______________________________________________________________________________
%_______________________________________________________________________________

%Oraciones generales
%-------------------------------------------------------------------------------
oracion(O) --> oracion_simple(O).
oracion(O) --> oracion_coordinada(O).
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

%-------------------------------------------------------------------------------
%Definición de las oraciones subordinadas (pueden empeza con un nexo)
%-------------------------------------------------------------------------------
oracion_subordinada(or(GN, GV)) --> g_nominal(GN), g_verbal(GV).
oracion_subordinada(or(NX, GV)) --> nexo(NX), g_verbal(GV).
oracion_subordinada(or(NX, GN, GV)) --> nexo(NX), g_nominal(GN), g_verbal(GV).

%-------------------------------------------------------------------------------
%Definición de las oraciones compuestas (union de varias oraciones coordinadas, de dos o mas simples o de coordinadas y simples)
%-------------------------------------------------------------------------------
%oracion_compuesta(ocm(OS1, Conj, OS2)) --> oracion_simple(OS1), conjuncion(Conj), oracion_simple(OS2).

oracion_compuesta(ocm(OC, Conj, OS)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OC1, Conj, OC2)) --> oracion_coordinada(OC1), conjuncion(Conj), oracion_coordinada(OC2).
oracion_compuesta(ocm(OC, Conj, OCM)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_compuesta(OCM).
oracion_compuesta(ocm(OS, Conj, OC)) --> oracion_simple(OS), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, Conj, OCM)) --> oracion_simple(OS), conjuncion(Conj), oracion_compuesta(OCM).

%oracion_compuesta(ocm(OS1, OS2)) --> oracion_simple(OS1), oracion_simple(OS2).
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
g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(N, GN)) --> nombre(N), g_nominal(GN).
g_nominal(gn(NP)) --> nombre_propio(NP).
g_nominal(gn(NP, OR)) --> nombre_propio(NP), oracion_subordinada(OR).                                               %Dentro de un grupo nominal puede haber una oracion subordinada
g_nominal(gn(NP, GN)) --> nombre_propio(NP), g_nominal(GN).
g_nominal(gn(GAdv, N)) --> g_adverbial(GAdv), nombre(N).                                                            %Un grupo nominal puede ser acompañado por un adverbio
g_nominal(gn(GAdv, NP)) --> g_adverbial(GAdv), nombre_propio(NP).
g_nominal(gn(Det, N)) --> determinante(Det), nombre(N).
g_nominal(gn(Det, N, OR)) --> determinante(Det), nombre(N), oracion_subordinada(OR).
g_nominal(gn(Det, N, GN)) --> determinante(Det), nombre(N), g_nominal(GN).
g_nominal(gn(Det, GAdj, N)) --> determinante(Det), g_adjetival(GAdj), nombre(N).                                    %Un grupo nominal puede ser acompañado por un adjetivo
g_nominal(gn(Det, GAdv, N)) --> determinante(Det), g_adverbial(GAdv), nombre(N).
%g_nominal(gn(Det, GAdv, GAdj, N)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), nombre(N).
%g_nominal(gn(Det, GAdj, GN)) --> determinante(Det), g_adjetival(GAdj), g_nominal(GN).
g_nominal(gn(Det, GAdv, GN)) --> determinante(Det), g_adverbial(GAdv), g_nominal(GN).
%g_nominal(gn(Det, GAdv, GAdj, GN)) --> determinante(Det), g_adverbial(GAdv), g_adjetival(GAdj), g_nominal(GN).
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
%g_verbal(gv(V, GV)) --> verbo(V), g_verbal(GV).
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
   obtener_oracion(Oracion, 1, [], LSucia),                                     %Metodo base que permite sacar las oraciones simples de una dada manteniendo las etiquetas y paréntesis
   limpiar_oraciones(LSucia, [], L).                                            %Método que elimina de las oraciones obtenias en obtener_oraciones las etiquetas y las oraciones presentando las oraciones simples limpias

obtener_oracion(_, 0, Fin, Fin).                                                %Caso de parada cuando se ha terminado de recorrer la oración

obtener_oracion(Oracion, 1, Lista, Final):-                                     %Recorre la oración dada separándo las oraciones y poniendo sujeto correspondiente si es una oracion con sujeto omitido
    X = Oracion,
    (
    o(Sujeto, Predicado) = X,
    term_to_atom(Sujeto, Atom),
    atomic_list_concat(Atoms, 'or(', Atom),
    length(Atoms, Continuar),
    
    (Continuar is 2 ->
    (nth1(1, Atoms, S1),
    nth1(2, Atoms, S2),
    term_to_atom(Predicado, Atomo2),
    atomic_list_concat([S1, Atomo2], ' ', Cad),
    Y = [S2 | [Cad | Lista]],
    obtener_oracion(Oracion, 0, Y, Final),!)
    ;
    (Y = [X | Lista],
    obtener_oracion(Oracion, 0, Y, Final),!))
    ;
    o(Predicado) = X,
    nth1(1, Lista, Elem),
    Z = Elem,
    o(Sujeto, _) = Z,
    Y = [o(Sujeto, Predicado) | Lista],
    obtener_oracion(Oracion, 0, Y, Final),!
    ).

obtener_oracion(Oracion, 1, Lista, Final):-                                     %Caso se si es una oracion coordinada
    X = Oracion,
    oc(Simple, _, Simple2) = X,                                                 %Se obtienen las dos oraciones simples que forman la coordenada y se analizan para ver si tienen mas oraciones dentro
    obtener_oracion(Simple, 1, Lista, Lista1),!,
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.

obtener_oracion(Oracion, 1, Lista, Final):-                                     %Caso si es una oracion compuesta
    X = Oracion,
    (
    ocm(Simple, Simple2) = X
    ;
    ocm(Simple, _, Simple2) = X
    ),
    obtener_oracion(Simple, 1, Lista, Lista1),!,                                %Separa las oraciones que forman la compuesta en dos volviendo a analizarlas para ver si hay mas oraciones simples dentro
    obtener_oracion(Simple2, 1, Lista1, Lista2),!,
    obtener_oracion(Oracion, 0, Lista2, Final),!.

%-------------------------------------------------------------------------------
%Metodos para limpiar oraciones
%-------------------------------------------------------------------------------
limpiar_oraciones([ ], Final, Final).                                           %Caso para terminar

limpiar_oraciones([Head | Tail], Final, Aux):-                                  %Limpia la cabeza de la lista
    limpiar_oracion(Head, Resultado),                                           %Llamada recursiva para limpiar el resto de la lista
    limpiar_oraciones(Tail, [Resultado | Final], Aux).

limpiar_oracion(Oracion, Cadena):-
    term_to_atom(Oracion, Atom),
    atomic_list_concat(Atoms, ',', Atom),
    maplist(atom_string, Atoms, Lista),                                         %Convierte lista en una String para su procesamiento
    procesar_elementos(Lista, [], Resultado),                                   %Llama a metodo para eliminar los elementos que no nos interesan (etiquetas y parentesis)
    atomic_list_concat(Resultado, ' ', Cad),
    Cadena = Cad.

procesar_elementos([ ], Final, Final).                                          %Condicion de parada

procesar_elementos([Head|Tail], Final, Aux):-                                   %Metodo que procesa string para ver si tiene los elementos que no nos interesan (etiquetas y paréntesis)
    split_string(Head, "(", "", Lista),                                         %Separa cuando hay un parentesis
    length(Lista, Tam),
    nth1(Tam, Lista, Elem),
    nth1(1, Lista, Condicion),
    (Condicion = "'nx" ->                                                       %Si es una subordinada y contiene un nexo lo eliminamos
    procesar_elementos(Tail, Final, Aux)
    ;
    (eliminar_caracter(Elem, ')', Resultado),                                   %Llama a metodo para eliminar etiquetas
    eliminar_caracter(Resultado, '\'', Resultado2),
    append(Final, [Resultado2], ListaFinal),
    procesar_elementos(Tail, ListaFinal, Aux)
    )).


eliminar_caracter(Cadena, Caracter, NuevaCadena):-                              %De la cadena pasada elimina de la cadena mandada el elemnto indicado por Caracter
    atom_chars(Cadena, ListaCaracteres),
    delete(ListaCaracteres, Caracter, ListaSinCaracter),
    atom_chars(NuevaCadena, ListaSinCaracter).

