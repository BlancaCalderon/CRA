%Consulta: oracion(eng,X,[the,man,eats],[]), oracion(esp,X,Y,[]).
traducir(Idm1, Idm2, Lista1):- oracion(Idm1, Oracion, Lista1, []), oracion(Idm2, Oracion, Traduccion, []), write(Traduccion).

%------------------ INICIO GRAMATICA ESPAÑOL(esp) / INGLES(ing) --------------------
oracion(esp, O) --> oracion_simple(esp, O).
oracion(esp, O) --> oracion_compuesta(esp, O).

oracion(ing, O) --> oracion_simple(ing, O).
oracion(ing, O) --> oracion_compuesta(ing, O).
%-------------------------------------------------------------------------------------

%---------------------------- ORACION SIMPLE -----------------------------------------------
oracion_simple(esp, o(GN, GV)) --> g_nominal(esp,GN), g_verbal(esp,GV).
oracion_simple(esp, o(GV, GN, Sig)) --> g_verbal(esp,GV), g_nominal(esp,GN), signo(Sig).
oracion_simple(esp, o(GV)) --> g_verbal(esp,GV).

oracion_simple(ing, o(GN, GV)) --> g_nominal(ing,GN), g_verbal(ing,GV).
oracion_simple(ing, o(GV, GN, Sig)) --> g_verbal(ing,GV), g_nominal(ing,GN), signo(Sig).
oracion_simple(ing, o(GV)) --> g_verbal(ing,GV).
%------------------------------------------------------------------------------------

%---------------------------- ORACION COORDINADA -----------------------------------------------
oracion_coordinada(esp, oc(O1, Conj, O2)) --> oracion_simple(esp, O1), conjuncion(esp, Conj), oracion_simple(esp, O2).
oracion_coordinada(esp, oc(O1, Conj, O2, Conj2, O3)) --> oracion_simple(esp, O1), conjuncion(esp, Conj), oracion_simple(esp, O2), conjuncion(esp, Conj2), oracion_simple(esp, O3).
oracion_coordinada(esp, oc(O1, Conj, O2, Conj2, O3, Conj3, O4)) --> oracion_simple(esp, O1), conjuncion(esp, Conj), oracion_simple(esp, O2), conjuncion(esp, Conj2), oracion_simple(esp, O3), conjuncion(esp, Conj3), oracion_simple(esp, O4).
oracion_coordinada(esp, oc(O1, Conj, O2, Conj2, O3, Conj3, O4, Conj4, O5)) --> oracion_simple(esp, O1), conjuncion(esp, Conj), oracion_simple(esp, O2), conjuncion(esp, Conj2), oracion_simple(esp, O3), conjuncion(esp, Conj3), oracion_simple(esp, O4), conjuncion(esp, Conj4), oracion_simple(esp, O5).

oracion_coordinada(ing, oc(O1, Conj, O2)) --> oracion_simple(ing, O1), conjuncion(ing, Conj), oracion_simple(ing, O2).
oracion_coordinada(ing, oc(O1, Conj, O2, Conj2, O3)) --> oracion_simple(ing, O1), conjuncion(ing, Conj), oracion_simple(ing, O2), conjuncion(ing, Conj2), oracion_simple(ing, O3).
oracion_coordinada(ing, oc(O1, Conj, O2, Conj2, O3, Conj3, O4)) --> oracion_simple(ing, O1), conjuncion(ing, Conj), oracion_simple(ing, O2), conjuncion(ing, Conj2), oracion_simple(ing, O3), conjuncion(ing, Conj3), oracion_simple(ing, O4).
oracion_coordinada(ing, oc(O1, Conj, O2, Conj2, O3, Conj3, O4, Conj4, O5)) --> oracion_simple(ing, O1), conjuncion(ing, Conj), oracion_simple(ing, O2), conjuncion(ing, Conj2), oracion_simple(ing, O3), conjuncion(ing, Conj3), oracion_simple(ing, O4), conjuncion(ing, Conj4), oracion_simple(ing, O5).
%------------------------------------------------------------------------------------

%---------------------------- ORACION SUBORDINADA -----------------------------------------------
oracion_simple_sub(esp, o(GN, GV)) --> g_nominal_sub(esp, GN), g_verbal(esp, GV).
oracion_simple_sub(ing, o(GN, GV)) --> g_nominal_sub(ing, GN), g_verbal(ing, GV).

oracion_subordinada(esp, or(GN, GV)) --> g_nominal(esp, GN), g_verbal(esp, GV).
oracion_subordinada(esp, or(NX, GV)) --> nexo(esp, NX), g_verbal(esp, GV).
oracion_subordinada(esp, or(NX, GN, GV)) --> nexo(esp, NX), g_nominal(esp, GN), g_verbal(esp, GV).


oracion_subordinada(ing, or(GN, GV)) --> g_nominal(ing, GN), g_verbal(ing, GV).
oracion_subordinada(ing, or(NX, GV)) --> nexo(ing, NX), g_verbal(ing, GV).
oracion_subordinada(ing, or(NX, GN, GV)) --> nexo(ing, NX), g_nominal(ing, GN), g_verbal(ing, GV).
%------------------------------------------------------------------------------------


%---------------------------- ORACION COMPUESTA -----------------------------------------------
oracion_compuesta(esp, ocm(OC)) --> oracion_coordinada(esp, OC).
oracion_compuesta(esp, ocm(OR)) --> oracion_simple_sub(esp, OR).

oracion_compuesta(ing, ocm(OC)) --> oracion_coordinada(ing, OC).
oracion_compuesta(ing, ocm(OR)) --> oracion_simple_sub(ing, OR).
%------------------------------------------------------------------------------------

%---------------------------- GRUPOS NOMINALES -----------------------------------------------
g_nominal_sub(esp, gn(NP, OR)) --> nombre_propio(esp, NP), oracion_subordinada(esp, OR).
g_nominal_sub(esp, gn(GN, OR)) --> g_nominal(esp, GN), oracion_subordinada(esp, OR).

g_nominal_sub(ing, gn(NP, OR)) --> nombre_propio(ing, NP), oracion_subordinada(ing, OR).
g_nominal_sub(ing, gn(GN, OR)) --> g_nominal(ing, GN), oracion_subordinada(ing, OR).


g_nominal(esp, gn(N)) --> nombre(esp, N).
g_nominal(esp, gn(N, GN)) --> nombre(esp, N), g_nominal(esp, GN).
g_nominal(esp, gn(NP)) --> nombre_propio(esp, NP).
g_nominal(esp, gn(NP, GN)) --> nombre_propio(esp, NP), g_nominal(esp, GN).
g_nominal(esp, gn(GAdv, N)) --> g_adverbial(esp, GAdv), nombre(esp, N).                                                            %Un grupo nominal puede ser acompañado por un adverbio
g_nominal(esp, gn(GAdv, NP)) --> g_adverbial(esp, GAdv), nombre_propio(esp, NP).
g_nominal(esp, gn(Det, N)) --> determinante(esp, Det), nombre(esp, N).
g_nominal(esp, gn(Det, N, GN)) --> determinante(esp, Det), nombre(esp, N), g_nominal(esp, GN).
g_nominal(esp, gn(Det, GAdj, N)) --> determinante(esp, Det), g_adjetival(esp, GAdj), nombre(esp, N).                                    %Un grupo nominal puede ser acompañado por un adjetivo
g_nominal(esp, gn(Det, GAdv, N)) --> determinante(esp, Det), g_adverbial(esp, GAdv), nombre(esp, N).
g_nominal(esp, gn(Det, GAdv, GN)) --> determinante(esp, Det), g_adverbial(esp, GAdv), g_nominal(esp, GN).
g_nominal(esp, gn(GAdj, N)) --> g_adjetival(esp, GAdj), nombre(esp, N).
g_nominal(esp, gn(GAdj, NP)) --> g_adjetival(esp, GAdj), nombre_propio(esp, NP).
g_nominal(esp, gn(GAdv, GAdj, N)) --> g_adverbial(esp, GAdv), g_adjetival(esp, GAdj), nombre(esp, N).
g_nominal(esp, gn(GAdv, GAdj, NP)) --> g_adverbial(esp, GAdv), g_adjetival(esp, GAdj), nombre_propio(esp, NP).
g_nominal(esp, gn(GPrep, GN)) --> g_preposicional(esp, GPrep), g_nominal(esp, GN).

g_nominal(ing, gn(N)) --> nombre(ing, N).
g_nominal(ing, gn(N, GN)) --> nombre(ing, N), g_nominal(ing, GN).
g_nominal(ing, gn(NP)) --> nombre_propio(ing, NP).
g_nominal(ing, gn(NP, GN)) --> nombre_propio(ing, NP), g_nominal(ing, GN).
g_nominal(ing, gn(GAdv, N)) --> g_adverbial(ing, GAdv), nombre(ing, N).                                                            %Un grupo nominal puede ser acompañado por un adverbio
g_nominal(ing, gn(GAdv, NP)) --> g_adverbial(ing, GAdv), nombre_propio(ing, NP).
g_nominal(ing, gn(Det, N)) --> determinante(ing, Det), nombre(ing, N).
g_nominal(ing, gn(Det, N, GN)) --> determinante(ing, Det), nombre(ing, N), g_nominal(ing, GN).
g_nominal(ing, gn(Det, GAdj, N)) --> determinante(ing, Det), g_adjetival(ing, GAdj), nombre(ing, N).                                    %Un grupo nominal puede ser acompañado por un adjetivo
g_nominal(ing, gn(Det, GAdv, N)) --> determinante(ing, Det), g_adverbial(ing, GAdv), nombre(ing, N).
g_nominal(ing, gn(Det, GAdv, GN)) --> determinante(ing, Det), g_adverbial(ing, GAdv), g_nominal(ing, GN).
g_nominal(ing, gn(GAdj, N)) --> g_adjetival(ing, GAdj), nombre(ing, N).
g_nominal(ing, gn(GAdj, NP)) --> g_adjetival(ing, GAdj), nombre_propio(ing, NP).
g_nominal(ing, gn(GAdv, GAdj, N)) --> g_adverbial(ing, GAdv), g_adjetival(ing, GAdj), nombre(ing, N).
g_nominal(ing, gn(GAdv, GAdj, NP)) --> g_adverbial(ing, GAdv), g_adjetival(ing, GAdj), nombre_propio(ing, NP).
g_nominal(ing, gn(GPrep, GN)) --> g_preposicional(ing, GPrep), g_nominal(ing, GN).


g_nominal(esp, gn(N, Conj, GN)) --> nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).                                           %Puede haber doble sujeto (separado por y)
g_nominal(esp, gn(NP, Conj, GN)) --> nombre_propio(esp, NP), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GAdv, N, Conj, GN)) --> g_adverbial(esp, GAdv), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GAdv, NP, Conj, GN)) --> g_adverbial(esp, GAdv), nombre_propio(esp, NP), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(Det, N, Conj, GN)) --> determinante(esp, Det), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(Det, GAdj, N, Conj, GN)) --> determinante(esp, Det), g_adjetival(esp, GAdj), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(Det, GAdv, N, Conj, GN)) --> determinante(esp, Det), g_adverbial(esp, GAdv), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(Det, GAdv, GAdj, N, Conj, GN)) --> determinante(esp, Det), g_adverbial(esp, GAdv), g_adjetival(esp, GAdj), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GAdj, N, Conj, GN)) --> g_adjetival(esp, GAdj), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GAdj, NP, Conj, GN)) --> g_adjetival(esp, GAdj), nombre_propio(esp, NP), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GAdv, GAdj, N, Conj, GN)) --> g_adverbial(esp, GAdv), g_adjetival(esp, GAdj), nombre(esp, N), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GAdv, GAdj, NP, Conj, GN)) --> g_adverbial(esp, GAdv), g_adjetival(esp, GAdj), nombre_propio(esp, NP), conjuncion(esp, Conj), g_nominal(esp, GN).
g_nominal(esp, gn(GPrep, GN1, Conj, GN2)) --> g_preposicional(esp, GPrep), g_nominal(esp, GN1), conjuncion(esp, Conj), g_nominal(esp, GN2).

g_nominal(ing, gn(N, Conj, GN)) --> nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).                                           %Puede haber doble sujeto (separado por y)
g_nominal(ing, gn(NP, Conj, GN)) --> nombre_propio(ing, NP), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GAdv, N, Conj, GN)) --> g_adverbial(ing, GAdv), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GAdv, NP, Conj, GN)) --> g_adverbial(ing, GAdv), nombre_propio(ing, NP), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(Det, N, Conj, GN)) --> determinante(ing, Det), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(Det, GAdj, N, Conj, GN)) --> determinante(ing, Det), g_adjetival(ing, GAdj), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(Det, GAdv, N, Conj, GN)) --> determinante(ing, Det), g_adverbial(ing, GAdv), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(Det, GAdv, GAdj, N, Conj, GN)) --> determinante(ing, Det), g_adverbial(ing, GAdv), g_adjetival(ing, GAdj), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GAdj, N, Conj, GN)) --> g_adjetival(ing, GAdj), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GAdj, NP, Conj, GN)) --> g_adjetival(ing, GAdj), nombre_propio(ing, NP), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GAdv, GAdj, N, Conj, GN)) --> g_adverbial(ing, GAdv), g_adjetival(ing, GAdj), nombre(ing, N), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GAdv, GAdj, NP, Conj, GN)) --> g_adverbial(ing, GAdv), g_adjetival(ing, GAdj), nombre_propio(ing, NP), conjuncion(ing, Conj), g_nominal(ing, GN).
g_nominal(ing, gn(GPrep, GN1, Conj, GN2)) --> g_preposicional(ing, GPrep), g_nominal(ing, GN1), conjuncion(ing, Conj), g_nominal(ing, GN2).
%------------------------------------------------------------------------------------

%---------------------------- GRUPOS VERBALES -----------------------------------------------
g_verbal(esp, gv(V)) --> verbo(esp, V).
g_verbal(esp, gv(V, GN)) --> verbo(esp, V), g_nominal(esp, GN).                                                                     %Un verbo puede estar acompañado por un grupo adjetival
g_verbal(esp, gv(V, GAdj)) --> verbo(esp, V), g_adjetival(esp, GAdj).                                                               %Un verbo puede estar acompañado por un grupo adverbial
g_verbal(esp, gv(V, GAdv)) --> verbo(esp, V), g_adverbial(esp, GAdv).                                                               %Un verbo puede estar acompañado por un grupo preposicional
g_verbal(esp, gv(V, GPrep)) --> verbo(esp, V), g_preposicional(esp, GPrep).
g_verbal(esp, gv(GAdv, GV)) --> g_adverbial(esp, GAdv), g_verbal(esp, GV).
g_verbal(esp, gv(GPrep, GV)) --> g_preposicional(esp, GPrep), g_verbal(esp, GV).
g_verbal(esp, gv(V, GAdj, GPrep)) --> verbo(esp, V), g_adjetival(esp, GAdj), g_preposicional(esp, GPrep).
g_verbal(esp, gv(V, GAdv, GAdj)) --> verbo(esp, V), g_adverbial(esp, GAdv), g_adjetival(esp, GAdj).
g_verbal(esp, gv(V, GV, GPrep)) --> verbo(esp, V), g_verbal(esp, GV), g_preposicional(esp, GPrep).
g_verbal(esp, gv(V, GN, GPrep)) --> verbo(esp, V), g_nominal(esp, GN), g_preposicional(esp, GPrep).
g_verbal(esp, gv(GAdv, GV, GPrep)) --> g_adverbial(esp, GAdv), g_verbal(esp, GV), g_preposicional(esp, GPrep).
g_verbal(esp, gv(GPrep1, GV, GPrep2)) --> g_preposicional(esp, GPrep1), g_verbal(esp, GV), g_preposicional(esp, GPrep2).

g_verbal(ing, gv(V)) --> verbo(ing, V).
g_verbal(ing, gv(V, GN)) --> verbo(ing, V), g_nominal(ing, GN).                                                                     %Un verbo puede estar acompañado por un grupo adjetival
g_verbal(ing, gv(V, GAdj)) --> verbo(ing, V), g_adjetival(ing, GAdj).                                                               %Un verbo puede estar acompañado por un grupo adverbial
g_verbal(ing, gv(V, GAdv)) --> verbo(ing, V), g_adverbial(ing, GAdv).                                                               %Un verbo puede estar acompañado por un grupo preposicional
g_verbal(ing, gv(V, GPrep)) --> verbo(ing, V), g_preposicional(ing, GPrep).
g_verbal(ing, gv(GAdv, GV)) --> g_adverbial(ing, GAdv), g_verbal(ing, GV).
g_verbal(ing, gv(GPrep, GV)) --> g_preposicional(ing, GPrep), g_verbal(ing, GV).
g_verbal(ing, gv(V, GAdj, GPrep)) --> verbo(ing, V), g_adjetival(ing, GAdj), g_preposicional(ing, GPrep).
g_verbal(ing, gv(V, GAdv, GAdj)) --> verbo(ing, V), g_adverbial(ing, GAdv), g_adjetival(ing, GAdj).
g_verbal(ing, gv(V, GV, GPrep)) --> verbo(ing, V), g_verbal(ing, GV), g_preposicional(ing, GPrep).
g_verbal(ing, gv(V, GN, GPrep)) --> verbo(ing, V), g_nominal(ing, GN), g_preposicional(ing, GPrep).
g_verbal(ing, gv(GAdv, GV, GPrep)) --> g_adverbial(ing, GAdv), g_verbal(ing, GV), g_preposicional(ing, GPrep).
g_verbal(ing, gv(GPrep1, GV, GPrep2)) --> g_preposicional(ing, GPrep1), g_verbal(ing, GV), g_preposicional(ing, GPrep2).
%------------------------------------------------------------------------------------

%---------------------------- GRUPOS ADJETIVALES -----------------------------------------------
g_adjetival(esp, gadj(Adj)) --> adjetivo(esp, Adj).
g_adjetival(esp, gadj(GAdv, Adj)) --> g_adverbial(esp, GAdv), adjetivo(esp, Adj).
g_adjetival(esp, gadj(Adj, Conj, GAdj)) --> adjetivo(esp, Adj), conjuncion(esp, Conj), g_adjetival(esp, GAdj).

g_adjetival(ing, gadj(Adj)) --> adjetivo(ing, Adj).
g_adjetival(ing, gadj(GAdv, Adj)) --> g_adverbial(ing, GAdv), adjetivo(ing, Adj).
g_adjetival(ing, gadj(Adj, Conj, GAdj)) --> adjetivo(ing, Adj), conjuncion(ing, Conj), g_adjetival(ing, GAdj).
%------------------------------------------------------------------------------------

%---------------------------- GRUPOS ADVERBIALES -----------------------------------------------
g_adverbial(esp, gadv(Adv)) --> adverbio(esp, Adv).
g_adverbial(esp, gadv(Adv, GAdv)) --> adverbio(esp, Adv), g_adverbial(esp, GAdv).

g_adverbial(ing, gadv(Adv)) --> adverbio(ing, Adv).
g_adverbial(ing, gadv(Adv, GAdv)) --> adverbio(ing, Adv), g_adverbial(ing, GAdv).
%------------------------------------------------------------------------------------

%---------------------------- GRUPOS PREPOSICIONALES -----------------------------------------------
g_preposicional(esp, gp(Prep, GN)) -->  preposicion(esp, Prep), g_nominal(esp, GN).
g_preposicional(esp, gp(Prep, GN, GPrep)) -->  preposicion(esp, Prep), g_nominal(esp, GN), g_preposicional(esp, GPrep).

g_preposicional(ing, gp(Prep, GN)) -->  preposicion(ing, Prep), g_nominal(ing, GN).
g_preposicional(ing, gp(Prep, GN, GPrep)) -->  preposicion(ing, Prep), g_nominal(ing, GN), g_preposicional(ing, GPrep).
%------------------------------------------------------------------------------------

%---------------------------- GRAMATICA -----------------------------------------------
determinante(esp, det(X)) --> [X], {det(X,_)}.
determinante(ing, det(X)) --> [Y], {det(X,Y)}.

nombre(esp, n(X)) --> [X], {n(X,_)}.
nombre(ing, n(X)) --> [Y], {n(X,Y)}.

nombre_propio(esp, np(X)) --> [X], {np(X,_)}.
nombre_propio(ing, np(X)) --> [Y], {np(X,Y)}.

verbo(esp, v(X)) --> [X], {v(X,_)}.
verbo(ing, v(X)) --> [Y], {v(X,Y)}.

adjetivo(esp, adj(X)) --> [X], {adj(X,_)}.
adjetivo(ing, adj(X)) --> [Y], {adj(X,Y)}.

adverbio(esp, adv(X)) --> [X], {adv(X,_)}.
adverbio(ing, adv(X)) --> [Y], {adv(X,Y)}.

conjuncion(esp, conj(X)) --> [X], {conj(X,_)}.
conjuncion(ing, conj(X)) --> [Y], {conj(X,Y)}.

preposicion(esp, prep(X)) --> [X], {prep(X,_)}.
preposicion(ing, prep(X)) --> [Y], {prep(X,Y)}.

signo(esp, sig(X)) --> [X], {sig(X,_)}.
signo(ing, sig(X)) --> [Y], {sig(X,Y)}.

nexo(esp, nx(X)) --> [X], {nx(X,_)}.
nexo(ing, nx(X)) --> [Y], {nx(X,Y)}.
%------------------------------------------------------------------------------------

%TRADUCCION

%---------------------------- DETERMINANTES -----------------------------------------------
det('el', 'the').
det('la', 'the').
det('los', 'the').
det('las', 'the').
det('un', 'a').
det('una', 'a').
det('unos', 'some').
det('unas', 'some').
det('este', 'this').
det('esta', 'this').
det('estos', 'these').
det('estas', 'these').
det('ese', 'that').
det('esa', 'that').
det('esos', 'those').
det('esas', 'those').
det('aquel', 'that').
det('aquella', 'that').
det('aquellos', 'those').
det('aquellas', 'those').
det('mi', 'my').
det('mis', 'my').
det('tu', 'your').
det('tus', 'your').
det('su', 'his').
det('sus', 'his').
det('su', 'her').
det('sus', 'her').
det('su', 'its').
det('sus', 'its').
det('su', 'their').
det('sus', 'their').
det('su', 'your (formal)').
det('sus', 'your (formal)').
det('nuestro', 'our').
det('nuestra', 'our').
det('nuestros', 'our').
det('nuestras', 'our').
det('cualquier', 'any').
det('cualquieras', 'any').
det('alguno', 'some').
det('alguna', 'some').
det('algunos', 'some').
det('algunas', 'some').
det('ningún', 'none').
det('ninguna', 'none').
det('ningunos', 'none').
det('ningunas', 'none').
det('otro', 'other').
det('otra', 'other').
det('otros', 'other').
det('otras', 'other').
det('varios', 'several').
det('varias', 'several').
det('poco', 'little').
det('poca', 'little').
det('pocos', 'few').
det('pocas', 'few').
det('mucho', 'much').
det('mucha', 'much').
det('muchos', 'many').
det('muchas', 'many').
det('todo', 'all').
det('toda', 'all').
det('todos', 'all').
det('todas', 'all').
det('demás', 'others').
det('tanto', 'so much').
det('tanta', 'so much').
det('tantos', 'so many').
det('tantas', 'so many').
det('cada', 'each').

det('the', 'el').
det('the', 'la').
det('the', 'los').
det('the', 'las').
det('a', 'un').
det('a', 'una').
det('some', 'unos').
det('some', 'unas').
det('this', 'este').
det('this', 'esta').
det('these', 'estos').
det('these', 'estas').
det('that', 'ese').
det('that', 'esa').
det('those', 'esos').
det('those', 'esas').
det('that', 'aquel').
det('that', 'aquella').
det('those', 'aquellos').
det('those', 'aquellas').
det('my', 'mi').
det('my', 'mis').
det('your', 'tu').
det('your', 'tus').
det('his', 'su').
det('his', 'sus').
det('her', 'su').
det('her', 'sus').
det('its', 'su').
det('its', 'sus').
det('their', 'su').
det('their', 'sus').
det('your', 'su').
det('your', 'sus').
det('our', 'nuestro').
det('our', 'nuestra').
det('our', 'nuestros').
det('our', 'nuestras').
det('any', 'cualquier').
det('any', 'cualquiera').
det('some', 'alguno').
det('some', 'alguna').
det('some', 'algunos').
det('some', 'algunas').
det('none', 'ningún').
det('none', 'ninguna').
det('none', 'ningunos').
det('none', 'ningunas').
det('other', 'otro').
det('other', 'otra').
det('other', 'otros').
det('other', 'otras').
det('several', 'varios').
det('several', 'varias').
det('little', 'poco').
det('little', 'poca').
det('few', 'pocos').
det('few', 'pocas').
det('much', 'mucho').
det('much', 'mucha').
det('many', 'muchos').
det('many', 'muchas').
det('all', 'todo').
det('all', 'toda').
det('all', 'todos').
det('all', 'todas').
det('others', 'demás').
det('so much', 'tanto').
det('so much', 'tanta').
det('so many', 'tantos').
det('so many', 'tantas').
det('each', 'cada').
%------------------------------------------------------------------------------------

%---------------------------- SUSTANTIVOS(NOMBRES) -----------------------------------------------
n('casa', 'house').
n('casas', 'houses').
n('persona', 'person').
n('personas', 'people').
n('día', 'day').
n('días', 'days').
n('año', 'year').
n('años', 'years').
n('cosa', 'thing').
n('cosas', 'things').
n('tiempo', 'time').
n('tiempos', 'times').
n('lugar', 'place').
n('lugares', 'places').
n('vida', 'life').
n('vidas', 'lives').
n('mano', 'hand').
n('manos', 'hands').
n('trabajo', 'work').
n('trabajos', 'jobs').
n('parte', 'part').
n('partes', 'parts').
n('momento', 'moment').
n('momentos', 'moments').
n('manera', 'way').
n('maneras', 'ways').
n('ojo', 'eye').
n('ojos', 'eyes').
n('país', 'country').
n('países', 'countries').
n('punto', 'point').
n('puntos', 'points').
n('forma', 'form').
n('formas', 'forms').
n('hijo', 'son').
n('color', 'color').
n('colores', 'colors').
n('hija', 'daughter').
n('hijas', 'daughters').
n('hijos', 'sons').
n('problema', 'problem').
n('problemas', 'problems').
n('grupo', 'group').
n('grupos', 'groups').
n('sistema', 'system').
n('sistemas', 'systems').
n('familia', 'family').
n('familias', 'families').
n('agua', 'water').
n('aguas', 'waters').
n('noche', 'night').
n('noches', 'nights').
n('padre', 'father').
n('padres', 'fathers').
n('gobierno', 'government').
n('gobiernos', 'governments').
n('empresa', 'company').
n('empresas', 'companies').
n('historia', 'history').
n('historias', 'histories').
n('idea', 'idea').
n('ideas', 'ideas').
n('guerra', 'war').
n('guerras', 'wars').
n('programa', 'program').
n('programas', 'programs').
n('mundo', 'world').
n('mundos', 'worlds').
n('caso', 'case').
n('casos', 'cases').
n('mujer', 'woman').
n('mujeres', 'women').
n('ciudad', 'city').
n('ciudades', 'cities').
n('nivel', 'level').
n('niveles', 'levels').
n('político', 'politician').
n('políticos', 'politicians').
n('cuerpo', 'body').
n('cuerpos', 'bodies').
n('ser', 'being').
n('seres', 'beings').
n('poder', 'power').
n('poderes', 'powers').
n('brazo', 'arm').
n('brazos', 'arms').
n('nombre', 'name').
n('nombres', 'names').
n('amigo', 'friend').
n('amigos', 'friends').
n('mes', 'month').
n('meses', 'months').
n('ciudadano', 'citizen').
n('ciudadanos', 'citizens').
n('equipo', 'team').
n('equipos', 'teams').
n('sistema', 'system').
n('sistemas', 'systems').
n('clase', 'class').
n('clases', 'classes').
n('realidad', 'reality').
n('realidades', 'realities').
n('proyecto', 'project').
n('proyectos', 'projects').
n('cambio', 'change').
n('cambios', 'changes').
n('resultado', 'result').
n('resultados', 'results').
n('proceso', 'process').
n('procesos', 'processes').
n('mercado', 'market').
n('mercados', 'markets').
n('papel', 'paper').
n('papeles', 'papers').
n('modelo', 'model').
n('modelos', 'models').
n('aire', 'air').
n('aires', 'airs').
n('número', 'number').
n('números', 'numbers').
n('hijo', 'son').
n('hijos', 'sons').
n('ley', 'law').
n('leyes', 'laws').
n('accionista', 'shareholder').
n('accionistas', 'shareholders').
n('valor', 'value').
n('valores', 'values').
n('grupo', 'group').
n('grupos', 'groups').
n('nivel', 'level').
n('niveles', 'levels').
n('área', 'area').
n('áreas', 'areas').
n('medio', 'medium').
n('medios', 'mediums').
n('centro', 'center').
n('centros', 'centers').
n('comunidad', 'community').
n('comunidades', 'communities').
n('sociedad', 'society').
n('sociedades', 'societies').
n('mercado', 'market').
n('mercados', 'markets').
n('técnica', 'technique').
n('técnicas', 'techniques').
n('zona', 'zone').
n('zonas', 'zones').
n('derecho', 'right').
n('derechos', 'rights').
n('izquierdo', 'left').
n('izquierdos', 'lefts').
n('área', 'area').
n('áreas', 'areas').
n('producto', 'product').
n('productos', 'products').
n('idea', 'idea').
n('ideas', 'ideas').
n('concepto', 'concept').
n('conceptos', 'concepts').
n('proyecto', 'project').
n('proyectos', 'projects').
n('servicio', 'service').
n('servicios', 'services').
n('información', 'information').
n('informaciones', 'informations').
n('objetivo', 'objective').
n('objetivos', 'objectives').
n('factor', 'factor').
n('factores', 'factors').
n('cliente', 'client').
n('clientes', 'clients').
n('sistema', 'system').
n('sistemas', 'systems').
n('proceso', 'process').
n('procesos', 'processes').
n('capacidad', 'capacity').
n('capacidades', 'capacities').
n('recurso', 'resource').
n('recursos', 'resources').
n('fundamento', 'foundation').
n('fundamentos', 'foundations').
n('principio', 'principle').
n('principios', 'principles').
n('norma', 'norm').
n('normas', 'norms').
n('estructura', 'structure').
n('estructuras', 'structures').
n('fuente', 'source').
n('fuentes', 'sources').
n('estrategia', 'strategy').
n('estrategias', 'strategies').
n('oportunidad', 'opportunity').
n('oportunidades', 'opportunities').
n('forma', 'form').
n('formas', 'forms').
n('resultado', 'result').
n('resultados', 'results').
n('relación', 'relationship').
n('relaciones', 'relationships').
n('imagen', 'image').
n('imágenes', 'images').
n('caso', 'case').
n('casos', 'cases').
n('plan', 'plan').
n('planes', 'plans').
n('principio', 'principle').
n('principios', 'principles').
n('perspectiva', 'perspective').
n('perspectivas', 'perspectives').
n('criterio', 'criterion').
n('criterios', 'criteria').
n('situación', 'situation').
n('situaciones', 'situations').
n('medio', 'medium').
n('medios', 'media').
n('interés', 'interest').
n('intereses', 'interests').
n('momento', 'moment').
n('momentos', 'moments').
n('derecho', 'law').
n('derechos', 'rights').
n('tarea', 'task').
n('tareas', 'tasks').
n('producto', 'product').
n('productos', 'products').
n('recurso', 'resource').
n('recursos', 'resources').
n('objetivo', 'objective').
n('objetivos', 'objectives').
n('necesidad', 'need').
n('necesidades', 'needs').
n('impacto', 'impact').
n('impactos', 'impacts').
n('instrumento', 'instrument').
n('instrumentos', 'instruments').
n('información', 'information').
n('informaciones', 'informations').
n('proceso', 'process').
n('procesos', 'processes').
n('cultura', 'culture').
n('culturas', 'cultures').
n('sector', 'sector').
n('sectores', 'sectors').
n('experiencia', 'experience').
n('experiencias', 'experiences').
n('objeto', 'object').
n('objetos', 'objects').
n('hombre', 'man').
n('hombres', 'men').

n('house', 'casa').
n('houses', 'casas').
n('person', 'persona').
n('people', 'personas').
n('day', 'día').
n('days', 'días').
n('year', 'año').
n('years', 'años').
n('thing', 'cosa').
n('things', 'cosas').
n('time', 'tiempo').
n('times', 'tiempos').
n('place', 'lugar').
n('places', 'lugares').
n('life', 'vida').
n('lives', 'vidas').
n('hand', 'mano').
n('hands', 'manos').
n('work', 'trabajo').
n('jobs', 'trabajos').
n('part', 'parte').
n('parts', 'partes').
n('moment', 'momento').
n('moments', 'momentos').
n('way', 'manera').
n('ways', 'maneras').
n('eye', 'ojo').
n('eyes', 'ojos').
n('country', 'país').
n('countries', 'países').
n('point', 'punto').
n('points', 'puntos').
n('form', 'forma').
n('forms', 'formas').
n('son', 'hijo').
n('color', 'color').
n('colors', 'colores').
n('daughter', 'hija').
n('daughters', 'hijas').
n('sons', 'hijos').
n('problem', 'problema').
n('problems', 'problemas').
n('group', 'grupo').
n('groups', 'grupos').
n('system', 'sistema').
n('systems', 'sistemas').
n('family', 'familia').
n('families', 'familias').
n('water', 'agua').
n('waters', 'aguas').
n('night', 'noche').
n('nights', 'noches').
n('father', 'padre').
n('fathers', 'padres').
n('government', 'gobierno').
n('governments', 'gobiernos').
n('company', 'empresa').
n('companies', 'empresas').
n('history', 'historia').
n('histories', 'historias').
n('idea', 'idea').
n('ideas', 'ideas').
n('war', 'guerra').
n('wars', 'guerras').
n('program', 'programa').
n('programs', 'programas').
n('world', 'mundo').
n('worlds', 'mundos').
n('case', 'caso').
n('cases', 'casos').
n('woman', 'mujer').
n('women', 'mujeres').
n('city', 'ciudad').
n('cities', 'ciudades').
n('level', 'nivel').
n('levels', 'niveles').
n('politician', 'político').
n('politicians', 'políticos').
n('body', 'cuerpo').
n('bodies', 'cuerpos').
n('being', 'ser').
n('beings', 'seres').
n('power', 'poder').
n('powers', 'poderes').
n('arm', 'brazo').
n('arms', 'brazos').
n('name', 'nombre').
n('names', 'nombres').
n('friend', 'amigo').
n('friends', 'amigos').
n('month', 'mes').
n('months', 'meses').
n('citizen', 'ciudadano').
n('citizens', 'ciudadanos').
n('team', 'equipo').
n('teams', 'equipos').
n('system', 'sistema').
n('systems', 'sistemas').
n('class', 'clase').
n('classes', 'clases').
n('reality', 'realidad').
n('realities', 'realidades').
n('project', 'proyecto').
n('projects', 'proyectos').
n('change', 'cambio').
n('changes', 'cambios').
n('result', 'resultado').
n('results', 'resultados').
n('process', 'proceso').
n('processes', 'procesos').
n('market', 'mercado').
n('markets', 'mercados').
n('paper', 'papel').
n('papers', 'papeles').
n('model', 'modelo').
n('models', 'modelos').
n('air', 'aire').
n('airs', 'aires').
n('number', 'número').
n('numbers', 'números').
n('son', 'hijo').
n('sons', 'hijos').
n('law', 'ley').
n('laws', 'leyes').
n('shareholder', 'accionista').
n('shareholders', 'accionistas').
n('value', 'valor').
n('values', 'valores').
n('group', 'grupo').
n('groups', 'grupos').
n('level', 'nivel').
n('levels', 'niveles').
n('area', 'área').
n('areas', 'áreas').
n('medium', 'medio').
n('mediums', 'medios').
n('center', 'centro').
n('centers', 'centros').
n('community', 'comunidad').
n('communities', 'comunidades').
n('society', 'sociedad').
n('societies', 'sociedades').
n('market', 'mercado').
n('markets', 'mercados').
n('technique', 'técnica').
n('techniques', 'técnicas').
n('zone', 'zona').
n('zones', 'zonas').
n('right', 'derecho').
n('rights', 'derechos').
n('left', 'izquierdo').
n('lefts', 'izquierdos').
n('area', 'área').
n('areas', 'áreas').
n('product', 'producto').
n('products', 'productos').
n('idea', 'idea').
n('ideas', 'ideas').
n('concept', 'concepto').
n('concepts', 'conceptos').
n('project', 'proyecto').
n('projects', 'proyectos').
n('service', 'servicio').
n('services', 'servicios').
n('information', 'información').
n('informations', 'informaciones').
n('objective', 'objetivo').
n('objectives', 'objetivos').
n('factor', 'factor').
n('factors', 'factores').
n('client', 'cliente').
n('clients', 'clientes').
n('system', 'sistema').
n('systems', 'sistemas').
n('process', 'proceso').
n('processes', 'procesos').
n('capacity', 'capacidad').
n('capacities', 'capacidades').
n('resource', 'recurso').
n('resources', 'recursos').
n('foundation', 'fundamento').
n('foundations', 'fundamentos').
n('principle', 'principio').
n('principles', 'principios').
n('norm', 'norma').
n('norms', 'normas').
n('structure', 'estructura').
n('structures', 'estructuras').
n('source', 'fuente').
n('sources', 'fuentes').
n('strategy', 'estrategia').
n('strategies', 'estrategias').
n('opportunity', 'oportunidad').
n('opportunities', 'oportunidades').
n('form', 'forma').
n('forms', 'formas').
n('result', 'resultado').
n('results', 'resultados').
n('relationship', 'relación').
n('relationships', 'relaciones').
n('image', 'imagen').
n('images', 'imágenes').
n('case', 'caso').
n('cases', 'casos').
n('plan', 'plan').
n('plans', 'planes').
n('principle', 'principio').
n('principles', 'principios').
n('perspective', 'perspectiva').
n('perspectives', 'perspectivas').
n('criterion', 'criterio').
n('criteria', 'criterios').
n('situation', 'situación').
n('situations', 'situaciones').
n('medium', 'medio').
n('media', 'medios').
n('interest', 'interés').
n('interests', 'intereses').
n('moment', 'momento').
n('moments', 'momentos').
n('law', 'derecho').
n('rights', 'derechos').
n('task', 'tarea').
n('tasks', 'tareas').
n('product', 'producto').
n('products', 'productos').
n('resource', 'recurso').
n('resources', 'recursos').
n('objective', 'objetivo').
n('objectives', 'objetivos').
n('need', 'necesidad').
n('needs', 'necesidades').
n('impact', 'impacto').
n('impacts', 'impactos').
n('instrument', 'instrumento').
n('instruments', 'instrumentos').
n('information', 'información').
n('informations', 'informaciones').
n('process', 'proceso').
n('processes', 'procesos').
n('culture', 'cultura').
n('cultures', 'culturas').
n('sector', 'sector').
n('sectors', 'sectores').
n('experience', 'experiencia').
n('experiences', 'experiencias').
n('object', 'objeto').
n('objects', 'objetos').
n('man', 'hombre').
n('men', 'hombres').
%------------------------------------------------------------------------------------

%---------------------------- VERBOS -----------------------------------------------
v('ser', 'be').
v('soy', 'am').
v('eres', 'are').
v('es', 'is').
v('somos', 'are').
v('sois', 'are').
v('son', 'are').
v('haber', 'have').
v('tengo', 'have').
v('tienes', 'have').
v('tiene', 'has').
v('tenemos', 'have').
v('teneis', 'have').
v('tienen', 'have').
v('estar', 'be').
v('estoy', 'am').
v('estás', 'are').
v('está', 'is').
v('estamos', 'are').
v('estáis', 'are').
v('están', 'are').
v('tener', 'have').
v('tengo', 'have').
v('tienes', 'have').
v('tiene', 'has').
v('tenemos', 'have').
v('tenéis', 'have').
v('tienen', 'have').
v('hacer', 'do').
v('hago', 'do').
v('haces', 'do').
v('hace', 'does').
v('hacemos', 'do').
v('hacéis', 'do').
v('hacen', 'do').
v('ir', 'to go').
v('voy', 'go').
v('vas', 'go').
v('va', 'goes').
v('vamos', 'go').
v('vais', 'go').
v('van', 'go').
v('ver', 'see').
v('veo', 'see').
v('ves', 'see').
v('ve', 'sees').
v('vemos', 'see').
v('veis', 'see').
v('ven', 'see').
v('saber', 'know').
v('sé', 'know').
v('sabes', 'know').
v('sabe', 'knows').
v('sabemos', 'know').
v('sabéis', 'know').
v('saben', 'know').
v('querer', 'love').
v('quiero', 'love').
v('quieres', 'love').
v('quiere', 'loves').
v('queremos', 'love').
v('queréis', 'love').
v('quieren', 'love').
v('poder', 'can').
v('puedo', 'can').
v('puedes', 'can').
v('puede', 'can').
v('podemos', 'can').
v('podéis', 'can').
v('pueden', 'can').
v('decir', 'say').
v('digo', 'say').
v('dices', 'say').
v('dice', 'says').
v('decimos', 'say').
v('decís', 'say').
v('dicen', 'say').
v('dar', 'give').
v('doy', 'give').
v('das', 'give').
v('da', 'gives').
v('damos', 'give').
v('dais', 'give').
v('dan', 'give').
v('venir', 'come').
v('vengo', 'come').
v('vienes', 'come').
v('viene', 'comes').
v('venimos', 'come').
v('venís', 'come').
v('vienen', 'come').
v('parecer', 'seem').
v('parezco', 'seem').
v('pareces', 'seem').
v('parece', 'seems').
v('parecemos', 'seem').
v('pareceis', 'seems').
v('parecen', 'seems').
v('creer', 'believe').
v('creo', 'believe').
v('crees', 'believe').
v('cree', 'believes').
v('creemos', 'believe').
v('creéis', 'believe').
v('creen', 'believe').
v('tomar', 'take').
v('tomo', 'take').
v('tomas', 'take').
v('toma', 'takes').
v('tomamos', 'take').
v('tomáis', 'take').
v('toman', 'take').
v('sentir', 'feel').
v('siento', 'feel').
v('sientes', 'feel').
v('siente', 'feels').
v('sentimos', 'feel').
v('sentís', 'feel').
v('sienten', 'feel').
v('vivir', 'live').
v('vivo', 'live').
v('vives', 'live').
v('vive', 'lives').
v('vivimos', 'live').
v('vivís', 'live').
v('viven', 'live').
v('deber', 'must').
v('debo', 'must').
v('debes', 'must').
v('debe', 'must').
v('debemos', 'must').
v('debéis', 'must').
v('deben', 'must').
v('pensar', 'think').
v('pienso', 'think').
v('piensas', 'think').
v('piensa', 'thinks').
v('pensamos', 'think').
v('pensáis', 'think').
v('piensan', 'think').
v('encontrar', 'find').
v('encuentro', 'find').
v('encuentras', 'find').
v('encuentra', 'finds').
v('encontramos', 'find').
v('encontráis', 'find').
v('encuentran', 'find').
v('llegar', 'arrive').
v('llego', 'arrive').
v('llegas', 'arrive').
v('llega', 'arrives').
v('llegamos', 'arrive').
v('llegáis', 'arrive').
v('llegan', 'arrive').
v('comenzar', 'start').
v('comienzo', 'start').
v('comienzas', 'start').
v('comienza', 'starts').
v('comenzamos', 'start').
v('comenzáis', 'start').
v('comienzan', 'start').
v('terminar', 'finish').
v('termino', 'finish').
v('terminas', 'finish').
v('termina', 'finishes').
v('terminamos', 'finish').
v('termináis', 'finish').
v('terminan', 'finish').
v('trabajar', 'work').
v('trabajo', 'work').
v('trabajas', 'work').
v('trabaja', 'works').
v('trabajamos', 'work').
v('trabajáis', 'work').
v('trabajan', 'work').

v('be', 'ser').
v('am', 'soy').
v('are', 'eres').
v('is', 'es').
v('are', 'somos').
v('are', 'sois').
v('are', 'son').
v('have', 'haber').
v('have', 'tengo').
v('have', 'tienes').
v('has', 'tiene').
v('have', 'tenemos').
v('have', 'teneis').
v('have', 'tienen').
v('be', 'estar').
v('am', 'estoy').
v('are', 'estás').
v('is', 'está').
v('are', 'estamos').
v('are', 'estáis').
v('are', 'están').
v('have', 'tener').
v('have', 'tengo').
v('have', 'tienes').
v('has', 'tiene').
v('have', 'tenemos').
v('have', 'tenéis').
v('have', 'tienen').
v('do', 'hacer').
v('do', 'hago').
v('do', 'haces').
v('does', 'hace').
v('do', 'hacemos').
v('do', 'hacéis').
v('do', 'hacen').
v('to go', 'ir').
v('go', 'voy').
v('go', 'vas').
v('goes', 'va').
v('go', 'vamos').
v('go', 'vais').
v('go', 'van').
v('see', 'ver').
v('see', 'veo').
v('see', 'ves').
v('sees', 've').
v('see', 'vemos').
v('see', 'veis').
v('see', 'ven').
v('know', 'saber').
v('know', 'sé').
v('know', 'sabes').
v('knows', 'sabe').
v('know', 'sabemos').
v('know', 'sabéis').
v('know', 'saben').
v('love', 'querer').
v('love', 'quiero').
v('love', 'quieres').
v('loves', 'quiere').
v('love', 'queremos').
v('love', 'queréis').
v('love', 'quieren').
v('can', 'poder').
v('can', 'puedo').
v('can', 'puedes').
v('can', 'puede').
v('can', 'podemos').
v('can', 'podéis').
v('can', 'pueden').
v('say', 'decir').
v('say', 'digo').
v('say', 'dices').
v('says', 'dice').
v('say', 'decimos').
v('say', 'decís').
v('say', 'dicen').
v('give', 'dar').
v('give', 'doy').
v('give', 'das').
v('gives', 'da').
v('give', 'damos').
v('give', 'dais').
v('give', 'dan').
v('come', 'venir').
v('come', 'vengo').
v('come', 'vienes').
v('comes', 'viene').
v('come', 'venimos').
v('come', 'venís').
v('come', 'vienen').
v('seem', 'parecer').
v('seem', 'parezco').
v('seem', 'pareces').
v('seem', 'parece').
v('seem', 'parecemos').
v('seem', 'pareceis').
v('seem', 'parecen').
v('believe', 'creer').
v('believe', 'creo').
v('believe', 'crees').
v('believes', 'cree').
v('believe', 'creemos').
v('believe', 'creéis').
v('believe', 'creen').
v('take', 'tomar').
v('take', 'tomo').
v('take', 'tomas').
v('takes', 'toma').
v('take', 'tomamos').
v('take', 'tomáis').
v('take', 'toman').
v('feel', 'sentir').
v('feel', 'siento').
v('feel', 'sientes').
v('feels', 'siente').
v('feel', 'sentimos').
v('feel', 'sentís').
v('feel', 'sienten').
v('live', 'vivir').
v('live', 'vivo').
v('live', 'vives').
v('lives', 'vive').
v('live', 'vivimos').
v('live', 'vivís').
v('live', 'viven').
v('must', 'deber').
v('must', 'debo').
v('must', 'debes').
v('must', 'debe').
v('must', 'debemos').
v('must', 'debéis').
v('must', 'deben').
v('think', 'pensar').
v('think', 'pienso').
v('think', 'piensas').
v('thinks', 'piensa').
v('think', 'pensamos').
v('think', 'pensáis').
v('think', 'piensan').
v('find', 'encontrar').
v('find', 'encuentro').
v('find', 'encuentras').
v('finds', 'encuentra').
v('find', 'encontramos').
v('find', 'encontráis').
v('find', 'encuentran').
v('arrive', 'llegar').
v('arrive', 'llego').
v('arrive', 'llegas').
v('arrives', 'llega').
v('arrive', 'llegamos').
v('arrive', 'llegáis').
v('arrive', 'llegan').
v('start', 'comenzar').
v('start', 'comienzo').
v('start', 'comienzas').
v('starts', 'comienza').
v('start', 'comenzamos').
v('start', 'comenzáis').
v('start', 'comienzan').
v('finish', 'terminar').
v('finish', 'termino').
v('finish', 'terminas').
v('finishes', 'termina').
v('finish', 'terminamos').
v('finish', 'termináis').
v('finish', 'terminan').
v('work', 'trabajar').
v('work', 'trabajo').
v('work', 'trabajas').
v('works', 'trabaja').
v('work', 'trabajamos').
v('work', 'trabajáis').
v('work', 'trabajan').
%------------------------------------------------------------------------------------

%---------------------------- NOMBRES PROPIOS -----------------------------------------------
np('Juan', 'Juan').
np('María', 'María').
np('José', 'José').
np('Ana', 'Ana').
np('Luis', 'Luis').
np('Carlos', 'Carlos').
np('Isabel', 'Isabel').
np('Antonio', 'Antonio').
np('Pedro', 'Pedro').
np('Laura', 'Laura').
np('Francisco', 'Francisco').
np('Carmen', 'Carmen').
np('Manuel', 'Manuel').
np('Rosa', 'Rosa').
np('Andrés', 'Andrés').
np('Elena', 'Elena').
np('Miguel', 'Miguel').
np('Lucía', 'Lucía').
np('Sergio', 'Sergio').
np('Marta', 'Marta').
%------------------------------------------------------------------------------------

%---------------------------- ADJETIVOS -----------------------------------------------
adj('guapo', 'pretty').
adj('bueno', 'good').
adj('nuevo', 'new').
adj('mismo', 'same').
adj('otro', 'other').
adj('grande', 'big').
adj('alto', 'tall').
adj('largo', 'long').
adj('pequeño', 'small').
adj('joven', 'young').
adj('viejo', 'old').
adj('blanco', 'white').
adj('negro', 'black').
adj('rojo', 'red').
adj('verde', 'green').
adj('azul', 'blue').
adj('amarillo', 'yellow').
adj('claro', 'light').
adj('oscuro', 'dark').
adj('bonito', 'pretty').
adj('feo', 'ugly').
adj('feliz', 'happy').
adj('triste', 'sad').
adj('fácil', 'easy').
adj('difícil', 'difficult').
adj('duro', 'hard').
adj('blando', 'soft').
adj('caliente', 'hot').
adj('frío', 'cold').
adj('rico', 'tasty').
adj('pobre', 'poor').

adj('pretty', 'guapo').
adj('good', 'bueno').
adj('new', 'nuevo').
adj('same', 'mismo').
adj('other', 'otro').
adj('big', 'grande').
adj('tall', 'alto').
adj('long', 'largo').
adj('small', 'pequeño').
adj('young', 'joven').
adj('old', 'viejo').
adj('white', 'blanco').
adj('black', 'negro').
adj('red', 'rojo').
adj('green', 'verde').
adj('blue', 'azul').
adj('yellow', 'amarillo').
adj('light', 'claro').
adj('dark', 'oscuro').
adj('pretty', 'bonito').
adj('ugly', 'feo').
adj('happy', 'feliz').
adj('sad', 'triste').
adj('easy', 'fácil').
adj('difficult', 'difícil').
adj('hard', 'duro').
adj('soft', 'blando').
adj('hot', 'caliente').
adj('cold', 'frío').
adj('tasty', 'rico').
adj('poor', 'pobre').
%------------------------------------------------------------------------------------

%---------------------------- ADVERBIOS -----------------------------------------------
adv('ahora', 'now').
adv('aquí', 'here').
adv('allí', 'there').
adv('siempre', 'always').
adv('nunca', 'never').
adv('todavía', 'still').
adv('ya', 'already').
adv('también', 'also').
adv('sí', 'yes').
adv('no', 'no').
adv('quizás', 'perhaps').
adv('tal vez', 'maybe').
adv('más', 'more').
adv('menos', 'less').
adv('bien', 'well').
adv('mal', 'badly').
adv('mucho', 'much').
adv('poco', 'little').
adv('despacio', 'slowly').
adv('rápido', 'quickly').
adv('pronto', 'soon').
adv('tarde', 'late').
adv('temprano', 'early').
adv('además', 'furthermore').
adv('todavía no', 'not yet').

adv('now', 'ahora').
adv('here', 'aquí').
adv('there', 'allí').
adv('always', 'siempre').
adv('never', 'nunca').
adv('still', 'todavía').
adv('already', 'ya').
adv('also', 'también').
adv('yes', 'sí').
adv('no', 'no').
adv('perhaps', 'quizás').
adv('maybe', 'tal vez').
adv('more', 'más').
adv('less', 'menos').
adv('well', 'bien').
adv('badly', 'mal').
adv('much', 'mucho').
adv('little', 'poco').
adv('slowly', 'despacio').
adv('quickly', 'rápido').
adv('soon', 'pronto').
adv('late', 'tarde').
adv('early', 'temprano').
adv('furthermore', 'además').
adv('not yet', 'todavía no').
%------------------------------------------------------------------------------------

%---------------------------- CONJUNCIONES -----------------------------------------------
conj('y', 'and').
conj('o', 'or').
conj('pero', 'but').
conj('porque', 'because').
conj('si', 'if').
conj('como', 'like').
conj('aunque', 'although').
conj('mientras', 'while').
conj('ya que', 'since').
conj('pues', 'so, therefore').
conj('ni', 'nor').
conj('antes', 'before').
conj('después', 'after').
conj('por lo tanto', 'therefore').
conj('sin embargo', 'however').
conj('por lo tanto', 'therefore').

conj(',', ',').

conj('and', 'y').
conj('or', 'o').
conj('but', 'pero').
conj('because', 'porque').
conj('if', 'si').
conj('like', 'como').
conj('although', 'aunque').
conj('while', 'mientras').
conj('since', 'ya que').
conj('so, therefore', 'pues').
conj('nor', 'ni').
conj('before', 'antes').
conj('after', 'después').
conj('therefore', 'por lo tanto').
conj('however', 'sin embargo').
%------------------------------------------------------------------------------------

%---------------------------- PREPOSICIONES -----------------------------------------------
prep('a', 'to').
prep('ante', 'before').
prep('bajo', 'under').
prep('con', 'with').
prep('de', 'of, from').
prep('desde', 'from').
prep('durante', 'during').
prep('en', 'in').
prep('entre', 'between').
prep('hacia', 'toward').
prep('hasta', 'until').
prep('mediante', 'by means of').
prep('para', 'for').
prep('por', 'by, through').
prep('según', 'according to').
prep('sin', 'without').
prep('sobre', 'on, about').
prep('tras', 'after').

prep('to', 'a').
prep('before', 'ante').
prep('under', 'bajo').
prep('with', 'con').
prep('of, from', 'de').
prep('from', 'desde').
prep('during', 'durante').
prep('in', 'en').
prep('between', 'entre').
prep('toward', 'hacia').
prep('until', 'hasta').
prep('by means of', 'mediante').
prep('for', 'para').
prep('by, through', 'por').
prep('according to', 'según').
prep('without', 'sin').
prep('on, about', 'sobre').
prep('after', 'tras').
%------------------------------------------------------------------------------------

%---------------------------- NEXOS -----------------------------------------------
nx('que', 'that').
nx('porque', 'because').
nx('aunque', 'although').
nx('si', 'if').
nx('cuando', 'when').
nx('mientras', 'while').
nx('después de que', 'after').
nx('antes de que', 'before').
nx('como', 'as').
nx('para que', 'so that').
nx('sin que', 'without').
nx('a fin de que', 'in order that').
nx('ya que', 'since').
nx('por si', 'in case').
nx('a menos que', 'unless').
nx('a pesar de que', 'despite').
nx('en caso de que', 'in case').

nx('that', 'que').
nx('because', 'porque').
nx('although', 'aunque').
nx('if', 'si').
nx('when', 'cuando').
nx('while', 'mientras').
nx('after', 'después de que').
nx('before', 'antes de que').
nx('as', 'como').
nx('so that', 'para que').
nx('without', 'sin que').
nx('in order that', 'a fin de que').
nx('since', 'ya que').
nx('in case', 'por si').
nx('unless', 'a menos que').
nx('despite', 'a pesar de que').
nx('in case of', 'en caso de que').
%------------------------------------------------------------------------------------

%---------------------------- SIGNOS -----------------------------------------------
sig('?').
sig('!').
%------------------------------------------------------------------------------------
