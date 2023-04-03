%-------------------------------------------------------------------------------
%Oraciones
%-------------------------------------------------------------------------------
oracion_simple(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).
oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV), g_nominal(GN), signo(Sig).

oracion_coordinada(oc(O, CC, O)) --> oracion_simple(O), conjuncion_coordinada(CC), oracion_simple(O).

oracion_subordinada(or(O, CS, O)) --> oracion_simple(O), conjuncion_subordinada(CS), oracion_simple(O).

%Oraciones compuestas
oracion_compuesta(ocm(OS, OS)) --> oracion_simple(OS), oracion_simple(OS).
oracion_compuesta(ocm(OS, OC)) --> oracion_simple(OS), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, OR)) --> oracion_simple(OS), oracion_subordinada(OR).
oracion_compuesta(ocm(OC, OC)) --> oracion_coordinada(OC), oracion_coordinada(OC).
oracion_compuesta(ocm(OC, OS)) --> oracion_coordinada(OC), oracion_simple(OS).
oracion_compuesta(ocm(OC, OR)) --> oracion_coordinada(OC), oracion_subordinada(OR).
oracion_compuesta(ocm(OR, OR)) --> oracion_subordinada(OR), oracion_subordinada(OR).
oracion_compuesta(ocm(OR, OS)) --> oracion_subordinada(OR), oracion_simple(OS).
oracion_compuesta(ocm(OR, OC)) --> oracion_subordinada(OR), oracion_coordinada(OC).

oracion_compuesta(ocm(OCM, OS)) --> oracion_compuesta(OCM), oracion_simple(OS).
oracion_compuesta(ocm(OCM, OC)) --> oracion_compuesta(OCM), oracion_coordinada(OC).
oracion_compuesta(ocm(OCM, OR)) --> oracion_compuesta(OCM), oracion_subordinada(OR).


%-------------------------------------------------------------------------------
%Grupos
%-------------------------------------------------------------------------------
%Grupo nominal
g_nominal(gn(N)) --> nombre(N),!.
g_nominal(gn(NP)) --> nombre_propio(NP).
g_nominal(gn(Det, N)) --> determinante(Det), nombre(N).
g_nominal(gn(Det, GAdj, N)) --> determinante(Det), g_adjetival(GAdj), nombre(N).
g_nominal(gn(GAdj, NP)) --> g_adjetival(GAdj), nombre_propio(NP).
g_nominal(gn(GAdv, GN)) --> g_adverbial(GAdv), g_nominal(GN).

g_nominal(gn(GN, Conj, GN)) --> g_nominal(GN), conjuncion_coordinada(Conj), g_nominal(GN).
g_nominal(gn(GN, GAdv)) --> g_nominal(GN), g_adverbial(GAdv).
g_nominal(gn(GPrep, GN)) --> g_nominal(GN), g_preposicional(GPrep).

%Grupo verbal
g_verbal(gv(GAdv, GV, GAdv)) --> g_adverbial(GAdv), g_verbal(GV), g_adverbial(GAdv),!.
g_verbal(gv(GPrep, GV, GPrep)) --> g_preposicional(GPrep), g_verbal(GV), g_preposicional(GPrep),!.

g_verbal(gv(V, GV)) --> verbo(V), g_verbal(GV),!.

g_verbal(gv(V, GN)) --> verbo(V), g_nominal(GN),!.

g_verbal(gv(V, GAdj)) --> verbo(V), g_adjetival(GAdj),!.

g_verbal(gv(GAdv, GV)) --> g_adverbial(GAdv), g_verbal(GV).

g_verbal(gv(GPrep, GV)) --> g_preposicional(GPrep), g_verbal(GV).

g_verbal(gv(GV, GAdv)) --> g_verbal(GV), g_adverbial(GAdv).
g_verbal(gv(GV, GPrep)) --> g_verbal(GV), g_preposicional(GPrep).

g_verbal(gv(V)) --> verbo(V).


%Grupo adjetival
g_adjetival(gadj(Adj)) --> adjetivo(Adj).
g_adjetival(gadj(Adj, Conj, GAdj)) --> adjetivo(Adj), conjuncion_coordinada(Conj), g_adjetival(GAdj).

%Grupo adverbial
g_adverbial(gadv(Adv)) --> adverbio(Adv).
g_adverbial(gadv(Adv, GAdv)) --> adverbio(Adv), g_adverbial(GAdv).

%Grupo preposicional
g_preposicional(gp(Prep, GN)) -->  preposicion(Prep), g_nominal(GN).
g_preposicional(gp(Prep, GN, Conj, GP)) -->  preposicion(Prep), g_nominal(GN), conjuncion_coordinada(Conj), g_preposicional(GP).


%-------------------------------------------------------------------------------
%Elementos
%-------------------------------------------------------------------------------
determinante(det(X)) --> [X], {det(X)}.
nombre(n(X)) --> [X], {n(X)}.
nombre_propio(np(X)) --> [X], {np(X)}.
verbo(v(X)) --> [X], {v(X)}.
adjetivo(adj(X)) --> [X], {adj(X)}.
adverbio(adv(X)) --> [X], {adv(X)}.
conjuncion_coordinada(conjc(X)) --> [X], {conjc(X)}.
conjuncion_subordinada(conjs(X)) --> [X], {conjs(X)}.
preposicion(prep(X)) --> [X], {prep(X)}.
signo(sig(X)) --> [X], {sig(X)}.

:-consult('diccionario.pl').


