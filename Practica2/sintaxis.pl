%-------------------------------------------------------------------------------
%Oraciones
%-------------------------------------------------------------------------------
oracion_simple(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).
oracion_simple(o(GV, GN, Sig)) --> g_verbal(GV), g_nominal(GN), signo(Sig).
oracion_aux(o(GV)) --> g_verbal(GV).

oracion_coordinada(oc(O1, CC, O2)) --> oracion_simple(O1), conjuncion_coordinada(CC), oracion_simple(O2).
oracion_coordinada(oc(O1, CC, OA)) --> oracion_simple(O1), conjuncion_coordinada(CC), oracion_aux(OA).

oracion_subordinada(or(O1, CS, O2)) --> oracion_simple(O1), conjuncion_subordinada(CS), oracion_simple(O2).

%Oraciones compuestas
oracion_compuesta(ocm(OS, Conj, OS)) --> oracion_simple(OS), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OS, Conj, OC)) --> oracion_simple(OS), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, Conj, OR)) --> oracion_simple(OS), conjuncion(Conj), oracion_subordinada(OR).
oracion_compuesta(ocm(OC, Conj, OC)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OC, Conj, OS)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OC, Conj, OR)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_subordinada(OR).
oracion_compuesta(ocm(OR, Conj, OR)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_subordinada(OR).
oracion_compuesta(ocm(OR, Conj, OS)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_simple(OS).
oracion_compuesta(ocm(OR, Conj, OC)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, Conj, OCM)) --> oracion_simple(OS), conjuncion(Conj), oracion_compuesta(OCM).
oracion_compuesta(ocm(OC, Conj, OCM)) --> oracion_coordinada(OC), conjuncion(Conj), oracion_compuesta(OCM).
oracion_compuesta(ocm(OR, Conj, OCM)) --> oracion_subordinada(OR), conjuncion(Conj), oracion_compuesta(OCM).

oracion_compuesta(ocm(OS, OS)) --> oracion_simple(OS), oracion_simple(OS).
oracion_compuesta(ocm(OS, OC)) --> oracion_simple(OS), oracion_coordinada(OC).
oracion_compuesta(ocm(OS, OR)) --> oracion_simple(OS), oracion_subordinada(OR).
oracion_compuesta(ocm(OS, OA)) --> oracion_simple(OS), oracion_aux(OA).
oracion_compuesta(ocm(OC, OC)) --> oracion_coordinada(OC), oracion_coordinada(OC).
oracion_compuesta(ocm(OC, OS)) --> oracion_coordinada(OC), oracion_simple(OS).
oracion_compuesta(ocm(OC, OR)) --> oracion_coordinada(OC), oracion_subordinada(OR).
oracion_compuesta(ocm(OC, OA)) --> oracion_coordinada(OC), oracion_aux(OA).
oracion_compuesta(ocm(OR, OR)) --> oracion_subordinada(OR), oracion_subordinada(OR).
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
g_nominal(gn(Det, GN)) --> determinante(Det), g_nominal(GN).
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

%--------------------------
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

:-consult('diccionario.pl').
