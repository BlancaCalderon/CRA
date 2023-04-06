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
det('which').
det('whose').
det('what').

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
oracion11(['The', 'word', 'processor', 'which', 'is', 'a', 'quite', 'powerful', 'tool', ',', 'is used', 'for', 'writing', 'documents']).
oracion12(['The', 'word', 'processor', 'is', 'a', 'very', 'powerful', 'tool', 'that', 'is used', 'for', 'writing', 'documents', 'but', 'it', 'is', 'quite', 'slow']).
oracion13(['The', 'mouse', 'that', 'the', 'cat', 'caught', 'was', 'gray']).
oracion14(['The', 'man', 'we', 'saw', 'yesterday', 'was', 'my', 'neighbor']).

