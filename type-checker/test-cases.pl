%varTs
hasType([{"X", intT}, {"Y", intT}], varT("X"), T).
hasType([{"X", boolT}, {"X", intT}], varT("X"), T).

%Constants
hasType([], -652, T).
hasType([], true, T).

%arithmetic
hasType([], add(sub(2,5), div(6,mul(2,5))), T).

%boolean
hasType([{"X", boolT}], and(implies(or(varT("X"), false), true), implies(varT("X"), not(false))), T).

%comparison
hasType([{"X", boolT}, {"Y", boolT}], or(and(grt(-2, 6), les(3,100)), implies(eql(5, varT("Y")), varT("X"))), T).

%equality
hasType([], eql([[1,3], true],  [1,3,true]), T).

%if then else
hasType([{"X", boolT}, {"Y", intT}], conditional(and(varT("X"), grt(varT("Y"), 0)), varT("Y"), varT("X")), T).

%let d in e
hasType([{"Y", intT}], qualif(def("X",3), add(varT("Y"), varT("X"))), T).
hasType([{"Y", intT}], qualif(def(varT("Y"),3), mul(varT("Y"),5)), T).


%abstraction
hasType([{"X", boolT}, {"W", boolT}], function("X", varT("W")), arrowT(boolT, boolT)). 

%application
hasType([{"R", arrowT(boolT,boolT)}, {"S", boolT}], apply(varT("R"), varT("S")), boolT).
hasType([{"R", arrowT(boolT,boolT)}, {"S", boolT}], apply(varT("R"), varT("S")), T).

%n-tuple
hasType([{"X", boolT}, {"W", boolT}], [varT("W"), varT("W"), and(varT("X"), varT("Y"))], [boolT, boolT]).

%projection
hasType([{"Y", boolT}, {"Z", boolT}], proj([varT("X"), varT("W"), and(varT("X"), varT("Y"))], 1), boolT).

%type elaborates
typeElaborates([], def("X", add(3,4)), T).
typeElaborates([], def("Y", true), T).
typeElaborates([], defParl(def("X",3), def("Y",true)), T).
typeElaborates([], defParl(def("X",3), def("X",true)), T).
typeElaborates([], defSeql(def("X",mul(31,20)), def("Y",true)), T).

typeElaborates([{"X",boolT}, {"Y",intT}],
				defLocl(
							def("X",31),
						 	defParl(def("X", [varT("Y")]), def("Y", false))
						), T).

typeElaborates([{"X",boolT}, {"Y",intT}],
				defLocl(
							def("X",20),
							defParl(def("X",3), def("Y",false))
						 ), T).

typeElaborates([{"X",intT}], def("Y", 9), G).
typeElaborates([{"X",intT}], defSeql(def("Z",true), def("Y", false)), G).
typeElaborates([{"X",intT}], defParl(def("Z",9), def("Y",0)), G).
typeElaborates([{"X",intT}], defLocl(def("Z",9), def("Y",4)), G).
typeElaborates([{"X",intT}], defParl(defSeql(def("Z",8), def("Y", true)), def("Y", false)), G).
typeElaborates([{"X",intT}], defSeql(defParl(def("Z",45), def("Y", false)), def("Y",8)), G).
