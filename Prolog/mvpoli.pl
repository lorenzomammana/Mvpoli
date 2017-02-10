%%%% 807391 Mammana Lorenzo
%%%% Nessuna collaborazione

%%% mvpoli.pl

%%	is_monomial(M)
%	true if M is a monomial

is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs),
	total_degree_calculator(VPs, TD).

%%	is_varpower(V)
%	true if v is a varpower

is_varpower(v(Power, VarSymbol)) :-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

%%	is_polynomial(P)
%	true if P is a polynomial

is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).

%%	coefficients(P, R)
%       true if R is a list of coeffients of P
%	P could be a monomial, a polynomial already parsed or not
%	parsed

coefficients(Poly, RCoefficients) :-
	as_polynomial(Poly, P),
	arg(1, P, LM),
	LM \= [],
	!,
	apply_coeff(P, RCoefficients).

coefficients(Poly, [0]) :-
	as_polynomial(Poly, P),
	arg(1, P, LM),
	LM = [],
	!.

%%	apply_coeff(P, R)
%	same as coefficients but P must be a parsed polynomial

apply_coeff(poly(MonoList), Coefficients) :-
	!,
	get_coefficients(MonoList, Coefficients).

%%	get_coefficients(M, C)
%	true when M is a list of monomials and C is a list of
%	coefficients in M

get_coefficients([m(C, _, _) | Omono], [C | Coefficients]) :-
	get_coefficients(Omono, Coefficients).

get_coefficients([], []).

%%	merge_list(L, LR)
%	true if L is a list of lists and LR is a list
%	containing the elements of the lists of L

merge_list([L | Ls], LR) :-
	append(L, Tmp, LR),
	merge_list(Ls, Tmp).

merge_list([], []).

%%	variables(P, V)
%	true if V is a list of Variables that appear in P
%	V must be sorted lexicographically
%	P could be a monomial, a polynomial already parsed or not
%	parsed

variables(Poly, SVariables) :-
	as_polynomial(Poly, P),
	apply_variables(P, SVariables).

%%	apply_variables(P, V)
%	same as variables but P must be a parsed polynomial

apply_variables(poly(MonoList), SVariables) :-
	!,
	get_mono_variables(MonoList, MonoVariables),
	get_vars_sym(MonoVariables, SymVariables),
	merge_list(SymVariables, Variables),
	sort(Variables, SVariables).

%%	get_mono_variables(M, V)
%	true if M is a list of monomials and V is a list of
%	variables that appear in M

get_mono_variables([m(_, _, Vars) | Omono], [Vars | Variables]) :-
	get_mono_variables(Omono, Variables).

get_mono_variables([], []).

%%	get_vars_sym(V, S)
%	true if V is a list of varpowers and S is a list
%	of symbols that appear in V

get_vars_sym([Var | Ovar], [S | VarsSym]) :-
	get_var_sym(Var, S),
	get_vars_sym(Ovar, VarsSym).

get_vars_sym([], []).

get_var_sym([v(_, X) | Ovar], [X | Xs]) :-
	get_var_sym(Ovar, Xs).

get_var_sym([], []).

%%	maxdegree(P, M)
%	true if M is the maximum degree of P
%	P could be a monomial, a polynomial already parsed or not
%	parsed

maxdegree(Poly, Max) :-
	as_polynomial(Poly, P),
	apply_degree(P, Degree),
	test_empty(Degree, NDegree),
	max_list(NDegree, Max).

%%	test_empty(L, R)
%	R = L if L is not empty, R = [0] otherwise

test_empty([X | Xs], [X | Xs]).

test_empty([], [0]).

%%	apply_degree(P, D)
%	true if D is a list of degrees that appear in P
%	P must be a parsed polynomial

apply_degree(poly(MonoList), Degrees) :-
	!,
	get_degrees(MonoList, Degrees).

%%	maxdegree(P, M)
%	true if M is the minimum degree of P
%	P could be a monomial, a polynomial already parsed or not
%	parsed

mindegree(Poly, Min) :-
	as_polynomial(Poly, P),
	apply_degree(P, Degree),
	test_empty(Degree, NDegree),
	min_list(NDegree, Min).

%%	get_degrees(M, D)
%	true if M is a list of monomials and D is a list of degrees
%	that appear in M

get_degrees([Mono | Omono], [D | ListDegrees]) :-
	arg(2, Mono, D),
	get_degrees(Omono, ListDegrees).

get_degrees([], []).

%%	monomials(P, SM)
%	true if SM is a list of grlex sorted monomials that appear in P
%	P could be a monomial, a polynomial already parsed or not
%	parsed

monomials(Poly, SortedMono) :-
	as_polynomial(Poly, P),
	apply_monomials(P, SortedMono).

%%	apply_monomials(P, SM)
%	same as monomials but P must be a parsed polynomial

apply_monomials(poly(MonoList), SortedMono) :-
	!,
	sort(2, @=<, MonoList, Monomials),
	sort_poly(Monomials, SortedMono).

%%	polyplus(P1, P2, R)
%	true if R is the result sorted grlex of P1 + P2
%	P(1|2) could be a monomial, a polynomial already parsed or
%	not parsed

polyplus(Poly1, Poly2, poly(Result)) :-
	as_polynomial(Poly1, P1),
	as_polynomial(Poly2, P2),
	sum(P1, P2, Result),
	!.

%%	sum(P1, P2, R)
%	true if R equals monomials(P3)
%	where P3 is P1 + P2
%	P1 and P2 must be parsed polynomials

sum(poly(M1), poly(M2), FinResult) :-
	is_polynomial(poly(M1)),
	is_polynomial(poly(M2)),
	append(M1, M2, ListMonoAll),
	sort(2, @=<, ListMonoAll, SortedMono),
	sort_poly(SortedMono, SortedMonoSum),
	sum_monomials(SortedMonoSum, Result),
	simplify_coeff(Result, SCResult),
	simplify_exp(SCResult, FinResult).

%%	sum_monomials(M, R)
%	true if R is a list of monomials equal to the sum of
%	the monomials in M
%	M must be grlex sorted

sum_monomials([m(C1, TD, V), m(C2, TD, V) | Omono], Result) :-
	D is C1 + C2,
	D \= 0,
	!,
	sum_monomials([m(D, TD, V) | Omono], Result).

sum_monomials([m(C1, TD, V), m(C2, TD, V) | Omono], Result) :-
	D is C1 + C2,
	D = 0,
	!,
	sum_monomials(Omono, Result).

sum_monomials([m(C1, TD1, V), m(C2, TD2, V2) | Omono],
	      [m(C1, TD1, V) | Result]) :-
	V \= V2,
	!,
	sum_monomials([m(C2, TD2, V2) | Omono], Result).

sum_monomials([m(C, TD, V)], [m(C, TD, V)]) :-
	!.

sum_monomials([], []).

%%	polyminus(P1, P2, R)
%	true if R is the result grlex sorted of P1 - P2
%	P(1|2) could be a monomial, a polynomial already parsed or
%	not parsed

polyminus(Poly1, Poly2, poly(Result)) :-
	as_polynomial(Poly1, P1),
	as_polynomial(Poly2, P2),
	diff(P1, P2, Result),
	!.

%%	diff(P1, P2, R)
%	true if R equals monomials(P3)
%	where P3 is P1 - P2
%	P1 and P2 must be parsed polynomials

diff(poly(M1), poly(M2), Result) :-
	is_polynomial(poly(M1)),
	is_polynomial(poly(M2)),
	negatelist(M2, NM2),
	append(M1, NM2, ListMonoAll),
	sort(2, @=<, ListMonoAll, SortedMono),
	sort_poly(SortedMono, SortedMonoSum),
	sum_monomials(SortedMonoSum, Result).

%%	negatelist(L, NL)
%	true if L is a list of monomials and
%	NL is the same list but with negate coefficients

negatelist([m(C, TD, V) | Om], [m(NC, TD, V) | Rest]) :-
	NC is - C,
	negatelist(Om, Rest).

negatelist([], []).

%%	test_mono(P, M)
%	M is the list of monomials in the polynomial
%	if the polynomial has no monomials M is m(0, 0, [])

test_mono(poly([X]), X) :-
	!.

test_mono(poly([]), m(0, 0, [])) :-
	!.

%%	as_monomial(Exp, Mono)
%	parse a given expression into the corresponding monomial
%	representation
%	Exp must be an unparsed monomial
%	Mono variables are lex sorted

as_monomial(P + Q, m(Coeff, 0, [])) :-
	catch(_ is Q, _, false),
	catch(_ is P, _, false),
	!,
	Coeff is P + Q.

as_monomial(_ + _, _) :-
	!,
	fail.

as_monomial(_ - _, _) :-
	!,
	fail.

as_monomial(Expression, RetMono) :-
	!,
	extract_coefficient(Expression, C),
	extract_vars(Expression, Var),
	sort(2,@=<,Var,SVar),
	mul_var(SVar, MVar),
	sort_monomial(MVar, SortedVar),
	total_degree_calculator(Var, TD),
	!,
	Coeff is C,
	Monomial = m(Coeff, TD, SortedVar),
	simplify_exp([Monomial], [SimMono]),
	test_null(SimMono, RetMono).

%%	test_null(M1, M2)
%	M2 = M1 if the coefficients of M1 is not 0
%	M2 = m(0, 0, []) otherwise

test_null(m(0, _, _), m(0, 0, [])) :-
	!.

test_null(m(C, E, V), m(C, E, V)) :-
	C \= 0,
	!.

%%	sort_monomials(M, SM)
%	true if M is a list of monomials and SM
%	is the grlex sorted list of M

sort_monomials([m(C, TD, V) | Om], [m(C, TD, SVM) | OSm]) :-
	!,
	mul_var(V, VM),
	sort_monomial(VM, SVM),
	sort_monomials(Om, OSm).

sort_monomials([], []) :-
	!.

%%	as_polynomial(Exp, Poly)
%	parse the expression Exp into the corresponding
%	polynomial representation
%	Exp can be a parsed monomial, a parsed polynomial or an
%	unparsed expression

as_polynomial(m(C, TD, V), poly(SC)) :-
	!,
	is_monomial(m(C, TD, V)),
	mul_var(V, VM),
	sort_monomial(VM, SVM),
	simplify_coeff([m(C, TD, SVM)], SCm),
	simplify_exp(SCm, SC).

as_polynomial(poly(MonoList), poly(SCfinal)) :-
	!,
	is_polynomial(poly(MonoList)),
	sort_monomials(MonoList, SMonoList),
	sort(2, @=<, SMonoList, SortedMono),
	sort_poly(SortedMono, SortedPoly),
	simplify_coeff(SortedPoly, SCm),
	simplify_exp(SCm, SC),
	sum(poly(SC), poly([]), SCfinal).

as_polynomial(Expression, poly(SimPolyFinal)) :-
	!,
	extract_monomials(Expression, Monomials),
	sort(2, @=<, Monomials, SortedMono),
	sort_poly(SortedMono, SortedPoly),
	simplify_coeff(SortedPoly, SimPolyC),
	simplify_exp(SimPolyC, SimPoly),
	sum(poly(SimPoly), poly([]), SimPolyFinal).

%%	simplify_coeff(M, SM)
%	true if M is a list of monomials and SM
%	is the same list without monomials with coefficients 0

simplify_coeff([m(0, _, _) | Omono], Sim) :-
	!,
	simplify_coeff(Omono, Sim).

simplify_coeff([m(C, TD, V) | Omono], [m(C, TD, V) | Sim]) :-
	!,
	C \= 0,
	simplify_coeff(Omono, Sim).

simplify_coeff([], []).

%%	simplify_exp(M, SM)
%	true if M is a list of monomials and SM is the same list
%	but variables with coefficients 0 are removed

simplify_exp([m(C, TD, []) | Omono], [m(C, TD, []) | Sim]) :-
	!,
	simplify_exp(Omono, Sim).

simplify_exp([m(C, _, V) | Omono], [m(C, NTD, CkdV) | Sim]) :-
	V \= [],
	!,
	check_exp(V, CkdV),
	total_degree_calculator(CkdV, NTD),
	simplify_exp(Omono, Sim).

simplify_exp([], []).

%%	check_exp(V, CV)
%	true if V is a list of variables and CV is the same
%	list but without variables with coefficient 0

check_exp([v(0, _) | Ov], Var) :-
	!,
	check_exp(Ov, Var).

check_exp([v(E, S) | Ov], [v(E, S) | Var]) :-
	E \= 0,
	!,
	check_exp(Ov, Var).

check_exp([], []).

%%	extract_monomials(Exp, M)
%	parse an expression into the corresponding list of monomials

extract_monomials(Mono1 + Mono2, Monomials) :-
	!,
	as_monomial(Mono2, M),
	extract_monomials(Mono1, Omonomials),
	append(Omonomials, [M], Monomials).

extract_monomials(Mono1 - Mono2, Monomials) :-
	!,
	as_monomial(Mono2, MnoSign),
	negate(MnoSign, M),
	extract_monomials(Mono1, Omonomials),
	append(Omonomials, [M], Monomials).

extract_monomials(Mono, [M]) :-
	!,
	as_monomial(Mono, M).
%%	negate(M, NM)
%	true if M is a monomial and NM is the same monomial
%	but with negate coefficient

negate(m(C, TD, V), m(NC, TD, V)) :-
	NC is 0 - C.

%%	extract_coefficient(Exp, C)
%	C is the coefficient of the given unparsed monomial
%       expression Exp

extract_coefficient(V * Q, C * Q) :-
	catch(_ is Q, _, false),
	!,
	extract_coefficient(V, C).

extract_coefficient(V * _, C) :-
	!,
	extract_coefficient(V, C).

extract_coefficient(V, C) :-
	catch(_ is V, _, false),
	C = V,
	!.

extract_coefficient(V, 1) :-
	atomic(V).

extract_coefficient(V, 1) :-
	compound(V).

%%	total_degree_calculator(V, TD)
%	TD is the total degree of the variables in V

total_degree_calculator([v(X, _) | V], TD) :-
	total_degree_calculator(V, T),
	TD is X + T.

total_degree_calculator([], 0).

%%	extract_vars(Exp, Vars)
%	parse variables from the given monomial expression Exp into
%	variables list Vars

extract_vars(V * Q, Variables) :-
	catch(_ is Q, _, false),
	!,
	extract_vars(V, Variables).


extract_vars(V * S^E, Variables) :-
	!,
	Var = [v(E, S)],
	extract_vars(V, Ovar),
	append(Ovar, Var, Variables).

extract_vars(V * S, Variables) :-
	!,
	Var = [v(1, S)],
	extract_vars(V, Ovar),
	append(Ovar, Var, Variables).

extract_vars(S, []) :-
	catch(_ is S, _, C = 1),
	S = C,
	!.

extract_vars(S^E, [Var]) :-
	!,
	ground(S),
	Var = v(E, S).

extract_vars(S, [Var]) :-
	Var = v(1, S).

extract_vars(_, []).

%%	sort_monomial(M, SM)
%	SM is the lex sorted monomial M

sort_monomial(Mono, Sorted) :-
	!,
	sort(2, @=<, Mono, Sorted).

sort_monomial([], []) :-
	!.

%%	sort_poly(M, P)
%	true if M is a list of lex sorted monomials
%	and P is the list of grlex sorted monomials in M

sort_poly(SortedMono, ExtractedPoly) :-
	degree_split(SortedMono, DivMono),
	create_map(DivMono, MonoMap),
	sort_maps(MonoMap, SortedMaps),
	merge_list(SortedMaps, SortedMap),
	extract_poly(SortedMap, ExtractedPoly).

%%	sort_maps(M, SM)
%	true if M is a list of map(Mono, Varstring)
%	and SM is the same list lex sorted on Varstring

sort_maps([Map | Omap], [Smap | SortedMap]) :-
	sort(2, @=<, Map, Smap),
	sort_maps(Omap, SortedMap).

sort_maps([], []).

%%	extract_poly(M, L)
%	true if M is a list of map(X, Y)
%	and L is a list of all X in M

extract_poly([map(X, _) | Omap], [X | Xs]) :-
	extract_poly(Omap, Xs).

extract_poly([], []).

%%	create_map(L, LM)
%	true if L is a list of list of monomials and LM is a list of
%	list of map(Mono, Varstring) where Mono is in L and Varstring is
%	a string represention of Mono variables

create_map([L | Ls], [Lmap | RestMap]) :-
	extract_map(L, Lmap),
	create_map(Ls, RestMap).

create_map([], []).

%%	extract_map(M, L)
%	true if M is a list of monomials and L is a list of
%	map(Mono, Varstring) where Mono is in L and Varstring
%	is a string representation of Mono variables

extract_map([m(C, TD, V) | Omono], [map(m(C, TD, V), VarString) | Rest]) :-
	create_var_string(V, VarString),
	extract_map(Omono, Rest).

extract_map([], []).

%%	create_var_string(V, Sv)
%	V is a list of variables, Sv is a String obtained
%	concatenating the StringVar representation of all variables
%	a variable v(E, S) equals a String S|E

create_var_string([v(E, S) | Ov], StringVar) :-
	atomics_to_string([S, E], '|', Str),
	create_var_string(Ov, Ostr),
	string_concat(Str, Ostr, StringVar).

create_var_string([], "").

%%	degree_split(M, DM)
%	split a list of monomials M into a list of lists DM
%	each list of DM contains all the monomials in M with the
%	same degree

degree_split([m(C, TD, V) | Om], [Le | Rest]) :-
	test_equal([m(C, TD, V) | Om], TD, Le, Supp),
	degree_split(Supp, Rest).

degree_split([], []).

%%	test_equal(M, TD, M2, S)
%       S is a list of all monomials in M with total degree TD

test_equal([m(C, TD, V) | Om], TD, [m(C, TD, V) | E], Supp) :-
	!,
	test_equal(Om, TD, E, Supp).

test_equal([m(C, DD, V) | Om], TD, [], [m(C, DD, V) | Om]) :-
	!,
	DD \= TD.

test_equal([], _, [], []) :-
	!.

%%	pprint_polynomial(P)
%	print the representation of the given P
%	P can be a monomial, a parsed polynomial or an unparsed
%	polynomial

pprint_polynomial(Polynomial) :-
	as_polynomial(Polynomial, P),
	apply_pprint_polynomial(P),
	write("\n").

%%	apply_pprint_polynomial(P)
%	same as pprint_polynomial but P must be a parsed polynomial

apply_pprint_polynomial(poly([])) :-
	!,
	write('0').

apply_pprint_polynomial(Polynomial) :-
	!,
	print_mono(Polynomial).

%%	print_mono(M)
%%	print the parsed polynomial M

print_mono(poly([m(C, _, V),  m(C2, _, V2) | Omono])) :-
	write_coeff(C),
	!,
	write_vars(V),
	write_sign(C2),
	print_mono(poly([m(C2, _, V2) | Omono])).

print_mono(poly([m(C, _, V)])) :-
	V \= [],
	write_coeff(C),
	!,
	write_vars(V).

print_mono(poly([m(C, _, [])])) :-
	write(C),
	!.

print_mono([]).

print_mono(poly([])) :-
	!.

%%	write_sign(C)
%	print + if C > 0
%	nothign otherwis

write_sign(C) :-
	C < 0,
	!,
	write(' ').

write_sign(C) :-
	C > 0,
	!,
	write(' + ').

%%	print_coeff(C)
%	nothing if C = 1 ; -1
%	print C otherwise

write_coeff(1) :-
	!.

write_coeff(- C) :-
	C = 1,
	write('- '),
	!.

write_coeff(C) :-
	C \= 1,
	C \= -1,
	write(C),
	write('').

%%	write_vars(V)
%	print the representation of a list of vars

write_vars([]).

write_vars([v(1, S)]) :-
	write(S),
	!.

write_vars([v(E, S)]) :-
	write(S^E),
	!.

write_vars([v(1, S) | Ov]) :-
	write(S),
	write(''),
	!,
	write_vars(Ov).

write_vars([v(E, S) | Ov]) :-
	write(S^E),
	write(''),
	!,
	write_vars(Ov).

%%	polyval(P, V, Val)
%	true when Val contains P value into the
%	n-dimensional point represented by V that contains
%	a value for each variable in variables(P, Variables)

polyval(Polynomial, VariableValues, Value) :-
	as_polynomial(Polynomial, P),
	variables(P, Var),
	length(Var, L),
	delete_n_list(VariableValues, L, DVar),
	map_val(Var, DVar, MapVal),
	evaluate(P, MapVal, Value).

%%	delete_n_list(IList, L, DList)
%	true if DList is the list where its first L elements
%	equal the first L elements of IList

delete_n_list([X | Xs], L, [X | O]) :-
	L > 0,
	!,
	R is L - 1,
	delete_n_list(Xs, R, O).

delete_n_list(_, 0, []) :-
	!.


%%	map_val(V, Vals, Map)
%	true when Map is a list of map(Var, Val) where
%       Var is the element in the same position of Val in V and Vals

map_val([Var | Ovar], [Val | Oval], [map(Var, Val) | Omap]) :-
	map_val(Ovar, Oval, Omap).

map_val([], [], []).

%%	evaluate(P, Map, V)
%	V is the result of polyval

evaluate(poly(MonoList), MapVal, Value) :-
	evaluate_mono(MonoList, MapVal, Value).

%%	evaluate_mono(M, Map, V)
%	V is the evaluation of a list of monomials
%	based on map(Var, Val) values of Map

evaluate_mono([m(C, _, V) | Omono], MapVal, Value) :-
	evaluate_var(V, MapVal, VarVal),
	MonoVal is C * VarVal,
	evaluate_mono(Omono, MapVal, OmonoVal),
	Value is MonoVal + OmonoVal.

evaluate_mono([], _, 0).

%%	evaluate_var(V, Map, Val)
%	Val is the evaluation of a list of vars based
%	on map(Var, Val) values of Map

evaluate_var([v(E, S) | Ov], [map(S, V) | Om], Value) :-
	!,
	Val is V^E,
	evaluate_var(Ov, Om, Oval),
	Value is Val * Oval.

evaluate_var([v(E, S) | Ov], [map(D, _) | Om], Value) :-
	D \= E,
	!,
	evaluate_var([v(E, S) | Ov], Om, Value).

evaluate_var([], _, 1).

%%	polytimes(P1, P2, R)
%	true if R is the result grlex sorted of P1 * P2
%	P(1|2) could be a monomial, a polynomial already parsed or
%	not parsed


polytimes(Poly1, Poly2, poly(Result)) :-
	as_polynomial(Poly1, P1),
	as_polynomial(Poly2, P2),
	mul(P1, P2, TResult),
	sort_poly(TResult, SResult),
	sum(poly(SResult), poly([]), SSum),
	simplify_coeff(SSum, CSResult),
	simplify_exp(CSResult, Result).

%%	mul(P1, P2, R)
%	R is the list of monomials of P3 where
%	P3 = P1 * P2
%	P1 and P2 must be parsed polynomials

mul(poly(M1), poly(M2), Result) :-
	mul_monos(M1, M2, Result).

%%	mul_monos(M, M2, R)
%	R is the result of Mul
%	M and M2 are list of monomials

mul_monos([m(C, TD, V) | Omono], M2, Result) :-
	mul_mono(m(C, TD, V), M2, MResult),
	mul_monos(Omono, M2, OResult),
	append(MResult, OResult, Result).

mul_monos([], _, []).

%%	mul_mono(M, M2, R)
%	multiply a single monomial M with a list of monomials M2
%	R is the result of this multiplication

mul_mono(m(C, TD, V), [m(C2, TD2, V2) | Om], [m(RC, RTD, RVar) | Result]) :-
	!,
	RC is C * C2,
	RTD is TD + TD2,
	append(V, V2, Var),
	sort(2, @=<, Var, SVar),
	mul_var(SVar, RVar),
	mul_mono(m(C, TD, V), Om, Result).

mul_mono(_, [], []) :-
	!.

%%	mul_var(V, RV)
%	RV is the list of variables resulted from the
%	multiplication of the variables that appear in V

mul_var([v(E, S), v(E2, S) | Ov], Rvar) :-
	!,
	RE is E + E2,
	mul_var([v(RE, S) | Ov], Rvar).

mul_var([v(E, S), v(E2, S2) | Ov], [v(E, S) | Rvar]) :-
	!,
	S \= S2,
	mul_var([v(E2, S2) | Ov], Rvar).

mul_var([v(E, S)], [v(E, S)]) :-
	!.

mul_var([], []) :-
	!.

%%% end of file mvpoli.pl
















