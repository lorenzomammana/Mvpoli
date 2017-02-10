A library for the manipulation of multivariate polynomial, 
can perform various operations such as:
- coefficients extraction
- total degree calculation of a given polynomial
- evaluation of a polynomial in a point v=<v1...vk> 
  (k is the number of variables in the polynomial)
- sum, difference, multiplication of polynomials

Documentation can be found in doc-mvpoli.txt

Examples

-- coefficients

coefficients(log(3) + 4 * x + sin(90) * y * q + 42 * pizza, P).
P = [1.0986122886681098, 42, 4, 0.8939966636005579].

-- variables

variables(log(3) + 4 * x + sin(90) * y * q + 42 * pizza, P).
P = [pizza, q, x, y].

-- monomials

monomials(log(3) + 4 * x + sin(90) * y * q + 42 * pizza, P).
P = [m(1.0986122886681098, 0, []), m(42, 1, [v(1, pizza)]),
     m(4, 1, [v(1, x)]), m(0.8939966636005579, 2, [v(1, q), v(1, y)])].

-- maxdegree

maxdegree(log(3) + 4 * x + sin(90) * y * q + 42 * pizza, P).
P = 2.

-- mindegree

mindegree(log(3) + 4 * x + sin(90) * y * q + 42 * pizza, P).
P = 0.

-- polyplus

polyplus(a + b + c + d^3 + log(1), 3 * a + 5 * b + 12 + 4 * z * f + 5 * d^2, P), pprint_polynomial(P) ; true.
12.0 + 4a + 6b + c + 5d^2 + 4fz + d^3
P = poly([m(12.0, 0, []), m(4, 1, [v(1, a)]), m(6, 1, [v(1, b)]), 
          m(1, 1, [v(1, c)]), m(5, 2, [v(2, d)]), m(4, 2, [v(1, f), v(1, z)]), 
          m(1, 3, [v(3, d)])]) ;

-- polyminus

polyminus(a + b + c + d^3 + log(1), 3 * a + 5 * b + 12 + 4 * z * f + 5 * d^2, P), pprint_polynomial(P).
-12.0 -2a -4b + c -5d^2 -4fz + d^3
P = poly([m(-12.0, 0, []), m(-2, 1, [v(1, a)]), m(-4, 1, [v(1, b)]),
          m(1, 1, [v(1, c)]), m(-5, 2, [v(2, d)]), m(-4, 2, [v(1, f), v(1, z)]), 
          m(1, 3, [v(3, d)])]).

-- polytimes

polytimes(a + b, a + b, C1), polytimes(C1, a + b, C2), polytimes(C2, a + b, C3), pprint_polynomial(C1), pprint_polynomial(C2), pprint_polynomial(C3).
2ab + a^2 + b^2
3ab^2 + 3a^2b + a^3 + b^3
4ab^3 + 6a^2b^2 + 4a^3b + a^4 + b^4
C1 = poly([m(2, 2, [v(1, a), v(1, b)]), m(1, 2, [v(2, a)]), m(1, 2, [v(2, b)])]),
C2 = poly([m(3, 3, [v(1, a), v(2, b)]), m(3, 3, [v(2, a), v(1, b)]), m(1, 3, [v(3, a)]), m(1, 3, [v(3, b)])]),
C3 = poly([m(4, 4, [v(1, a), v(3, b)]), m(6, 4, [v(2, a), v(2, b)]), m(4, 4, [v(3, a), v(1, b)]), m(1, 4, [v(4, a)]), m(1, 4, [v(4, b)])]).

-- as_monomial

as_monomial(x + x + x, M).
false.

as_monomial((23 * 2 + (47 + 5)) * y^2, P).
P = m(98, 2, [v(2, y)]).

-- as_polynomial

as_polynomial(x + x + x, M).
M = poly([m(3, 1, [v(1, x)])]).

-- polyval

polyval(poly([m(12.0, 0, []), m(4, 1, [v(1, a)]), m(6, 1, [v(1, b)]), m(1, 1, [v(1, c)]), m(5, 2, [v(2, d)]), m(4, 2, [v(1, f), v(1, z)]), m(1, 3, [v(3, d)])]), [1, 2, 3, 4, 5, 6], V).
V = 295.0.

-- pprint_polynomial

pprint_polynomial(12 * x^0 + 42 * y^3 * a^123 + b + q + bar^23).
12 + b + q + bar^23 + 42a^123y^3
true.