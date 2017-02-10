A library for the manipulation of multivariate polynomial, 
can perform various operations such as:
- coefficients extraction
- total degree calculation of a given polynomial
- evaluation of a polynomial in a point v=<v1...vk> 
  (k is the number of variables in the polynomial)
- sum, difference, multiplication of polynomials

documentation can be found in doc-mvpoli.txt


Examples

-- coefficients

(let ((poly '(+ (log 3)
                (* 4 x)
                (* (sin 90) y q)
                (* 42 pizza))))
    (coefficients poly))
(1.0986123 42 4 0.89399666)

-- variables

(let ((poly '(+ (log 3)
                (* 4 x)
                (* (sin 90) y q)
                (* 42 pizza))))
    (variables poly))
(PIZZA Q X Y)

-- monomials

(let ((poly '(+ (log 3)
             (* 4 x)
             (* (sin 90) y q)
             (* 42 pizza))))
    (monomials poly))
((M 1.0986123 0 NIL) (M 42 1 ((V 1 PIZZA))) (M 4 1 ((V 1 X))) (M 0.89399666 2 ((V 1 Q) (V 1 Y))))

-- maxdegree

(let ((poly '(+ (log 3)
                (* 4 x)
                (* (sin 90) y q)
                (* 42 pizza))))
    (maxdegree poly))
2

-- mindegree

(let ((poly '(+ (log 3)
                (* 4 x)
                (* (sin 90) y q)
                (* 42 pizza))))
    (mindegree poly))
0

-- polyplus

(let ((p1 '(+ (* a)
              (* b)
              (* c)
              (* (expt d 3))
              (* (log 1))))
      (p2 '(+ (* 3 a)
              (* 5 b)
              12
              (* 4 z f)
              (* 5 (expt d 2)))))
    (polyplus p1 p2))
(POLY ((M 12.0 0 NIL) 
	   (M 4 1 ((V 1 A))) 
	   (M 6 1 ((V 1 B))) 
	   (M 1 1 ((V 1 C))) 
	   (M 5 2 ((V 2 D))) 
	   (M 4 2 ((V 1 F) (V 1 Z))) 
	   (M 1 3 ((V 3 D)))))

-- polyminus

(let ((p1 '(+ (* a)
              (* b)
              (* c)
              (* (expt d 3))
              (* (log 1))))
      (p2 '(+ (* 3 a)
              (* 5 b)
              12
              (* 4 z f)
              (* 5 (expt d 2)))))
    (polyminus p1 p2))
(POLY ((M -12.0 0 NIL) 
	  (M -2 1 ((V 1 A)))
	  (M -4 1 ((V 1 B))) 
	  (M 1 1 ((V 1 C))) 
	  (M -5 2 ((V 2 D)))
	  (M -4 2 ((V 1 F) (V 1 Z))) 
	  (M 1 3 ((V 3 D)))))
	  
-- polytimes

(let* ((p (as-polynomial '(+ (* a) (* b))))
                   (r1 (polytimes p p))
                   (r2 (polytimes p r1))                                                                        
                   (r3 (polytimes p r2))
                   (s1 (pprint-polynomial r1))
                   (s2 (pprint-polynomial r2))
                   (s3 (pprint-polynomial r3))))
2ab + a^2 + b^2
3ab^2 + 3a^2b + a^3 + b^3
4ab^3 + 6a^2b^2 + 4a^3b + a^4 + b^4
NIL

-- as-monomial

(as-monomial '(* (+ (* 42 25) 23) (expt y 2)))
(M 1073 2 ((V 2 Y)))

-- as-polynomial

(as-polynomial '(+ (* x) (* x) (* x)))
(POLY ((M 3 1 ((V 1 X)))))

-- polyval

(polyval '(POLY ((M 12.0 0 NIL) 
				 (M 4 1 ((V 1 A)))
				 (M 6 1 ((V 1 B))) 
				 (M 1 1 ((V 1 C))) 
				 (M 5 2 ((V 2 D))) 
				 (M 4 2 ((V 1 F) (V 1 Z))) 
				 (M 1 3 ((V 3 D)))))
		 '(1 2 3 4 5 6)) 
295.0

-- pprint-polynomial

(pprint-polynomial '(+ (* 12 (expt x 0))
                       (* 42 (expt y 3) (expt a 123))
                       (* b)
                       (* q)
                       (* (expt bar 23))))
12 + b + q + bar^23 + 42a^123y^3
NIL

			   
				   