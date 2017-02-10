;;;; 807391 Mammana Lorenzo
;;;; Nessuna collaborazione

;;;; mvpoli.lisp


(defun is-monomial (m)
  "True if M is a correct monomial '(m Coeff TD Varpowers)."
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-total-degree m))
             (vps (monomial-vars-and-powers m)))
         (and (integerp mtd)
              (>= mtd 0)
              (eq (calculate-total-degree vps) mtd)
              (listp vps)
              (every #'is-varpower vps)))))

(defun is-varpower (vp)
  "True if VP is a correct varpower '(v Exp Sym)."
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp)))
         (and (integerp p)
              (>= p 0) 
              (symbolp v)))))

(defun is-polynomial (p)
  "True if P is a correct polynomial '(poly ListMono)."
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (poly-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


(defun mono-vars (m)
  "Return the varpowers list of the monomial M.
 M must be a parsed monomial"
  (if (is-monomial m)
      (fourth m)
    (error "L'espressione non è un monomio")))

(defun mono-coeff (m)
  "Return the coefficient of the monomial M.
 M must be a parsed monomial"
  (if (is-monomial m)
      (second m)
    (error "L'espressione non è un monomio")))

(defun mono-td (m)
  "Return the total degree of the monomial M.
 M must be a parsed monomial"
  (if (is-monomial m)
      (third m)
    (error "L'espressione non è un monomio")))

(defun monomial-total-degree (m)
  "Deprecated - Same as mono-
 Argument M A parsed monomial.t."
  (third m))

(defun monomial-vars-and-powers (m)
  "Deprecated - same as mono-vars.
 Argument M A parsed monomial."
  (fourth m)) 

(defun varpower-power (vp)
  "Return the power of the varpower VP."
  (second vp))

(defun varpower-symbol (vp)
  "Return the symbol of the varpower VP."
  (third vp))

(defun poly-monomials (p)
  "Return the list of monomials in the polynomial P."
  (second p))

(defun as-monomial (expression)
  "Parse an EXPRESSION into the corresponding sorted monomial.
 the EXPRESSION can be.
 '(* [coeff] var-expt*) |
 number |
 '()"
  (if (listp expression)
      (cond ((eq '* (first expression))
             (if (eq 0 (second expression))
                 (list 'm 0 0 nil) 
               (let ((c (extract-coeff (second expression))))
                 (cond ((eq c (second expression))
                        (let* ((vars (sort (extract-vars (cdr (cdr expression)))
                                           #'string-lessp
                                           :key #'third))
                               (td (calculate-total-degree vars))
                               (sim-vars (simplify-vars vars)))
                          (list 'm (eval c) td sim-vars)))
                       (t
                        (let* ((vars (sort (extract-vars (cdr expression))
                                           #'string-lessp
                                           :key #'third))
                               (td (calculate-total-degree vars))
                               (sim-vars (simplify-vars vars)))
                          (list 'm (eval c) td sim-vars)))))))
            (t
             (if (fboundp (first expression)) 
                 (list 'm (eval expression) 0 nil)
               (error "l'espressione non è un monomio"))))
    (list 'm expression 0 nil)))

(defun sort-monomials (listmono)
  "Return a list that contains LISTMONO elements sorted using #'sort-monomial."
  (if (eq nil listmono)
      ()
    (append (list (sort-monomial (first listmono)))
            (sort-monomials (rest listmono)))))

(defun sort-monomial (mono)
  "Sort the varpowers in MONO lexicographically
 MONO is a parsed monomial"
  (list (first mono)
        (second mono)
        (third mono)
        (sort (fourth mono)
              #'string-lessp
              :key #'third)))


;;; estra il coefficiente di un monomio
;;; 1 se non è numerico, altrimenti il numero/funzione
(defun extract-coeff (coeff)
  "Return 1 if COEFF is not a number, COEFF otherwise"
  (cond ((listp coeff)
         (if (and (not (eq 'expt (first coeff)))
                  (fboundp (first coeff))) 
             (if (numberp (apply (first coeff)
                                 (mapcar #'eval (rest coeff))))
                 coeff
               (error "Espressione non valida"))
           1))
        ((numberp coeff) coeff)
        (t 1)))

;;; estrae la lista di variabili di un mono nella forma
;;; (* [C] TD varlist) 
(defun extract-vars (varlist)
  "Return a list of varpowers based on the given VARLIST.
 VARLIST contains unparsed varpowers in the form [sym|(expt sym exp)]"
  (if (eq nil varlist)
      ()
    (if (listp (first varlist))
        (append (list (list 'v 
                            (third (first varlist)) 
                            (second (first varlist))))
                (extract-vars (rest varlist)))
      (append (list (list 'v 
			  '1 
			  (first varlist)))
              (extract-vars (rest varlist))))))

;;; calcola il grado totale di un monomio
;;; varlist è la lista di variabili nella forma (v E S)
(defun calculate-total-degree (varlist)
  "Return the total degree of VARLIST
 VARLIST is a list containing varpowers (v exp sym)"
  (if (eq varlist nil)
      0
    (+ (second (first varlist)) 
       (calculate-total-degree (rest varlist)))))   

;;; rimuove le variabili con coefficiente 0
;;; moltiplica le variabili con lo stesso simbolo
(defun simplify-vars (varlist)
  "Return a list that contains VARLIST elements without coefficient 0
 it also multiply the same varpowers in the list
 VARLIST is a list of varpowers (v exp sym)"
  (let ((scvar (simplify-coeff varlist)))
    (mul-var scvar)))

;;; riceve una lista di variabili nella forma (v E S)
;;; ritorna una lista di variabili semplificando quelle con lo
;;; stesso simbolo
(defun mul-var (varlist)
  "Multiply the same varpowers in VARLIST."
  (cond ((eq nil varlist)
         ())
        ((eq 1 (length varlist))
         varlist)
        (t
         (if (equal (third (first varlist))
                    (third (second varlist)))
             (let ((tmp (append (list (list 'v  
                                            (+ (second (first varlist))
                                               (second (second varlist)))
                                            (third (first varlist)))))))
               (mul-var (append tmp
                                (cdr (cdr varlist)))))
           (append (list (first varlist))
                   (mul-var (rest varlist)))))))

;;; rimuove dalla lista di variabili nella forma (v E S)
;;; quelle con E = 0                           
(defun simplify-coeff (varlist)
  "Remove varpowers with coefficient 0 from VARLIST."
  (cond ((eq nil varlist)
         ())
        ((eq (second (first varlist)) 0)
         (simplify-vars (rest varlist)))
        (t
         (append (list (first varlist))
                 (simplify-vars (rest varlist))))))
;; bisogna semplificare le variabili nel singolo monomio
(defun as-polynomial (expression)
  "Return the corresponding sorted polynomial of the given EXPRESSION
 EXPRESSION can be:
 a parsed polynomial (poly monoList)
 a parsed monomial (m C TD V)
 an unparsed expression"
  (if (listp expression)
      (cond ((eq 'poly (first expression))
             (if (is-polynomial expression)
                 (let ((monos (sort (sort-monomials (second expression))
                                    #'<
                                    :key #'third)))
                   (list 'poly
                         (remove-if-null 
                          (sum (reduce #'append
                                       (mapcar #'sort-poly
                                               (degree-split monos)))))))
               (error "l'espressione non è un polinomio")))
            ((eq 'm (first expression))
             (let ((mono (sort-monomial expression)))
               (list 'poly
                     (list (list (first mono)
                                 (second mono)
                                 (third mono)
                                 (simplify-vars (fourth mono)))))))
            (t
             (parse-polynomial expression)))
    (if (numberp (eval expression))
        (as-polynomial (append '(+)
                               (list expression)))
      (error "espressione non valida"))))

(defun parse-polynomial (expression)
  "Return the corresponding polynomial of the given EXPRESSION
 EXPRESSION can be:
 an unparsed polynomial (+ listMono*)
 an unparsed monomial (* [coefficient] varpowers*)
 a number"
  (cond ((and (listp expression)
              (eq '+ (first expression)))
         (let ((monos (sort (mapcar #'as-monomial
                                    (rest expression))
                            #'<
                            :key #'third)
                      ))
           (list 'poly
                 (remove-if-null 
                  (sum (reduce #'append
                               (mapcar #'sort-poly 
                                       (degree-split monos))))))))
        ((and (listp expression)
              (not (numberp (eval (first expression)))))
	 (parse-polynomial (append '(+)
				   (list expression))))
        (t
         (error "l'espressione non è un polinomio"))))

(defun sort-poly (monos)
  "Return MONOS grlex sorted."
  (sort monos #'string-lessp :key #'mono-to-string))

;;; converte le variabili del monomio in stringhe
;;; nella forma "S|ES|E..."
(defun mono-to-string (mono)
  "Return the string representation of MONO varpowers
 the string representation is Sym|ExpSym|Exp..."
  (to-string (fourth mono)))

;;; converte una variabile in stringa
;;; (v E S) -> "S|E"
(defun to-string (s)
  "Same as mono-to-string but S is a varpower list."
  (cond ((equal s nil)
         "")
        (t
         (concatenate 'string
                      (concatenate 'string 
                                   (write-to-string (third (first s)))
                                   "|"
                                   (write-to-string (second (first s))))
                      (to-string (rest s))))))

;;; divide una lista di monomi per grado
(defun degree-split (monolist)
  "Return MONOLIST split by degree
 MONOLIST must be grade sorted"
  (apply-degree-split (sort monolist
                            #'< 
                            :key #'third)))

(defun apply-degree-split (monolist)
  "Return MONOLIST split by degree
 MONOLIST must be grade sorted"
  (cond ((eq nil monolist)
         ()) 
        (t
         (let ((degs (test-equal monolist (third (first monolist)))))
           (append (list degs)
                   (apply-degree-split (remove-if (constantly t) 
                                                  monolist
                                                  :count (length degs))))))))

;;; crea una lista di monomi con lo stesso grado
(defun test-equal (monolist td)
  "Return a list that contains all the monomials in MONOLIST with the same TD."
  (cond ((or (eq nil monolist) 
             (not (eq (third (first monolist)) td)))
         ())
        ((eq (third (first monolist)) td)
         (append (list (first monolist))
                 (test-equal (rest monolist) td)))))

;;; esegue la somma tra due polinomi
(defun polyplus (poly1 poly2)
  "Return the polynomial representation of the sum between POLY1 and POLY2."
  (let ((p1 (as-polynomial poly1))
        (p2 (as-polynomial poly2)))
    (list 'poly
          (remove-if-null (sum (sort (append (second p1)
                                             (second p2))
                                     #'< 
                                     :key #'third))))))

(defun simplify (monos)
  "Simplify a list of monomials MONOS."
  (if (eq nil monos)
      ()
    (append (list (list 'm
                        (second (first monos))
                        (third (first monos))
                        (simplify-vars (mono-vars (first monos)))))
            (simplify (rest monos)))))

;;; richiama la somma dei monomi a due a due su tutto
;;; il polinomio
(defun sum (monos)
  "Sum a list of monomials MONOS."
  (let ((sort-to-sum (reduce #'append
                             (mapcar #'sort-poly 
                                     (degree-split monos)))))
    (let ((sum-monos (sum-monomials sort-to-sum)))
      (simplify sum-monos))))

;;; somma due monomi
(defun sum-monomials (monos)
  "Sum two by two the elements of the monomial list MONOS."
  (cond ((eq nil monos)
         ())
        ((eq 1 (length monos))
         monos)
        (t
         (if (equal (mono-vars (first monos))
                    (mono-vars (second monos)))
             (sum-monomials (append (list (list 'm
                                                (+ (mono-coeff (first monos))
                                                   (mono-coeff (second monos)))
                                                (mono-td (first monos))
                                                (mono-vars (first monos))))
                                    (remove-if (constantly t)
                                               monos
                                               :count 2)))
           (append (list (first monos))
                   (sum-monomials (remove-if (constantly t)
                                             monos
                                             :count 1)))))))
;;; esegue la differenza tra due polinomi
(defun polyminus (poly1 poly2)
  "Return the polynomial representation of the difference between POLY1, POLY2."
  (let ((p1 (as-polynomial poly1))
        (p2 (negate (second (as-polynomial poly2)))))
    (list 'poly
          (remove-if-null (sum (sort (append (second p1)
                                             p2)
                                     #'<
                                     :key #'third))))))

;;; ritorna una lista di monomi con coefficiente invertito
(defun negate (poly)
  "Return the list of monomials of POLY but with negate coefficients."
  (if (eq nil poly)
      ()
    (append (list (list 'm
                        (- 0 (mono-coeff (first poly)))
                        (mono-td (first poly))
                        (mono-vars (first poly))))
            (negate (rest poly)))))

;;; stampa la rappresentazione grafica del polinomio
;;; ritorna NIL
(defun pprint-polynomial (polynomial)
  "Print the graphical representation of the given POLYNOMIAL."
  (let ((poly (as-polynomial polynomial))) 
    (format t "~(~@A~)~%" (pprint-mono (second poly)))))

;;; ritorna la rappresentazione grafica di una lista di monomi
(defun pprint-mono (listmono)
  "Print the graphical representation of the list of monomials LISTMONO."
  (cond ((eq nil listmono)
         "0")
        ((eq 1 (length listmono))
         (let ((c (print-coeff (first listmono)))
               (vars (print-vars (mono-vars (first listmono)))))
           (concatenate 'string
                        c
                        vars)))
        (t
         (let ((c (print-coeff (first listmono)))
               (vars (print-vars (mono-vars (first listmono))))
               (sign (print-sign (second listmono))))
           (concatenate 'string
                        c
                        vars
                        sign
                        (pprint-mono (rest listmono)))))))

;;; ritorna la rappresentazione grafica delle variabili di 
;;; un monomio 
(defun print-vars (vars)
  "Print the graphical representation of the list of varpowers VARS."
  (cond ((eq vars nil)
         "")
        ((eq 1 (second (first vars)))
         (concatenate 'string 
                      (write-to-string (third (first vars)))
                      (print-vars (rest vars))))
        (t
         (concatenate 'string
                      (write-to-string (third (first vars)))
                      "^"
                      (write-to-string (second (first vars)))
                      (print-vars (rest vars))))))

;;; ritorna la rappresentazione grafica del coefficiente
;;; di un monomio        
(defun print-coeff (mono)
  "Print the graphical representation of the coefficient of MONO
 MONO is a parsed monomial"
  (cond ((eq 1 (mono-coeff mono))
         "")
        ((eq -1 (mono-coeff mono))
         " - ")
        (t
         (write-to-string (mono-coeff mono)))))

;;; ritorna la rappresentazione grafica del segno
;;; di un monomio
(defun print-sign (mono)
  "Print the sign of a given parsed monomial MONO."
  (if (> (second mono) 0)
      " + "
    " - "))

;;; ritorna la lista di variabili nel monomio
(defun varpowers (monomial)
  "Return the list of varpowers of MONOMIAL
 MONOMIAL must be a parsed monomial"
  (if (is-monomial monomial)
      (fourth monomial)
    (varpowers (as-monomial monomial))))

;;; ritorna la lista dei simboli di variabile
(defun get-vars (vars)
  "Return the list of varpowers symbol that appears in VARS
 VARS must be a varpowers list"
  (if (eq nil vars)
      ()
    (append (list (third (first vars)))
            (get-vars (rest vars)))))

;;; ritorna la lista di simboli variabili del monomio
(defun vars-of (monomial)
  "Same as get-vars but the input MONOMIAL is a parsed monomial."
  (if (is-monomial monomial)
      (get-vars (fourth monomial))
    (vars-of (as-monomial monomial))))

;;; ritorna il grado totale del monomio
(defun monomial-degree (monomial)
  "Return the total degree of the given MONOMIAL
 MONOMIAL must be a parsed monomial"
  (if (is-monomial monomial)
      (third monomial)
    (monomial-degree (as-monomial monomial))))

;;; ritorna il coefficiente del monomio
(defun monomial-coefficient (monomial)
  "Return the coefficient of the given MONOMIAL
 MONOMIAL must be a parsed monomial"
  (if (is-monomial monomial)
      (second monomial)
    (monomial-coefficient (as-monomial monomial))))

;;; ritorna la lista di coefficienti presenti nel polinomio
(defun coefficients (polynomial)
  "Return the list of coefficients that appears in the given POLYNOMIAL
 POLYNOMIAL can be already parsed or unparsed"
  (let ((poly (as-polynomial polynomial)))
    (if (eq nil (second poly))
        '(0)
      (mapcar 'monomial-coefficient (second poly)))))

;;; ritorna la lista di variabili presenti nel polinomio
;;; in ordine lessicografico
(defun variables (polynomial)
  "Return the list of variables lex sorted that appears in POLYNOMIAL."
  (let ((poly (as-polynomial polynomial)))
    (let ((varlist (mapcar 'varpowers (second poly))))
      (remove-duplicates (sort (reduce 'append
                                       (mapcar 'get-varsym varlist))
                               #'string-lessp)))))

;;; ritorna la lista di simboli di variabile  contenuti nel polinomio
(defun get-varsym (vars)
  "Return the list of varpowers symbols that appear in VARS
 VARS must be a list of parsed variables"
  (if (eq nil vars)
      ()
    (append (list (third (first vars)))
            (get-varsym (rest vars)))))

;;; ritorna la lista di monomi contenuti nel polinomio
(defun monomials (polynomial)
  "Return the list of monomials of the given POLYNOMIAL."
  (let ((poly (as-polynomial polynomial)))
    (second poly)))

;;; ritorna il grado massimo dei monomi in un polinomio
(defun maxdegree (polynomial)
  "Return the maximum degree of the given POLYNOMIAL."
  (let ((poly (as-polynomial polynomial)))
    (let ((degrees (mapcar 'monomial-degree
                           (second poly))))
      (if (eq nil degrees)
          0
        (first (sort degrees #'>))))))

;;; ritorna il grado minimo dei monomi in un polinomio
(defun mindegree (polynomial)
  "Return the minimum degree of the given POLYNOMIAL."
  (let ((poly (as-polynomial polynomial)))
    (let ((degrees (mapcar 'monomial-degree
                           (second poly))))
      (if (eq nil degrees)
          0
        (first (sort degrees #'<))))))

;;; Il predicato polyval é vero quanto Value contiene il valore
;;; del polinomio Polynomial (che può anche essere un monomio), 
;;; nel punto n-dimensionale rappresentato dalla lista VariableValues,
;;; che contiene un valore per ogni variabile ottenuta
;;; con il predicato variables/2. 
(defun polyval (expression varlist)
  "Return the value of the given EXPRESSION in the n-dimensional point
 represented by VARLIST that contains at least a value for every variables
 that appear in EXPRESSION"
  (let* ((poly (as-polynomial expression))
         (vars (variables poly)))
    (eval-poly (second poly)
               vars 
               (resize-list varlist
                            (length vars)))))

(defun resize-list (lista n)
  "Return a list that contains LISTA elements in number <=N." 
  (cond ((eq nil lista)
         ())
        ((eq 0 n)
         ())
        (t
         (append (list (first lista))
                 (resize-list (rest lista)
                              (- n 1))))))

(defun eval-poly (poly vars varval)
  "Return the result of polyval
 POLY is a parsed polynomial
 VARS is the result of (variables POLY)
 VARVAL contains VARS numeric values"
  (if (eq nil poly)
      0
    (+ (eval-mono (first poly) vars varval)
       (eval-poly (rest poly) vars varval))))

(defun eval-mono (mono vars varval)
  "Return the value of the monomial list MONO
 VARS contains a list of variables
 VARVAL contains the numeric values of VARS"
  (if (eq nil (fourth mono))
      (second mono) ; ritorno il coeff se var nulle
    (* (second mono)
       (eval-vars (fourth mono) vars varval))))

(defun eval-vars (vars varsym varval)
  "Return the value of the varpowers list VARS
 VARSYM contains a list of variables
 VARVAL contains the numeric values of VARSYM"
  (if (eq vars nil)
      1
    (* (eval-var (first vars) varsym varval)
       (eval-vars (rest vars) varsym varval))))

(defun eval-var (var varsym varval)
  (if (eq varsym nil)
      1
    (if (eq (third var) (first varsym))
        (* (expt (first varval) (second var))
           (eval-var var (rest varsym) (rest varval)))
      (eval-var var (rest varsym) (rest varval)))))

(defun polytimes (poly1 poly2)
  "Return the polynomial representation of the multiplication between
 POLY1 and POLY2
 POLY1/2 can be already parsed or unparsed polynomials"
  (let ((p1 (as-polynomial poly1))
        (p2 (as-polynomial poly2)))
    (let ((mulmonos (mul-monos (second p1) (second p2))))
      (list 'poly
            (remove-if-null 
             (sum (reduce #'append 
                          (mapcar #'sort-poly
                                  (degree-split (sort-monomials
						 mulmonos))))))))))

(defun mul-monos (monos1 monos2)
  "Return the result of the multiplication between MONOS1 and MONOS2
 MONOS1 and MONOS2 are lists of monomials"
  (if (eq nil monos1)
      ()
    (append (mul-mono (first monos1) monos2)
            (mul-monos (rest monos1) monos2))))

(defun mul-mono (mono monos2)
  (if (eq monos2 nil)
      ()
    (let ((var1 (copy-list (fourth mono)))
          (var2 (copy-list (fourth (first monos2)))))
      (append (list (list 'm
                          (* (second mono)
                             (second (first monos2)))
                          (+ (third mono)
                             (third (first monos2)))
                          (mul-var (sort (append var1
                                                 var2)
                                         #'string-lessp
                                         :key #'third))))
              (mul-mono mono (rest monos2))))))

(defun remove-if-null (listMono)
  "Return the multiplication of the monomials in LISTMONO"
  (if (eq listMono nil)
      ()
    (if (eq 0 (second (first listMono)))
        (remove-if-null (rest listMono))
      (append (list (first listMono))
              (remove-if-null (rest listMono))))))


;;;; end of file mvpoli.lisp
