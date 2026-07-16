(define (ty-variable var) (cons 'var var))
(define (ty-bool) (cons 'bool '()))
(define (ty-number) (cons 'num '()))
(define (ty-list l) (cons 'list l))

(define (get-type num) (car num))
(define (get-next l) (cdr l))

(define (eq-type? num t) (equal? (get-type num) t))
;; (define (eq-type? num t) (display num) (display " aha ") (display t) (newline) (equal? (get-type num) t))
(define (ty-number? num) (eq-type? num 'num))
(define (ty-bool? num) (eq-type? num 'bool))
(define (ty-var? num) (eq-type? num 'var))
(define (ty-list? num) (eq-type? num 'list))
(define (primitive? x) (or (ty-number? x) (ty-bool? x)))

(define (same-var? v1 v2)
  (if (and (ty-var? v1) (ty-var? v2))
    (string=? (cdr v1) (cdr v2))
    #f))

(define (unlist l)
  (if (ty-list? l)
    (get-next l)
    l))

(define (atom? x) (not (or (pair? x) (null? x) (list? x))))
;; -----------------------------------------------
(define (constant n) (cons 'const n))
(define (binary s e1 e2) (cons 'binary (list s e1 e2)))
(define (iff e1 e2 e3) (cons 'if (list e1 e2 e3)))
(define (variable s) (cons 'variable s))

(define (constant? c) (eq-type? c 'const))
(define (binary? t) (eq-type? t 'binary))
(define (if? t) (eq-type? t 'if))
(define (variable? t) (eq-type? t 'variable))

(define (binary-s t) (cadr t))
(define (binary-e1 t) (caddr t))
(define (binary-e2 t) (cadddr t))

(define (if-e1 t) (cadr t))
(define (if-e2 t) (caddr t))
(define (if-e3 t) (cadddr t))

(define (var-name v) (cdr v))
;; -----------------------------------------------



(define (occurs-check? vcheck ty)
  (cond ((ty-list? vcheck) (occurs-check? (get-next vcheck) ty))
        ((ty-list? ty) (occurs-check? vcheck (get-next ty)))
        ((ty-var? vcheck) (same-var? vcheck ty))
        (else #f)))


(define (substitute v subst n)
  (cond ((primitive? n) n)
        ((ty-var? n) (if (same-var? v n) subst n))
        (else (ty-list (substitute v subst (get-next n))))))


;; dict = list <var, type>
(define (lookup dict i)
  ;; (display dict) (display " lookup ") (display i) (newline)
  (cond ((null? dict) #f)
        ((and (atom? i) (string=? (caar dict) i)) (cadar dict))
        ((not (ty-var? i)) i)
        ((same-var? (caar dict) i) (cadar dict))
        (else (lookup (cdr dict) i))))


(define (subst-type subst ty)
  (cond ((null? subst) ty)
        ((ty-list? ty) (ty-list (subst-type subst (get-next ty))))
        ((and (ty-var? ty) (lookup subst ty)) (subst-type subst (lookup subst ty)))
        ;; ((and (ty-var? ty) (lookup subst ty)) (display subst) (display " idk ") (display ty) (newline) (subst-type subst (lookup subst ty)))
        (else ty)))

;; (define (subst-constrs subst cs)
;;   (map (lambda (x) (subst-type subst x)) cs))

(define (subst-constrs subst cs)
  ;; (display "sub-cons   ") (display subst) (display "  ") (display cs) (newline)
  (map (lambda (x) (list (subst-type subst (car x)) (subst-type subst (cadr x)))) cs))


(define (solve-help v n constraints)
  (if (occurs-check? v n)
    (error "cannot be solved (occurs check)")
    (let* ((dict (list (list v n)))
           (constraints (subst-constrs dict constraints))
           (subst (solve constraints))
           (n (subst-type subst n)))
      (cons (list v n) subst))))

(define (solve constraints)
  ;; (display constraints) (newline)
  (if (null? constraints) '()
    (let
      ((n1 (caar constraints)) (n2 (cadar constraints)) (rest (cdr constraints)))
      (cond ((and (ty-list? n1) (ty-list? n2)) (solve (cons (list (get-next n1) (get-next n2)) rest)))
             ((and (primitive? n1) (primitive? n2)) (solve rest))
             ((or (and (ty-list? n2) (primitive? n1)) (and (ty-list? n1) (primitive? n2))) (error "cannot be solved"))
             ((ty-var? n1) (solve-help n1 n2 rest))
             ((ty-var? n2) (solve-help n2 n1 rest))))))


(define (generate ctx e)
  ;; (display ctx) (display " gen ") (display e) (newline)
  (cond ((constant? e) (list (ty-number) '()))
        ((and (binary? e) (string=? "+" (binary-s e)))
         (let* ((ts1 (generate ctx (binary-e1 e))) (ts2 (generate ctx (binary-e2 e))) (t1 (car ts1)) (s1 (cadr ts1)) (t2 (car ts2)) (s2 (cadr ts2)))
           (cond ((and (null? s1) (null? s2)) (list (ty-number) (list t1 (ty-number)) (list t2 (ty-number))))
                 ((null? s1) (list (ty-number) (cons s2 (list (list t1 (ty-number)) (list t2 (ty-number))))))
                 ((null? s2) (list (ty-number) (cons s1 (list (list t1 (ty-number)) (list t2 (ty-number))))))
                 (else (list (ty-number) (cons (list s1 s2) (list (list t1 (ty-number)) (list t2 (ty-number)))))))))
        ((and (binary? e) (string=? "=" (binary-s e)))
         (let* ((ts1 (generate ctx (binary-e1 e))) (ts2 (generate ctx (binary-e2 e))) (t1 (car ts1)) (s1 (cadr ts1)) (t2 (car ts2)) (s2 (cadr ts2)))
           (cond ((and (null? s1) (null? s2)) (list (ty-bool) (list t1 (ty-number)) (list t2 (ty-number))))
                 ((null? s1) (list (ty-bool) (cons s2 (list (list t1 (ty-number)) (list t2 (ty-number))))))
                 ((null? s2) (list (ty-bool) (cons s1 (list (list t1 (ty-number)) (list t2 (ty-number))))))
                 (else (list (ty-bool) (cons (list s1 s2) (list (list t1 (ty-number)) (list t2 (ty-number)))))))))
        ((binary? e) (error "operator unsupported\n"))
        ((variable? e) (list (lookup ctx (var-name e)) '()))
        ((if? e)
         (let* ((econd (generate ctx (if-e1 e))) (etrue (generate ctx (if-e2 e))) (efalse (generate ctx (if-e2 e)))
                                                 (tt (car etrue)) (st (cadr etrue)) (tf (car efalse)) (sf (cadr efalse)) (tc (car econd)) (sc (cadr econd)))
           (list tt (cons (append sc st sf) (list (list tc (ty-bool)) (list tt tf))))
           ))))



  ;; --------------------------------------------------------------------------------------------------------

;; Can be solved ('a = number, 'b = list<number>)
(define b1 (list (list (ty-list (ty-variable "a")) (ty-list (ty-number))) (list (ty-variable "b") (ty-list (ty-variable "a")))))
;; Cannot be solved (list<'a> <> bool)
(define b2 (list (list (ty-list (ty-variable "a")) (ty-variable "b")) (list (ty-variable "b") (ty-bool))))  ;; should fail
;; Can be solved ('a = number, 'b = list<number>)
(define b3 (list (list (ty-list (ty-variable "a")) (ty-variable "b")) (list (ty-variable "b") (ty-list (ty-number)))))
;; Cannot be solved ('a <> list<'a>)
(define b4 (list (list (ty-list (ty-variable "a")) (ty-variable "a"))))  ;; should fail


(define e1 (binary "=" (variable "x") (binary "+" (constant 10) (variable "x"))))
(define tcs1 (generate (list (list "x" (ty-variable "a"))) e1))
(define t1 (car tcs1)) (define cs1 (cadr tcs1))

(define e2 (iff (variable "x") (binary "+" (constant 2) (constant 1)) (variable "y")))
(define tcs2 (generate (list (list "x" (ty-variable "a")) (list "y" (ty-variable "b"))) e2))
(define t2 (car tcs2)) (define cs2 (cadr tcs2))
