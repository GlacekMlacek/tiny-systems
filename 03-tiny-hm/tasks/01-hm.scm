(define (make-zero) (cons 'zero '()))
(define (make-number num) (cons 'succ num))
(define (make-var var) (cons 'var var))
(define (succ num) (make-number num))
(define (zero) (make-zero))
(define (variable var) (make-var var))

(define (get-type num) (car num))
(define (get-next num) (cdr num))

(define (eq-type? num t) (equal? (get-type num) t))
(define (succ? num) (eq-type? num 'succ))
(define (zero-t? num) (eq-type? num 'zero))
(define (var? num) (eq-type? num 'var))



(define (occurs-check? v n)
  (cond ((succ? n) (occurs-check? v (get-next n)))
        ;; ((var? n) (string=? (get-thing n) (get-thing v)))
        ((var? n) (string=? (get-next n) (get-next v)))
        (else #f)))



(define (substitute v subst n)
  (cond ((succ? n) (make-number (substitute v subst (get-next n))))
        ((and (var? n) (string=? (get-next n) (get-next v))) subst)
        (else n)))


(define (substitute-constraints v subst constraints)
  (map (lambda (x) (substitute v subst x)) constraints))


(define (substitute-all subst n)
  (if (null? subst) n
       (fold (lambda (vtn num) (substitute (car vtn) (cdr vtn) num)) n subst)))



(define (solve-help v n constraints)
  (if (occurs-check? v n)
    (error "cannot be solved (occurs check)")
    (let* ((constraints (substitute-constraints v n constraints))
           (subst (solve constraints))
           (n (substitute-all subst n)))
      (cons (list v n) subst))))

(define (solve constraints)
  (if (null? constraints) '()
    (let
      ((n1 (car constraints)) (n2 (cadr constraints)) (rest (cddr constraints)))
      (cond ((and (succ? n1) (succ? n2)) (solve (cons (get-next n1) (cons (get-next n2) rest))))
             ((and (zero-t? n1) (zero-t? n2)) (solve rest))
             ((or (and (succ? n2) (zero-t? n1)) (and (succ? n1) (zero-t? n2))) (error "cannot be solved"))
             ((var? n1) (solve-help n1 n2 rest))
             ((var? n2) (solve-help n2 n1 rest))))))




  ;; --------------------------------------------------------------------------------------------------------
  (define a1 (list (succ (variable "x")) (succ (zero))))
  (define a2 (list (succ (succ (zero))) (succ (zero)))) ;; should fail
  (define a3 (list (succ (succ (variable "x"))) (succ (zero)))) ;; should fail
  (define a4 (list (succ (variable "x")) (succ (zero)) (variable "y") (succ (variable "x"))))
  (define a5 (list (variable "x") (succ (succ (variable "z"))) (succ (variable "z")) (succ (zero))))
  (define a6 (list (variable "x") (succ (variable "x"))))
