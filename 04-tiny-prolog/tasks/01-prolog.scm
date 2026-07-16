;; ------TERMS------
(define (atom s) (cons 'atom s))
(define (var s) (cons 'var s))
(define (pred s terms) (cons 'pred (list s terms)))

(define (term-string term) (cdr term))
(define (pred-string predic) (cadr predic))
(define (pred-terms predic) (caddr predic))

;; -----OPTION-----
(define (some x) (cons 'some x))
(define (none) (cons 'none '()))

(define (some? x) (equal? (car x) 'some))
(define (none? x) (equal? (car x) 'none))

(define (some-consume x) (cdr x))

;; (define (option-match opt) (if () () ()))

;; ------CLAUSE------
(define (clause head body) (cons 'clause (list head body)))

(define (program clause-list) (cons 'program clause-list))


(define (fact p) (clause p '()))
(define (rule p b) (clause p b))


(define (match-type? term-clause type) (equal? (car term-clause) type))
(define (match-types? tc1 tc2 type) (and (match-type? tc1 type) (match-type? tc2 type)))
(define (match-strings? t1 t2) (string=? (term-string t1) (term-string t2)))
(define (atom? a) (match-type? a 'atom))
(define (var? v) (match-type? v 'var))
(define (pred? p) (match-type? p 'pred))
(define (term? t) (or (atom? t) (var? t) (pred? t)))

(define (match-atoms? a1 a2) (and (match-types? a1 a2 'atom) (match-strings? a1 a2)))
(define (match-preds? p1 p2) (and (match-types? p1 p2 'pred) (string=? (pred-string p1) (pred-string p2))))
(define (var-term? v t) (and (term? t) (var? v)))

;; ----------DICT----------
;;
;; (define (lookup dict s)
;;   (cond ((null? s) '())
;;         ((string=? (caar dict) s) (cdar dict))
;;         (else (lookup (cdr dict) s))))
;;
;; (define (set-dict dict s term)
;;   (cond ((null? s) (cons (cons s term) '()))
;;         ((string=? (caar dict) s) (cons (cons s term) (cdr dict)))
;;         (else (cons (car dict) (set-dict (cdr dict) s term)))))
;;
;; ;; ----------SUBST----------
;;
;; (define (substitute subst term)
;;   (cond ((var? term) (let ((v (lookup subst (term-string term))))
;;                        (if v (substitute subst v) term)))
;;         ((pred? term) (map (lambda (t) (substitute subst t)) (pred-terms term)))
;;         (else term)))
;;
;; (define (substitute-subst new-subst subst) ())
;;
;; (define (substitute-terms subst terms) (map (lambda (term) (substitute subst term)) terms))
;;
;;
;; ----------UNIFY----------

(define (unify-lists l1 l2)
  (cond ((and (null? l1) (null? l2)) (some '()))
        ((or (null? l1) (null? l2)) (error "???")) ;; lists cannot be unified
        (else (let* ((h1 (car l1)) (h2 (car l2))
                     (t1 (cdr l1)) (t2 (cdr l2))
                     (h (unify h1 h2)) (t (unify-lists t1 t2)))
                (if (and (some? t) (some? h))
                  (let ((h (some-consume h)) (t (some-consume t))) (some (append h t)))
                  (none))))))


(define (unify t1 t2)
  (cond ((match-atoms? t1 t2) (some '()))
        ((match-preds? t1 t2) (some (unify-lists (pred-terms t1) (pred-terms t2))))
        ((var-term? t1 t2) (some (list (term-string t1) t2)))
        ((var-term? t2 t1) (some (list (term-string t2) t1)))
        (else (none))))



;; -----------EXERCISES----------

;; -----01-----
;; X -> socrates
(define p1-1a (pred "human" (list (atom "socrates"))))
(define p1-1b (pred "human" (list (var  "X"))))
(define p1-1 (some-consume (unify p1-1a p1-1b)))

;; -> NONE
(define p1-2a (pred "human" (list (atom "socrates"))))
(define p1-2b (pred "mortal" (list (var "X"))))
(define p1-2 (some-consume (unify p1-2a p1-2b)))

;; X -> harry
(define p1-3a (pred "parent" (list (atom "charles") (atom "harry"))))
(define p1-3b (pred "parent" (list (atom "charles") (var  "X"))))
(define p1-3 (some-consume (unify p1-3a p1-3b)))

;; X -> charles; Y -> harry
(define p1-4a (pred "parent" (list (var "X") (atom "harry"))))
(define p1-4b (pred "parent" (list (atom "charles") (var "Y"))))
(define p1-4 (some-consume (unify p1-4a p1-4b)))

;; X -> succ succ zero
(define p1-5a (pred "succ" (list (pred "succ" (list (pred "succ" (list (atom "zero"))))))))
(define p1-5b (pred "succ" (list (var "X"))))
(define p1-5 (some-consume (unify p1-5a p1-5b)))

;; -> NONE
(define p1-6a (pred "succ" (list (pred "succ" (list (atom "zero"))))))
(define p1-6b (pred "succ" (list (atom "zero"))))
(define p1-6 (some-consume (unify p1-6a p1-6b)))


;; -----02-----

