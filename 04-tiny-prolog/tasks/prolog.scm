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

(define (wrap x) (if (and (pair? x) (none? x)) (none) (some x)))
(define (unwrap x) (if (and (pair? x) (none? x)) (none) (some-consume x)))

(define (opt-bind x act) (if (some? x)
                       (act)
                       (none)))
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
(define (var-var? v v1) (and (var? v1) (var? v)))

;; ----------DICT----------

;; (define (lookup dict s)
;;   (cond ((null? dict) '())
;;         ((string=? (caar dict) s) (cdar dict))
;;         (else (lookup (cdr dict) s))))

(define (lookup dict s)
  (cond ((null? dict) '())
        ((string=? (caar dict) s) (cadar dict))
        (else (lookup (cdr dict) s))))

;; (define (set-dict dict s term)
;;   (cond ((null? dict) (cons (cons s term) '()))
;;         ((string=? (caar dict) s) (cons (cons s term) (cdr dict)))
;;         (else (cons (car dict) (set-dict (cdr dict) s term)))))

(define (set-dict dict s term)
  ;; (cond ((null? dict) (cons (cons s term) '()))
  (cond ((null? dict) (cons (list s term) '()))
        ((string=? (caar dict) s) (cons (list s term) (cdr dict)))
        (else (cons (car dict) (set-dict (cdr dict) s term)))))

(define (dict-append dict p)
  (if (null? p)
    dict
    (set-dict dict (car p) (cadr p))))

;; ----------SUBST----------

;; (define (substitute subst term)
;;   (display "SUBST ") (display subst) (display " ") (display term) (newline)
;;   (cond ((null? subst) term)
;;         ((var? term) (let ((v (lookup subst (term-string term))))
;;                        (if (null? v) term (substitute subst v))))
;;         ((pred? term) (pred (pred-string term) (map (lambda (t) (substitute subst t)) (pred-terms term))))
;;         (else term)))

(define (substitute subst term)
  (display "SUBST ") (display subst) (display " ") (display term) (newline)
  (cond ((null? subst) term)
        ((var? term) (let ((v (lookup subst (term-string term))))
                       (if (null? v) term v)))
        ((pred? term) (pred (pred-string term) (map (lambda (t) (substitute subst t)) (pred-terms term))))
        (else term)))

(define (substitute-subst new-subst subst)
  (display new-subst) (display " ") (display subst) (newline)
  (cond ((or (null? new-subst) (some? new-subst)) (display "fuck\n") subst)
        ((or (null? subst) (some? subst)) (display "HUH???\n") new-subst)
        (else (map (lambda (str-term) (display str-term) (cons (car str-term) (substitute new-subst (cdr str-term)))) subst))))

(define (substitute-terms subst terms) (map (lambda (term) (substitute subst term)) terms))


;; ----------UNIFY----------

(define (unify-lists l1 l2)
  (cond ((and (null? l1) (null? l2)) (some '()))
        ((or (null? l1) (null? l2)) (error "???")) ;; lists cannot be unified
        (else (let* ((h1 (car l1)) (h2 (car l2)) (t1 (cdr l1)) (t2 (cdr l2)) (s1 (unify h1 h2)))
                (if (some? s1)
                  (let* ((s1 (unwrap s1)) (t1 (substitute-terms s1 t1)) (t2 (substitute-terms s1 t2)) (s2 (unify-lists t1 t2)))
                    (if (some? s2)
                      (wrap (substitute-subst (unwrap s2) (dict-append '() s1)))
                      (none)))
                  (none)) ))))


(define (unify t1 t2)
  (display "UNIFY ") (display t1) (display " ") (display t2) (newline)
  (cond ((match-atoms? t1 t2) (some '()))
        ((match-preds? t1 t2) (wrap (unwrap (unify-lists (pred-terms t1) (pred-terms t2)))))
        ((var-var? t1 t2) (wrap (list (list (term-string t1) t2) (list (term-string t2) t1))))
        ((var-term? t1 t2) (wrap (list (list (term-string t1) t2))))
        ((var-term? t2 t1) (wrap (list (list (term-string t2) t1))))
        (else (none))))

;; (define p2-5a (pred "add" (list (pred "succ" (list (var "X"))) (var "X"))))
;; (define p2-5b (pred "add" (list (var "Y") (var "Z"))))

;; (define p2-4a (pred "loves" (list (var "X") (atom "narcissus"))))
;; (define p2-4b (pred "loves" (list (var "Y") (var "X"))))


;; -----------EXERCISES----------

;; -----02-----
(define p2-1a (pred "loves" (list (atom "narcissus") (atom "narcissus"))))
(define p2-1b (pred "loves" (list (var "X") (var "X"))))

(define p2-2a (pred "loves" (list (atom "odysseus") (atom "penelope"))))
(define p2-2b (pred "loves" (list (var "X") (var "X"))))

(define p2-3a (pred "add" (list (atom "zero") (pred "succ" (list (atom "zero"))))))
(define p2-3b (pred "add" (list (var "Y") (pred "succ" (list (var "Y"))))))

(define p2-4a (pred "loves" (list (var "X") (atom "narcissus"))))
(define p2-4b (pred "loves" (list (var "Y") (var "X"))))

(define p2-5a (pred "add" (list (pred "succ" (list (var "X"))) (var "X"))))
(define p2-5b (pred "add" (list (var "Y") (var "Z"))))

