(define (make-command command-type args) (cons command-type args))
(define (new-program) (list (list 0 (make-command 'nop '()))))

(define (make-print exprs) (make-command 'print exprs))
(define (make-run) (make-command 'run '()))
(define (make-goto num) (make-command 'goto num))
(define (make-gosub num) (make-command 'gosub num))
(define (make-assign expr) (make-command 'assign expr))
(define (make-if expr) (make-command 'if expr))
(define (make-poke expr) (make-command 'poke expr))
(define (make-clear) (make-command 'clear '()))
(define (make-stop) (make-command 'stop '()))
(define (make-input var) (make-command 'input var))
(define (make-return) (make-command 'return '()))


(define (make-val val) (make-command 'val val))
(define (make-const val) (make-command 'const val))
(define (make-var var) (make-command 'var var))
(define (make-func args) (make-command 'func args))


(define (make-program line expr) (cons line expr))


;; (define clear-str "\033[25l\033[2J\033[H")
(define clear-str "\033[2J\033[H")



(define (merge! l1 l2) (if (null? (cdr l1)) (set-cdr! l1 l2) (merge (cdr l1) l2)))
(define (insert! l line expr)
  (if (null? (cdr l))
    (set-cdr! l (list (list line expr)))
    (insert! (cdr l) line expr)))

(define (set-program! prog line expr)
  (cond ((equal? (caar prog) line) (set-car! (cdar prog) expr))
        ((null? (cdr prog)) (set-cdr! prog (list (list line expr))))
        (else (set-program! (cdr prog) line expr))))

(define (lookup l num)
  (cond ((null? l) #f)
        ((equal? (caar l) num) (cadar l))
        (else (lookup (cdr l) num))))

(define program (list (list 0 (make-command 'nop '()))))

;; (define (do-print arg) (display arg) (newline))
(define (do-print args) (if (null? args) (newline) (begin (display (car args)) (do-print (cdr args)))))
(define (poke x y str) (display (string-append "\033[" (number->string x) ";" (number->string y) "H" str)))
(define (clear-screen) (display clear-str))
(define (stack-push stack num) (cons num stack))

(define (evaluate env args)
  (let
    ((type (car args)) (content (cdr args)))
    (cond
      ((equal? 'const type) (evaluate content))
      ((equal? 'val type) content)
      ((equal? 'var type) (let ((var (lookup env content))) (if var var (error (string-append "Var '" content "' not found")))))
      ((equal? 'func type) (cond
                             ((equal? (car content) "-") (- (evaluate env (cadr content)) (evaluate env (caddr content))))
                             ((equal? (car content) "=") (equal? (evaluate env (cadr content)) (evaluate env (caddr content))))
                             ((equal? (car content) "||") (or (evaluate env (cadr content)) (evaluate env (caddr content))))
                             ((equal? (car content) "<") (< (evaluate env (cadr content)) (evaluate env (caddr content))))
                             ((equal? (car content) ">") (> (evaluate env (cadr content)) (evaluate env (caddr content))))
                             ((equal? (car content) "MIN") (min (evaluate env (cadr content)) (evaluate env (caddr content))))
                             ;; ((equal? (car content) "RND") (random (evaluate env (cadr content))))
                             ((equal? (car content) "RND") (random-integer (evaluate env (cadr content))))
                             (else (error (string-append "Function '" (car content) "' not implemented")))))
      (else (error (string-append "not implemented '" (symbol->string type) "' in eval"))))))

(define (run-cmd cmd args line prog env stack)
  ;; (display cmd) (display args) (newline)
    (cond
      ((equal? 'nop cmd) (run-h (+ 10 line) prog env stack))
      ;; ((equal? 'print cmd) (do-print (evaluate env (car args))) (run-h (+ 10 line) prog env))
      ((equal? 'print cmd) (do-print (map (lambda (arg) (evaluate env arg)) args)) (run-h (+ 10 line) prog env stack))
      ((equal? 'goto cmd) (run-h args prog env stack))
      ((equal? 'run cmd) (run-h 0 prog env stack))
      ((equal? 'assign cmd) (set-program! env (car args) (evaluate env (cadr args))) (run-h (+ 10 line) prog env stack))
      ((equal? 'if cmd) (if (evaluate env (car args)) (run-cmd (cadr args) (cddr args) (+ 10 line) prog env stack) (run-h (+ 10 line) prog env stack)))
      ((equal? 'poke cmd) (poke (evaluate env (car args)) (evaluate env (cadr args)) (evaluate env (caddr args))) (run-h (+ 10 line) prog env stack))
      ((equal? 'clear cmd) (clear-screen) (run-h (+ 10 line) prog env stack))
      ((equal? 'stop cmd) '())
      ((equal? 'input cmd) (set-program! env args (read)) (run-h (+ 10 line) prog env stack))
      ((equal? 'gosub cmd) (run-h args prog env (stack-push stack (+ 10 line))))
      ((equal? 'return cmd) (run-h (car stack) prog env (cdr stack)))
      (else (display cmd) (error "not implemented"))))


(define (run-h line prog env stack) (if (not (lookup prog line)) #f
  (let*
    ((expr (lookup prog line)) (cmd (car expr)) (args (cdr expr)))
    ;; (thread-sleep! 0.0001)
    (run-cmd cmd args line prog env stack))))


(define (make-runtime commands prog)
  (cond
    ((null? commands) #f)
    (else (set-program! prog (caar commands) (cdar commands)) (make-runtime (cdr commands) prog))))

(define (make-line line command) (cons line command))

(define (run prog env) (run-h 0 prog env '()))


;; -------------------------------- TESTS --------------------------------
(define shared-env '())
(define hello-once-prog (list (list 0 (make-command 'nop '()))))
(define hello-once (make-runtime (list (make-line 10 (make-print (list (make-val "this is basic\n"))))) hello-once-prog))

(define hello-frvr-p (list (list 0 (make-command 'nop '()))))
(define hello-frvr (make-runtime (list (make-line 10 (make-print (make-val "this is basic\n"))) (make-line 20 (make-goto 10))) hello-frvr-p))

(define ho2p (new-program))
(define ho2 (make-runtime (list (make-line 10 (make-print (make-val "this is NOT basic\n"))) (make-line 10 (make-print (make-val "this is basic\n")))) ho2p))

(define hf2p (new-program))
(define hello-frvr (make-runtime (list (make-line 20 (make-goto 10)) (make-line 10 (make-print (make-val "this is NOT basic\n"))) (make-line 10 (make-print (make-val "this is basic\n"))) ) hf2p))


(define tvp (new-program))
(define tve (new-program))
(define tv (make-runtime (list
                           (make-line 10 (make-assign (list "S" (make-val "this is basic\n"))))
                           (make-line 20 (make-assign (list "I" (make-val 1))))
                           (make-line 30 (make-assign (list "B" (make-func (list "=" (make-var "I") (make-val 1))))))
                           (make-line 40 (make-print (make-var "S")))
                           (make-line 50 (make-print (make-var "I")))
                           (make-line 60 (make-print (make-var "B"))))
                         tvp))

(define htp (new-program))
(define hte (new-program))
(define ht (make-runtime (list
                           (make-line 10 (make-assign (list "I" (make-val 10))))
                           (make-line 20 (make-if (cons (make-func (list "=" (make-var "I") (make-val 0))) (make-goto 60))))
                           (make-line 30 (make-print (make-val "THIS IS BASIC\n")))
                           (make-line 40 (make-assign (list "I" (make-func (list "-" (make-var "I") (make-val 1))))))
                           (make-line 50 (make-goto 20))
                           (make-line 60 (make-print (make-val ""))))
                         htp))

(define starsp (new-program))
(define starse (new-program))
(define stars (make-runtime (list
                              (make-line 10 (make-clear))
                              (make-line 20 (make-poke (list (make-func (list "RND" (make-val 60))) (make-func (list "RND" (make-val 20))) (make-val "*"))))
                              (make-line 30 (make-assign (list "I" (make-val 100))))
                              (make-line 40 (make-poke (list (make-func (list "RND" (make-val 60))) (make-func (list "RND" (make-val 20))) (make-val " "))))
                              (make-line 50 (make-assign (list "I" (make-func (list "-" (make-var "I") (make-val 1))))))
                              (make-line 60 (make-if (cons (make-func (list ">" (make-var "I") (make-val 1))) (make-goto 40))))
                              (make-line 70 (make-goto 20)))
                            starsp))


(define nimp (new-program))
(define nime (new-program))
(define nim  (make-runtime (list
                             (make-line  10 (make-assign (list "M" (make-val 20))))
                             (make-line  20 (make-print (list (make-val "THERE ARE ") (make-var "M") (make-val " MATCHES LEFT"))))
                             (make-line  30 (make-print (list (make-val "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND ")
                                                              (make-func (list "MIN" (make-var "M") (make-val 5)))
                                                              (make-val " MATCHES"))))
                             (make-line  40 (make-print (list (make-val "HOW MANY MATCHES DO YOU TAKE?"))))
                             (make-line  50 (make-input "P"))
                             (make-line  60 (make-if (cons (make-func (list "||" (make-func (list "<" (make-var "P") (make-val 1)))
                                                                      (make-func (list "||" (make-func (list ">" (make-var "P") (make-val 5)))
                                                                                 (make-func (list ">" (make-var "P") (make-var "M")))))))
                                                           (make-goto 40))))
                             (make-line  70 (make-assign (list "M" (make-func (list "-" (make-var "M") (make-var "P"))))))
                             (make-line  80 (make-if (cons (make-func (list "=" (make-var "M") (make-val 0))) (make-goto 170))))
                             (make-line  90 (make-print (list (make-val "THERE ARE ") (make-var "M") (make-val " MATCHES LEFT"))))
                             (make-line 100 (make-print (list (make-val "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND ")
                                                              (make-func (list "MIN" (make-var "M") (make-val 5)))
                                                              (make-val " MATCHES"))))
                             (make-line 110 (make-print (list (make-val "HOW MANY MATCHES DO YOU TAKE?"))))
                             (make-line 120 (make-input "P"))
                             (make-line 130 (make-if (cons (make-func (list "||" (make-func (list "<" (make-var "P") (make-val 1)))
                                                                      (make-func (list "||" (make-func (list ">" (make-var "P") (make-val 5)))
                                                                                 (make-func (list ">" (make-var "P") (make-var "M")))))))
                                                           (make-goto 110))))
                             (make-line 140 (make-assign (list "M" (make-func (list "-" (make-var "M") (make-var "P"))))))
                             (make-line 150 (make-if (cons (make-func (list "=" (make-var "M") (make-val 0))) (make-goto 190))))
                             (make-line 160 (make-goto 20))
                             (make-line 170 (make-print (list (make-val "PLAYER 1 WINS!"))))
                             (make-line 180 (make-stop))
                             (make-line 190 (make-print (list (make-val "PLAYER 2 WINS!"))))
                             (make-line 200 (make-stop)))
                           nimp))



(define nim-subp (new-program))
(define nim-sube (new-program))
(define nim-sub (make-runtime (list
                                (make-line  10 (make-assign (list "M" (make-val 20))))
                                (make-line  20 (make-assign (list "U" (make-val 1))))
                                (make-line  30 (make-gosub 70))
                                (make-line  40 (make-assign (list "U" (make-val 2))))
                                (make-line  50 (make-gosub 70))
                                (make-line  60 (make-goto 20))
                                (make-line  70 (make-print (list (make-val "PLAYER ")
                                                                 (make-var "U")
                                                                 (make-val ": YOU CAN TAKE BETWEEN 1 AND ")
                                                                 (make-func (list "MIN" (make-var "M") (make-val 5)))
                                                                 (make-val " MATCHES"))))
                                (make-line  80 (make-print (list (make-val "THERE ARE ") (make-var "M") (make-val " MATCHES LEFT"))))
                                (make-line  90 (make-print (list (make-val "HOW MANY MATCHES DO YOU TAKE?"))))
                                (make-line 100 (make-input "P"))
                                (make-line 110 (make-if (cons (make-func (list "||" (make-func (list "<" (make-var "P") (make-val 1)))
                                                                               (make-func (list "||" (make-func (list ">" (make-var "P") (make-val 5)))
                                                                                                (make-func (list ">" (make-var "P") (make-var "M")))))))
                                                              (make-goto 80))))
                                (make-line 120 (make-assign (list "M" (make-func (list "-" (make-var "M") (make-var "P"))))))
                                (make-line 130 (make-if (cons (make-func (list "=" (make-var "M") (make-val 0))) (make-goto 150))))
                                (make-line 140 (make-return))
                                (make-line 150 (make-print (list (make-val "PLAYER ") (make-var "U") (make-val " WINS!")))))
                              nim-subp))
