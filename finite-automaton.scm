(define (make-fa state input rules)
  (list state input rules))
(define (state fa)
  (car fa))
(define (input fa)
  (cadr fa))
(define (rules fa)
  (caddr fa))

(define (step state input rules)
  (cond ((eq? input '()) state )
        ((assoc (cons state (car input)) rules)
         (assoc-ref rules (cons state (car input ))))
        (else 'qr)))

(define r '(((q0 . a) . q0)
            ((q0 . b) . qa)
            ((qa . b) . qa)))

(define fa (make-fa 'q0 '(a a a b b b) r))

(define (run fa)
  (cond ((eq? (input fa) '()) (state fa))
        (else (run (make-fa
                     (step (state fa) (input fa) (rules fa))
                     (cdr (input fa))
                     (rules fa))))))
