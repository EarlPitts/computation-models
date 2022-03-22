(define (finite-automaton rules input)
  (let ((tape (string->list input))
        (state "q0"))

    (define (step!)
      (set-state! (get-new-state))
      (move!)
      (display-internals))

    (define (set-state! new-state)
      (set! state new-state))
    (define (current-symbol)
      (if (eq? tape '())
        "blank"
        (car tape)))
    (define (finished?)
      (eq? tape '()))
    (define (move!)
      (set! tape (cdr tape)))
    (define (get-new-state)
        (let ((new-state (assoc-ref rules (cons state (current-symbol)))))
          (if new-state
            new-state
            "qr")))
    (define (result)
      (if (string=? state "qa")
        (display "Input accepted\n")
        (display "Input rejected\n")))

    (define (show)
      state)
    (define (display-internals)
      (display "Current state: ")
      (display state)
      (display "\n")
      (display "Current symbol: ")
      (display (current-symbol))
      (display "\n"))

    (define (run)
      (cond ((finished?) (result))
            (else (begin
                    (step!)
                    (run)))))

    (define (dispatch op)
      (cond ((eq? op 'step) (step!))
            ((eq? op 'show) (show))
            ((eq? op 'run) (run))))
    dispatch))

(define rules '((("q0" . #\a) . "q0")
                (("q0" . #\b) . "qa")
                (("qa" . #\b) . "qa")))

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

(run fa)
