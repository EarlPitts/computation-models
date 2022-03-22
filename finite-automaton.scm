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

(define fa (finite-automaton rules "aaabb"))
