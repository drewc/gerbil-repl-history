(define (TOP>)
  (display "TOP> ")
  (let* ((form (read))
         (result (eval form))
         (num (repl-history% 'add! form result)))
    (display (string-append "%" (number->string num) " => "))
    result))

(define (test-repl-history>)
  (let ((t (TOP>)))
    (if (eq? t 'exit)
      t
      (begin (display t) (display "\n") (test-repl-history>)))))
