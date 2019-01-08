(namespace ("drewc/repl-history#"
            repl-history-number
            repl-history-result-table
            repl-history-form-table
            repl-history-previous-cache-length
            repl-history-previous-cache
            repl-history-add!
            repl-history-number-cache-length
            repl-history-number-cache
            repl-history-clear
            repl-history-nope
            repl-history-find-cached-cons-by-number
            repl-history-result
            repl-history-previous-result
            repl-history-form
            repl-history-previous-form
            repl-history%
            read-percent
            every
            make-history-form
            read-percent-aux))

(define repl-history-number -1)

(define repl-history-result-table
  (make-table weak-values: #t
              test: eqv?))

(define repl-history-form-table
  (make-table weak-values: #t
              test: eqv?))

(define repl-history-previous-cache-length 3)

(define repl-history-previous-cache '())

(define (repl-history-add! form result)
  "=> the number"
  (let ((this-number (+ repl-history-number 1))
        (new-cache (cons (cons form result) repl-history-previous-cache)))
    (when (> (length new-cache) repl-history-previous-cache-length)
      (set! new-cache (take new-cache repl-history-previous-cache-length)))
    (table-set! repl-history-form-table this-number form)
    (table-set! repl-history-result-table this-number result)
    (set! repl-history-previous-cache new-cache)
    (set! repl-history-number  this-number)
  this-number))

(define repl-history-number-cache-length 10)

(define repl-history-number-cache '())

(define (repl-history-clear)
 (set! repl-history-number -1)
 (set! repl-history-previous-cache '())
 (set! repl-history-number-cache '()))

(define repl-history-nope (gensym))

(define (repl-history-find-cached-cons-by-number n)
  (let ((cached (assoc n repl-history-number-cache eqv?)))
    (or cached
        (let* ((result (table-ref repl-history-result-table n repl-history-nope))
               (form (table-ref repl-history-form-table n repl-history-nope)))
          (if (eq? result repl-history-nope)
            #!void
            (let ((new-cache (cons (cons n (cons form result)) repl-history-number-cache)))
              (when (> (length new-cache) repl-history-number-cache-length)
                (set! new-cache (take new-cache repl-history-number-cache-length)))
              (set! repl-history-number-cache new-cache)
              (car new-cache)))))))

(define (repl-history-result n)
  (let ((cached (repl-history-find-cached-cons-by-number n)))
    (if (pair? cached)
      (cddr cached)
      cached)))

(define (repl-history-previous-result n)
  (repl-history-result (- repl-history-number n)))

(define (repl-history-form n)
  (let ((cached (repl-history-find-cached-cons-by-number n)))
    (if (pair? cached)
      (cadr cached)
      cached)))

(define (repl-history-previous-form n)
  (repl-history-form (- repl-history-number n)))

;;; Make it so only one forms need exporting repl-history%

(define (repl-history% type . args)
  (case type
    ((add!) (apply repl-history-add! args))
    ((result) (apply repl-history-result args))
    ((previous-result) (apply repl-history-previous-result args))
    ((form) (apply repl-history-form args))
    ((previous-form) (apply repl-history-previous-form args))))

  ;;; This works only in gambit for now.


;;(##include "~~lib/gambit#.scm")
;;(##include "~~lib/_gambit#.scm")

(##define-macro (macro-peek-next-char-or-eof re) ;; possibly returns EOF
  `(macro-peek-char (macro-readenv-port ,re)))

(##define-macro (macro-read-next-char-or-eof re) ;; possibly returns EOF
  `(macro-read-char (macro-readenv-port ,re)))

(define (read-percent re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip #\#
    (read-percent-aux re start-pos)))

(define (every pred tlist)
  (if (char? pred)
    (let ((c pred))
      (set! pred (lambda (i) (equal? i c)))))
  (if (null? tlist)
    #f
    (let ((t (pred (car tlist))))
      (if t
        (if (null? (cdr tlist))
          #t
          (every pred (cdr tlist)))
        #f))))

(define (make-history-form re type n)
  (macro-readenv-wrap re (list 'drewc/repl-history#repl-history% (list 'quote type) n)))

(define (read-percent-aux re start-pos)
  (let* ((str (##build-delimited-string re #\% 1))
         (length (string-length str))
         (slist (string->list str)))

    (cond
     ;; First "%" repeating
     ((every #\% slist)
      (make-history-form
       re 'previous-result (- length 1)))
     ;; Now, "%+" with "+" repeating
     ((every #\+ (cdr slist))
      (make-history-form
       re 'previous-form (- length 2)))
     ;; Ok, is it now "%n" with n being an integer?
     ((every char-numeric? (cdr slist))
      (make-history-form
       re 'result (##string->number/keyword/symbol re (list->string (cdr slist)) #t)))
     ;;
     ((and (equal? (cadr slist) #\+)
           (every char-numeric? (cddr slist)))
      (make-history-form
       re 'form (##string->number/keyword/symbol re (list->string (cddr slist)) #t)))
     (else
      (macro-readenv-wrap re (##string->number/keyword/symbol re str #t))))))

(##readtable-char-handler-set! (current-readtable) #\%  read-percent)
