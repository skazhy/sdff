(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

;; 2.1: Arity repair

(define (compose f g)
  (assert (= (get-arity f) 1))
  (define (the-composition . args)
    (assert (= (get-arity g) (length args)))
    (f (apply g args)))
  (restrict-arity the-composition (get-arity g)))

(define (parallel-combine h f g)
  (assert (= (get-arity f) (get-arity g)))
  (define (the-combination . args)
    (assert (= (get-arity g) (length args)))
    (h (apply f args) (apply g args)))
  (restrict-arity the-combination (get-arity f)))
