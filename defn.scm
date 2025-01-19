(install-r7rs!)

(define-library (crow-utils defn)
  (import (scheme base)
          (srfi srfi-1))
  (export defn ->)
  (begin
    (define (assert pred? val)
      "Throws an exception if val does not satisfy pred"
      (when (not (pred? val))
        (error "Value does not satisfy predicate" val pred?)))

    (define (const x)
      (lambda args x))

    
    ;; Ok this is probably not a good idea?
    (define-syntax -> (syntax-rules ()))

    ;;;; Variant of define, allowing to specify in and output "types" as predicates,
    ;;;; with a syntax that looks a bit like static typing but is more like a contract:
    ;;;; I guess?
    (define-syntax defn
      (syntax-rules (->)
        ((_ (name args ...) (test ... -> ret_pred) #:doc doc body body* ...)
         (define (name args ...)
           doc
           (let ([all-args (list args ...)])
             (map assert
                  (take (list test ...) (length all-args))
                  all-args))
           (let ((return 
                  (begin 
                    body
                    body*
                    ...)))
             (assert ret_pred return)
             return)))
        ((_ (name args ...) (test ... -> ret_pred) body body* ...)
         (deefn (name args ...) (test ... -> ret_pred) #:doc "" body body* ...))
        ((_ (name args ...) (test ...) #:doc doc body body* ...)
         (defn (name args ...) (test ... -> (const #t)) #:doc doc body body* ...))
        ((_ (name args ...) (test ...) body body* ...)
         (defn (name args ...) (test ... -> (const #t)) #:doc "" body body* ...))
         ((_ args ...)
          (syntax-error "Wrong arguments to defn")))
      )
    ))

