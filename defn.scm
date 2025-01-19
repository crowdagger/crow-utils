(install-r7rs!)

(define-library (crow-utils defn)
  (import (scheme base)
          (srfi srfi-1)
          (srfi srfi-28))
  (export defn ->)
  (begin
    
    (define-syntax assert
      (syntax-rules ()
        ((_ pred val)
         (unless (pred val)
           (error (format "~a (value: ~a) does not satisfy ~a"
                   (quote val)
                   val
                   (quote pred)))))
        ((_ pred val rest ...)
         (begin
           (assert2 pred val)
           (assert2 rest ...)))
        ((_ rest)
         (syntax-error "assert takes an even number of arguments"))
        ))

    (define-syntax assert-list
      (syntax-rules ()
        ((_ () ())
         '())
        ((_ (p p* ...) (v v* ...))
         (begin
           (assert p v)
           (assert-list (p* ...) (v* ...))))))
         
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
           (assert-list (list test ...)
                        (list args ...))
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

