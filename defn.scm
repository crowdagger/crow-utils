(define-module (crow-utils defn)
  #:export (defn
             ->))

;;;; Variant of define, allowing to specify in and output "types" as predicates,
;;;; with a syntax that looks a bit like static typing but is more like a contract:
;;;; I guess?

(define (assert pred? val)
  "Throws an exception if val does not satisfy pred"
  (when (not (pred? val))
    (raise-exception (format #f "Value '~a' does not satisfy ~a"
                             val
                             pred?))))

;; Ok this is probably not a good idea?
(define-syntax -> (syntax-rules ()))

(define-syntax defn
  (syntax-rules (->)
    ((_ (name args ...) (test ... -> ret_pred) body body* ...)
     (define (name args ...)
       (let ([all-args (list args ...)])
         (map assert
              (list-head (list test ...) (length all-args))
              all-args))
       (let ((return 
              (begin 
                body
                body*
                ...)))
         (assert ret_pred return))
         return))))

