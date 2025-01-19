(install-r7rs!)


(define-library (crow-utils checked)
  (import (scheme base)
          (srfi srfi-1)
          (srfi srfi-28))
  (export defn ->
          lambda-checked values-checked define-checked)
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
    (define-syntax : (syntax-rules ()))

    ;;; Largely inspired by SRFI-253 example implementation
    ;;; seeee https://srfi.schemers.org/srfi-253/srfi/impl.scm
    (define-syntax %lambda-checked
     (syntax-rules ()
       ((_ (doc body ...) args (checks ...) ())
        (lambda args
          doc
          checks ...
          body ...))
       ((_ (doc body) (args ...) (checks ...) ((arg pred) . rest))
        (%lambda-checked
         (doc body)
         (args ... arg) (checks ... (assert pred arg)) rest))
       ((_ (doc body) (args ...) (checks ...) (arg . rest))
        (%lambda-checked
         (doc body)
         (args ... arg) (checks ...) rest))
       ((_ (doc body) (args ...) (checks ...) last)
        (%lambda-checked
         (doc body)
         (args ... . last) (checks ...) ()))))

    ;; Idem here
    (define-syntax lambda-checked
    (syntax-rules ()
      ((_ () body ...)
       (lambda () body ...))
      ((_ (arg . args) body ...)
       (%lambda-checked (body ...) () () (arg . args)))
      ;; Case of arg->list lambda, no-op.
      ((_ arg body ...)
       (lambda arg body ...))))

    ;;; And here
    (define-syntax values-checked
     (syntax-rules ()
       ((_ (predicate) value)
        (let ((v value))
          (assert predicate v)
          v))
       ((_ (predicate ...) value ...)
        (values (values-checked (predicate) value) ...))))

    ;; And again here
    (define-syntax define-checked
     (syntax-rules ()
       ;; Procedure
       ((_ (name args ...) #:doc doc body ...)
        (define name (%lambda-checked ( doc body ...) () () (args ...))))
       ((_ (name args ...) body ...)
        (define-checked (name args ...) #:doc "" body ...))
       ;; Variable
       ((_ name pred value)
        (define name (values-checked (pred) value)))))

    

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

