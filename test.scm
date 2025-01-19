(add-to-load-path "..")
(install-r7rs!)

(import (scheme base)
        (srfi srfi-64)
        (crow-utils checked))

(test-begin "Checked")
(test-group "defn"
  (defn (double x)
    (number? -> number?)
    #:doc "Doubles a number"
    (* 2 x))
  (test-assert (double 42))
  (test-error (double "42"))
  (defn (divide x y)
    (integer? integer? -> integer?)
    (/ x y))
  (test-assert (divide 4 2))
  (test-error (divide 4.2 2.1))
  (test-error (divide 4 3))

  (defn (complicated a b c d e)
    (string? number? list? procedure? boolean? -> integer?)
    42)
  (test-assert (complicated "Foo" 2 '(bar) (lambda (x) x) #f))
  (test-error (complicated "Foo" 2 "bar" #f 1.0))
  )
(test-end "Checked")

(defn (double x)
  (number? -> number?)
  #:doc "Doubles a number"
  (* 2 x))

(double 42)


(define-checked (add [x number?] [y number?])
  (+ x y))

(add 2 3)

