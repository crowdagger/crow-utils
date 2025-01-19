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


(test-group "define-checked"
  (define-checked (add [x number?] [y number?])
    (+ x y))

  (test-assert (add 2 3))
  (test-error (add "foo" 3))

  (define-checked (add-to-list [l list?] [i integer?])
    (cons i l))
  (test-assert (add-to-list '(3) 2))

  (define-checked (add2 [x integer?] [y integer?])
    #:doc "foo"
    (+ x y))
  (test-assert (add2 3 2))
  (test-error (add2 3 "boo"))

  (define-checked (add-to-list2 [l pair?] v)
    (cons v l))
  (test-assert (add-to-list2 '(foo) 42))
  (test-error (add-to-list2 '() 42))

  (define-checked (complicated [a string?]
                               [b number?]
                               [c list?]
                               [d procedure?]
                               [e boolean?])
    42)
  (test-assert (complicated "Foo" 2 '(bar) (lambda (x) x) #f))
  (test-error (complicated "Foo" 2 "bar" #f 1.0))

  (define-checked (complicated2 [a string?]
                               b
                               [c list?]
                               d
                               [e boolean?])
    42)
  (test-assert (complicated2 "Foo" 2 '(bar) (lambda (x) x) #f))
  (test-error (complicated2 "Foo" 2 "bar" #f 1.0))
  (test-error (complicated2 "Foo" 2 '(bar) 3 1.0))
  )

(test-end "Checked")
