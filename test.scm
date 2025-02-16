(add-to-load-path "..")

(import (scheme base)
        (srfi srfi-64)
        (crow-utils vec)
        (crow-utils checked))

(test-begin "checked")
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

  (define-checked (stuff [a integer?] [b integer?] c)
    (display a)
    (newline)
    (display b)
    (newline))
  (test-assert (stuff 3 2 'foo))
  (test-error (stuff 'foo 3 2))

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
(test-end "checked")

(test-begin "vec")
(test-group "vec-api"
  (define v (make-vec 2))
  (test-equal 2 (v 'allocated))
  (test-equal 0 (v 'length))
  (test-error (v 'get 0))
  (v 'push! 1)
  (test-equal 2 (v 'allocated))
  (test-equal 1 (v 'length))
  (test-equal 1 (v 'get 0))
  (test-equal 1 (v 0))
  (v 'push! 2)
  (test-equal 2 (v 'allocated))
  (v 'push! 3)
  (test-equal 4 (v 'allocated))
  (test-equal 3 (v 'length))
  (test-equal 3 (v 2))
  (test-equal '(1 2 3) (v '->list))
  (v 'set! 2 42)
  (test-equal 42 (v 2))
  (test-equal 42 (v 'pop!))
  (test-error (v 2))
  (test-equal 2 (v 'length))
  )

(test-end "vec")
