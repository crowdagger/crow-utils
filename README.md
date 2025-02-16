# crow-utils

Misc scheme utilites that I deemed vaguely useful for Scheme code I write.

I try to use R7RS style but it's mostly tested on Guile. 

## vec

Growable vector.

The interface is a bit not schemey, more object-like. 

```scheme 
;; Create an empty vector with two elements allocated
;; (by default (make-vec) allocates 32)
(define v (make-vec 2))

;; add elements to v
(v 'push! 1)
(v 'push! 2)

;; access element n
(v 'get 0) ; => 1
;; short form
(v 1) ; => 2

;; modify element n
(v 'set! 0 42)

;; If v grows beside allocation, its content will be copied
;; behind the scenes
(v 'push! 3)

(v 'length) ; => 3
(v 'allocated) ; => 4 (allocation doubles each time it is needed)

;; You can also shrink v
(v 'pop!) ; => 3 (last value)
(v 'length) ; => 2

;; List conversion
(v '->list) ; => '(0 1)
```

## checked

```scheme
(import (crow-utils checked))
```

Provides some tools to check argument types and return types of functions.

### defn

```scheme 
(defn (double x)
  (number? -> number?)
  #:doc "Doubles a number"
  (* 2 x))
```

Here, the function will check its argument when it is called, and also checks its return value before sending it.

Therefore, 

```scheme
(double 42)
```

will send an error. and so will 

```scheme
(defn (buggy-double x)
  (number? -> number?)
  #:doc "Doubles a number"
  (display (* 2 x)))
```

Syntax: `(defn (name args ...) (predicates ... [ -> return_predicate ]) [ #:doc doc ] body ...)`

Since some test are automatically added at the beginning of the body,
it messes up with the possibility of adding a docstring at the
beginning of the function ; the optional #:doc parameter allows to fix
that.


### SRFI 253

After I started implementing `defn`, I stumbled upon
[SRFI-253](https://srfi.schemers.org/srfi-253/) by Artyom Bologov
which tackles the same problem.

Therefore, This library also provides the equivalent `define-checked'
:

```scheme
(define-checked (add [x number?] [y number?])
  (+ x y))
```

It is also possible to provide the #:doc keyword to add a docstring.

Here, the return type isn't checked, but in many cases it might indeed
be redundant.

This library also provides `lambda-checked` and `values-checked`, but
not the full scope of this SRFI. 

