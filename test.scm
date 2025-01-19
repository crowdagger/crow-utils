(add-to-load-path "..")
(import (crow-utils checked))

(defn (double x)
  (number? -> number?)
  #:doc "Doubles a number"
  (* 2 x))

(double 42)


(define-checked (add [x number?] [y number?])
  (+ x y))
