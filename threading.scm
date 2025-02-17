(define-library (crow-utils threading)
  (import (scheme base))
  (export -> ->>)
  (begin
    ;;; Thread first macro ala Clojure
    (define-syntax ->
      (syntax-rules ()
        [(-> exp) exp]
        [(-> exp (f args ...) f* ...)
         (-> (f exp args ...) f* ...)]
        [(-> exp f f* ...)
         (-> (f exp) f* ...)]))

    ;;; Thread-last macro ala Clojure
    (define-syntax ->>
      (syntax-rules ()
        [(->> exp) exp]
        [(->> exp (f args ...) f* ...)
         (->> (f args ... exp) f* ...)]
        [(->> exp f f* ...)
         (->> (f exp) f* ...)]))
    ))
