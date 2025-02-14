(define-library (crow-utils vec)
  (import (scheme base)
          (scheme case-lambda)
          (ice-9 match))
  (export make-vec)
  (begin
    ;;;; Resizeable vector
    (define make-vec
      (case-lambda
        [() (make-vec 32)]
        [(n)
        (let* ([current 0]
               [max n]
               [v (make-vector n)])
          (define (get i)
            "Return the ith element in the vector"
            (unless (and (integer? i)
                         (>= i 0 )
                         (< i current))
              (error "Invalid index" i))
            (vector-ref v i))
          (define (push! x)
            "Push an element at the end of vector"
            ;; If we reach allocated size, double it and copy vector
            (when (= current max)
                (set! max (* 2 max))
                (let ([new-v (make-vector max)])
                  (vector-copy! new-v 0 v)
                  (set! v new-v)))
            (vector-set! v current x)
            (set! current (+ 1 current))
            #t)
          (define (pop!)
            "Return the last element of the vector and remove it"
            (unless (positive? current)
              (error "Can not pop on empty vector"))
            (let ([x (get (- current 1))])
              (set! current (- current 1))
              x))
          (lambda args
            "The dispatch 'method' that redirects to various functions"
            (match args
              [() v]
              [('length) current]
              [('allocated) max]
              [('pop!) (pop!)]
              [('push! x) (push! x)]
              [('get i) (get i)]
              [(i) (get i)]
              [else (error "Invalid arguments" args)])))]))))
