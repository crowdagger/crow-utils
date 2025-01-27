(define-library (crow-utils template)
  (import (scheme base)
          (srfi 13))
  (export process-template)
  (begin
    (define (find-closing s)
      "Search a string for }} and returns ???"
      (let ([b (string-contains s "}}")])
        (if (not b)
            (error "Unmatched opening brackets" s)
            (cons (string->symbol (substring s 0 b))
                  (find-opening (substring s (+ 2 b)))))))
    
    (define (find-opening s)
      "Search a strïng for {{ and returns ???"
      (let ([b (string-contains s "{{")])
        (if (not b)
            (list s)
            (cons (substring s 0 b)
                  (find-closing
                   (substring s (+ 2 b)))))))
    
    (define (process-template string)
      "Transform a template to a list of substrings and symbols"
      (unless (string? string)
        (error "Argument must be a string" string))
      (find-opening string))
))
