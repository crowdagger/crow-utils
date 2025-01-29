(define-library (crow-utils template)
  (import (scheme base)
          (srfi srfi-13)
          (srfi srfi-69))
  (export process-template apply-template)
  (begin
    (define (find-closing s)
      "Search a string for }} and returns ???"
      (let ([b (string-contains s "}}")])
        (if (not b)
            (error "Unmatched opening brackets" s)
            (cons (string->symbol
                   (string-trim-both (substring s 0 b)
                                     #\ ))
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

    (define (apply-template template context)
      "Apply template with values given by context

Context can be a hashtable or an alist."
      (let ([dict (if (hash-table? context)
                      context
                      (alist->hash-table context))])
        (apply string-append
         (map (lambda (x)
                (cond
                 [(string? x) x]
                 [(symbol? x) (hash-table-ref dict x)]
                 [else (error "Template should be a list of string and symbols" template)]))
              template))))
))
