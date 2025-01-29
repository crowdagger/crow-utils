(define-library (crow-utils template)
  (import (scheme base)
          (scheme write)
          (srfi srfi-13)
          (srfi srfi-69))
  (export process-template apply-template
          *opening-delimiter* *closing-delimiter*)
  (begin
    (define (find-closing s opening-delimiter closing-delimiter)
      "Search a string for the closing delimiter and returns ???"
      (let ([b (string-contains s closing-delimiter)])
        (if (not b)
            (error "Unmatched template's opening delimiter" s)
            (let ([tag (string-trim-both (substring s 0 b)
                                         #\ )])
              (cons (string->symbol tag)
                    (find-opening
                     (substring
                      s
                      (+ (string-length closing-delimiter)
                         b))
                     opening-delimiter
                     closing-delimiter))))))
    
    (define (find-opening s opening-delimiter closing-delimiter)
      "Search a strïng for opening delimiter and returns ???"
      (let ([b (string-contains s opening-delimiter)])
        (if (not b)
            (list s)
            (cons (substring s 0 b)
                  (find-closing
                   (substring s (+ (string-length opening-delimiter)
                                   b))
                   opening-delimiter
                   closing-delimiter)))))
    
    (define (process-template string . rest)
      "Transform a template to a list of substrings and symbols.

Additional parameters may include strings for opening and closing delimiters"
      (unless (string? string)
        (error "Template must be a string" string))
      (let ([opening-delimiter
             (if (> (length rest) 0)
                 (if (string? (car rest))
                     (car rest)
                     (error "Opening delimiter must be a string" (car rest)))
                 "{{")]
            [closing-delimiter
             (if (> (length rest) 1)
                 (if (string? (cadr rest))
                     (cadr rest)
                     (error "Closing delimiter must be a string"
                            (cadr rest)))
                 "}}")])
        (find-opening string opening-delimiter closing-delimiter)))

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
