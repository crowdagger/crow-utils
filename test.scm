(add-to-load-path "..")
(use-modules (crow-utils defn))

(defn (f x) (number? -> string?) (* x 2))

(f 2)
