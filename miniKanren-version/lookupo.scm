(load "mk.scm")
(load "test-check.scm")

;;; Use two lists of equal length to represent the environment.

(define lookupo
  (lambda (x env t)
    (fresh (y y* v v*)
      (== `((,y . ,y*) (,v . ,v*)) env)
      (conde
        ((== x y) (== v t))
        ((=/= x y) (lookupo x `(,y* ,v*) t))))))
