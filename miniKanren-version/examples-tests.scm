;;; Tests from Examples.hs

(test-check "almost-example-1"
;;; uses !-o instead of typeo
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,SECRET ,PUBLIC)) gamma)
      (== `(seq (while (< (intexp ()) h) (assign h (- h low))) (output ,PUBLIC (intexp (1)))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '())

(test-check "almost-example-2"
;;; uses !-o instead of typeo  
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,SECRET ,PUBLIC)) gamma)
      (== `(seq (cast p-tag (while (< (intexp ()) h) (assign h (- h low)))) (output ,PUBLIC (intexp (1)))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (SECRET PUBLIC))
     PUBLIC
     (seq (cast
           p-tag
           (while (< (intexp ()) h) (assign h (- h low))))
          (output PUBLIC (intexp (1))))
     PUBLIC)))

(test-check "example-1"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,SECRET ,PUBLIC)) gamma)
      (== `(seq (while (< (intexp ()) h) (assign h (- h low))) (output ,PUBLIC (intexp (1)))) c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '())

(test-check "example-2"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,SECRET ,PUBLIC)) gamma)
      (== `(seq (cast p-tag (while (< (intexp ()) h) (assign h (- h low)))) (output ,PUBLIC (intexp (1)))) c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '((((h low) (SECRET PUBLIC))
     (seq (cast
           p-tag
           (while (< (intexp ()) h) (assign h (- h low))))
          (output PUBLIC (intexp (1))))
     PUBLIC)))
