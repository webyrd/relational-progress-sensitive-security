;;; Tests from Examples.hs

(test-check "almost-example-1"
;;; uses !-o instead of typeo
  (run* (q)
    (fresh (ctx pc cmd l)
      (== `((h low) (,SECRET ,PUBLIC)) ctx)
      (== `(seq (while (< (intexp ()) h) (assign h (- h low))) (output ,PUBLIC (intexp (1)))) cmd)
      (!-o ctx pc cmd l)
      (== `(,ctx ,pc ,cmd ,l) q)))
  '())

(test-check "almost-example-2"
;;; uses !-o instead of typeo  
  (run* (q)
    (fresh (ctx pc cmd l)
      (== `((h low) (,SECRET ,PUBLIC)) ctx)
      (== `(seq (cast p-tag (while (< (intexp ()) h) (assign h (- h low)))) (output ,PUBLIC (intexp (1)))) cmd)
      (!-o ctx pc cmd l)
      (== `(,ctx ,pc ,cmd ,l) q)))
  '((((h low) (SECRET PUBLIC))
     PUBLIC
     (seq (cast
           p-tag
           (while (< (intexp ()) h) (assign h (- h low))))
          (output PUBLIC (intexp (1))))
     PUBLIC)))

(test-check "example-1"
  (run* (q)
    (fresh (ctx pc cmd l)
      (== `((h low) (,SECRET ,PUBLIC)) ctx)
      (== `(seq (while (< (intexp ()) h) (assign h (- h low))) (output ,PUBLIC (intexp (1)))) cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "example-2"
  (run* (q)
    (fresh (ctx pc cmd l)
      (== `((h low) (,SECRET ,PUBLIC)) ctx)
      (== `(seq (cast p-tag (while (< (intexp ()) h) (assign h (- h low)))) (output ,PUBLIC (intexp (1)))) cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '((((h low) (SECRET PUBLIC))
     (seq (cast
           p-tag
           (while (< (intexp ()) h) (assign h (- h low))))
          (output PUBLIC (intexp (1))))
     PUBLIC)))
