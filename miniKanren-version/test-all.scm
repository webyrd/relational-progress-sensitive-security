(load "pse.scm")
(load "demo-tests.scm")
(load "examples-tests.scm")


(test-check "almost-example-1"
;;; uses !-o instead of typeo
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(seq (while (> h (intexp ())) (assign h (- h low))) (output ,LOW (intexp (1)))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '())

(test-check "almost-example-2"
;;; uses !-o instead of typeo  
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(seq (cast p-tag (while (> h (intexp ())) (assign h (- h low)))) (output ,LOW (intexp (1)))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW))
     LOW
     (seq (cast
           p-tag
           (while (> h (intexp ())) (assign h (- h low))))
          (output LOW (intexp (1))))
     LOW)))

(test-check "example-1"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(seq (while (> h (intexp ())) (assign h (- h low))) (output ,LOW (intexp (1)))) c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '())

(test-check "example-2"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(seq (cast p-tag (while (> h (intexp ())) (assign h (- h low)))) (output ,LOW (intexp (1)))) c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '((((h low) (HIGH LOW))
     (seq (cast
           p-tag
           (while (> h (intexp ())) (assign h (- h low))))
          (output LOW (intexp (1))))
     LOW)))
