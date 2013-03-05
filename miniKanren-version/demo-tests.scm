;;; Tests from Demo.hs

(test-check "demo-1"
  (run* (q)
    (fresh (gamma c l)
      (== `((i MAXINT secret) (,PUBLIC ,PUBLIC ,SECRET)) gamma)
      (== `(seq
             (while (< i MAXINT)
               (while (< i secret)
                 skip))
             (seq
               (output ,PUBLIC (intexp ,(build-num 0)))
               (assign i (+ i (intexp ,(build-num 1))))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '())

(test-check "demo-2"
  (run* (q)
    (fresh (gamma c l)
      (== `((i secret) (,SECRET ,SECRET)) gamma)
      (== `(seq
             (while (< i secret)
               (assign i (+ i (intexp ,(build-num 1)))))
             (output ,PUBLIC (intexp ,(build-num 0))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '())

(test-check "demo-3"
  (run* (q)
    (fresh (gamma c l)
      (== `((i secret) (,SECRET ,SECRET)) gamma)
      (== `(seq
             (while (< i secret)
               (seq
                 (assign i (+ i (intexp ,(build-num 1))))
                 (output ,PUBLIC (intexp ,(build-num 0)))))
             (output ,PUBLIC (intexp ,(build-num 0))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '())

(test-check "demo-4"
  (run* (q)
    (fresh (gamma c l)
      (== `((i secret) (,SECRET ,SECRET)) gamma)
      (== '(while (< i secret)
             (assign i (+ i (intexp (1)))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '((((i secret) (SECRET SECRET))
     (while (< i secret) (assign i (+ i (intexp (1)))))
     SECRET)))

(test-check "demo-5"
  (run* (q)
    (fresh (gamma c l)
      (== `((i secret) (,PUBLIC ,SECRET)) gamma)
      (== `(seq
             (while (< i secret)
               (assign i (+ i (intexp ,(build-num 1)))))
             (output ,PUBLIC (intexp ,(build-num 0))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,c ,l) q)))
  '())

(test-check "loop-1"
  (run* (q)
    (fresh (gamma c l il secretl)
      (== `((secret i) (,il ,secretl)) gamma)
      (== `(while (< i secret)
             (assign i (+ i (intexp ,(build-num 1)))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,l) q)))
  '((((secret i) (_.0 SECRET)) SECRET)
    (((secret i) (PUBLIC PUBLIC)) PUBLIC)))

(test-check "loop-2"
  (run* (q)
    (fresh (gamma c l xl yl)
      (== `((x y) (,xl ,yl)) gamma)
      (== `(while (< x (intexp ,(build-num 10)))
             (seq
               (assign y (intexp ,(build-num 0)))
               (seq
                 (while (< y (intexp ,(build-num 10)))
                   (assign y (+ y (intexp ,(build-num 1)))))
                 (assign x (+ x (intexp ,(build-num 1)))))))
          c)
      (typeo gamma c l)
      (== `(,gamma ,l) q)))
  '((((x y) (SECRET SECRET))
     SECRET)
    (((x y) (PUBLIC PUBLIC))
     PUBLIC)))
