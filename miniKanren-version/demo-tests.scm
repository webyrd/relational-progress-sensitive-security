;;; Tests from Demo.hs

(test-check "demo-1"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i MAXINT secret) (,PUBLIC ,PUBLIC ,SECRET)) ctx)
      (== `(seq
             (while (< i MAXINT)
               (while (< i secret)
                 skip))
             (seq
               (output ,PUBLIC (intexp ,(build-num 0)))
               (assign i (+ i (intexp ,(build-num 1))))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "demo-2"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,SECRET ,SECRET)) ctx)
      (== `(seq
             (while (< i secret)
               (assign i (+ i (intexp ,(build-num 1)))))
             (output ,PUBLIC (intexp ,(build-num 0))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "demo-3"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,SECRET ,SECRET)) ctx)
      (== `(seq
             (while (< i secret)
               (seq
                 (assign i (+ i (intexp ,(build-num 1))))
                 (output ,PUBLIC (intexp ,(build-num 0)))))
             (output ,PUBLIC (intexp ,(build-num 0))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "demo-4"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,SECRET ,SECRET)) ctx)
      (== '(while (< i secret)
             (assign i (+ i (intexp (1)))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '((((i secret) (SECRET SECRET))
     (while (< i secret) (assign i (+ i (intexp (1)))))
     SECRET)))

(test-check "demo-5"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,PUBLIC ,SECRET)) ctx)
      (== `(seq
             (while (< i secret)
               (assign i (+ i (intexp ,(build-num 1)))))
             (output ,PUBLIC (intexp ,(build-num 0))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "loop-1"
  (run* (q)
    (fresh (ctx cmd l il secretl)
      (== `((secret i) (,il ,secretl)) ctx)
      (== `(while (< i secret)
             (assign i (+ i (intexp ,(build-num 1)))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,l) q)))
  '((((secret i) (_.0 SECRET)) SECRET)
    (((secret i) (PUBLIC PUBLIC)) PUBLIC)))

(test-check "loop-2"
  (run* (q)
    (fresh (ctx cmd l xl yl)
      (== `((x y) (,xl ,yl)) ctx)
      (== `(while (< x (intexp ,(build-num 10)))
             (seq
               (assign y (intexp ,(build-num 0)))
               (seq
                 (while (< y (intexp ,(build-num 10)))
                   (assign y (+ y (intexp ,(build-num 1)))))
                 (assign x (+ x (intexp ,(build-num 1)))))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,l) q)))
  '((((x y) (SECRET SECRET))
     SECRET)
    (((x y) (PUBLIC PUBLIC))
     PUBLIC)))
