;;; Tests from Demo.hs

(test-check "demo-1"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i MAXINT secret) (,PUBLIC ,PUBLIC ,SECRET)) ctx)
      (== (parse-cmd
            '(seq
               (while (< i MAXINT)
                 (while (< i secret)
                   skip))
               (seq
                 (output 0)
                 (inc i))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "demo-2"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,SECRET ,SECRET)) ctx)
      (== (parse-cmd
            '(seq
               (while (< i secret)
                 (inc i))
               (output 0)))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "demo-3"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,SECRET ,SECRET)) ctx)
      (== (parse-cmd
            '(seq
               (while (< i secret)
                 (seq
                   (inc i)
                   (output 0)))
               (output 0)))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "demo-4"
  (run* (q)
    (fresh (ctx cmd l)
      (== `((i secret) (,SECRET ,SECRET)) ctx)
      (== (parse-cmd
            '(while (< i secret)
               (inc i)))
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
      (== (parse-cmd
            '(seq
               (while (< i secret)
                 (inc i))
               (output 0)))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,cmd ,l) q)))
  '())

(test-check "loop-1"
  (run* (q)
    (fresh (ctx cmd l il secretl)
      (== `((secret i) (,secretl ,il)) ctx)
      (== (parse-cmd
            '(while (< i secret)
               (inc i)))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,l) q)))
  '((((secret i) (_.0 SECRET)) SECRET)
    (((secret i) (PUBLIC PUBLIC)) PUBLIC)))

(test-check "loop-2"
  (run* (q)
    (fresh (ctx cmd l xl yl)
      (== `((x y) (,xl ,yl)) ctx)
      (== (parse-cmd
            '(while (< x 10)
               (seq
                 (assign y 0)
                 (seq
                   (while (< y 10)
                     (inc y))
                   (inc x)))))
          cmd)
      (typeo ctx cmd l)
      (== `(,ctx ,l) q)))
  '((((x y) (SECRET SECRET))
     SECRET)
    (((x y) (PUBLIC PUBLIC))
     PUBLIC)))
