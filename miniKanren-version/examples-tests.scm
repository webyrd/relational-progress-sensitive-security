;;; Tests from Examples.hs

(define paper-ctx `((h        h^      hstep   l       l^)
                    (,SECRET ,SECRET ,SECRET ,PUBLIC ,PUBLIC)))

(test-check "tcmdOne"
  (run* (q)
    (fresh (ctx cmd)
      (== `((h l) (,SECRET ,PUBLIC)) ctx)
      (== (parse-cmd '(assign h 1))
          cmd)
      (typeo ctx cmd q)))
  '(PUBLIC))

(test-check "tcmdTwo"
  (run* (q)
    (fresh (ctx cmd)
      (== `((x y) (,SECRET ,PUBLIC)) ctx)
      (== (parse-cmd
            '(cast p-tag
                   (seq
                     (assign x 1)
                     (seq
                       (assign y 1)
                       (seq
                         (assign y 10)
                         (cast q-tag
                               (while (<= x y)
                                 (seq
                                   (inc x)
                                   (seq
                                     (inc x)
                                     (assign y 10))))))))))
          cmd)
      (typeo ctx cmd q)))
  '())

(test-check "tcmdEleven"
  (run* (q)
    (fresh (ctx cmd)
      (== `((x y) (,SECRET ,PUBLIC)) ctx)
      (== (parse-cmd
            '(cast p-tag
                   (seq
                     (assign x 1)
                     (seq
                       (assign y 10)
                       (cast q-tag
                             (while (< x 100)
                               (inc x)))))))
          cmd)
      (typeo ctx cmd q)))
  '())

(test-check "tcmdTwelve"
  (run* (q)
    (fresh (ctx cmd)
      (== `((x y z) (,SECRET ,SECRET ,SECRET)) ctx)
      (== (parse-cmd
            '(cast p-tag
                   (while (< x 10)
                     (seq
                       (assign y 0)
                       (seq
                         (while (< y 10)
                           (seq
                             (dec x)
                             (inc y)))
                         (inc x))))))
          cmd)
      (typeo ctx cmd q)))
  '(PUBLIC))

#;(test-check ""
  (run* (q)
    (fresh (ctx cmd)
      (== ctx)
      (== (parse-cmd
            '())
          cmd)
      (typeo ctx cmd q)))
  '???)

(test-check "example-1"
  (run* (q)
    (fresh (cmd)
      (== (parse-cmd
            '(while (< l 10)
               (seq
                 (cast p-tag
                       (while (< l h)
                         skip))
                 (inc l))))
          cmd)
      (typeo paper-ctx cmd q)))
  '(PUBLIC))

(test-check "example-2"
  (run* (q)
    (fresh (cmd)
      (== (parse-cmd
            '(seq
               (output 0)
               (seq
                 (cast p-tag
                       (while (< 0 h)
                         (assign h (- h l))))
                 (output 1))))
          cmd)
      (typeo paper-ctx cmd q)))
  '(PUBLIC))

(test-check "example-3"
  (run* (q)
    (fresh (cmd)
      (== (parse-cmd
            '(seq
               (output 0)
               (seq
                 (cast p-tag
                       (while (< 0 h)
                         (assign h (+ h l))))
                 (output 1))))
          cmd)
      (typeo paper-ctx cmd q)))
  '(PUBLIC))

(test-check "example-4"
  (run* (q)
    (fresh (cmd)
      (== (parse-cmd
            '(seq
               (cast p-tag
                       (if (< 0 h)
                           (while (= 1 1)
                             skip)
                           skip))
               (output 1)))
          cmd)
      (typeo paper-ctx cmd q)))
  '(PUBLIC))

(test-check "example-5"
  (run* (q)
    (fresh (cmd)
      (== (parse-cmd
            '(while (< 0 l)
               (seq
                 (assign h^ h)
                 (seq
                   (cast p-tag
                         (while (< 0 h^)
                           (assign h^ (- h^ hstep))))
                   (seq
                     (output l)
                     (dec l))))))
          cmd)
      (typeo paper-ctx cmd q)))
  '(PUBLIC))
