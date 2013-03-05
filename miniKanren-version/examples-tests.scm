;;; Tests from Examples.hs

(define paper-ctx `((h        h^      hstep   l       l^)
                    (,SECRET ,SECRET ,SECRET ,PUBLIC ,PUBLIC)))

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
