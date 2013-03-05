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
