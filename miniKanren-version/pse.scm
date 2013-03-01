(load "mk.scm")
(load "numbers.scm")
(load "lookupo.scm")
(load "test-check.scm")

;;; Relational implementation of type system and semantics from
;;; "Precise Enforcement of Progress-Sensitive Security" by Moore,
;;; Askarov, and Chong, CCS'12.
;;; (http://people.seas.harvard.edu/~aslan/ccs12.pdf)

;;; TODO:
;;; * implement parser/unparser
;;; * double-check output to make sure tests are correct
;;; * add more test programs
;;; * implement semantics
;;; * implement multi-level lattices

(define LOW 'LOW)
(define HIGH 'HIGH)

(define lattice-joino
  (lambda (l1 l2 l3)
    (conde
      [(== HIGH l1) (== HIGH l3)]
      [(== LOW l1) (== HIGH l2) (== HIGH l3)]
      [(== LOW l1) (== LOW l2) (== LOW l3)])))

(define lattice-leqo
  (lambda (l1 l2)
    (conde
      [(== HIGH l2)]
      [(== LOW l2) (== LOW l1)])))

(define bin-opo
  (lambda (op)
    (conde
      [(== '- op)]
      [(== '> op)])))

;;; type judgements for expressions (not explicitly given in the paper)
(define !-eo
  (lambda (gamma e term-level)
    (conde
      [(fresh (n)
         (== `(intexp ,n) e)
         (== LOW term-level))]
      [(symbolo e)
       (lookupo e gamma term-level)]
      [(fresh (bin-op e1 e2 tl1 tl2)
         (== `(,bin-op ,e1 ,e2) e)
         (bin-opo bin-op)
         (lattice-joino tl1 tl2 term-level)
         (!-eo gamma e1 tl1)
         (!-eo gamma e2 tl2))])))

;;; type judgements for commands
(define !-o
  (lambda (gamma pc comm term-level)
    (conde
      [(== 'skip comm) (== LOW term-level)]
      [(fresh (p c l)
         (== `(cast ,p ,c) comm)
         (== LOW term-level)
         (== LOW pc)
         (!-o gamma HIGH c l))]
      [(fresh (l e l^)
         (== `(output ,l ,e) comm)
         (== LOW term-level)
         (lattice-joino pc l^ l)
         (!-eo gamma e l^))]
      [(fresh (x e l^ xl pcl^)
         (== `(assign ,x ,e) comm)
         (== LOW term-level)
         (lattice-joino pc l^ pcl^)
         (lattice-leqo pcl^ xl)
         (lookupo x gamma xl)
         (!-eo gamma e l^))]
      [(fresh (c1 c2 l1 l2 pcl1)
         (== `(seq ,c1 ,c2) comm)
         (lattice-joino pc l1 pcl1)
         (lattice-joino l1 l2 term-level)
         (!-o gamma pc c1 l1)
         (!-o gamma pcl1 c2 l2))]
      [(fresh (e c l l^ pcl pcll^)
         (== `(while ,e ,c) comm)
         (== l^ term-level)
         (lattice-joino pc l pcl)
         (lattice-joino pcl l^ pcll^)
         (!-eo gamma e l)
         (!-o gamma pcll^ c l^))]
      [(fresh (e c1 c2 l pcl l1 l2)
         (== `(if ,e ,c1 ,c2) comm)
         (lattice-joino pc l pcl)
         (lattice-joino l1 l2 term-level)
         (!-eo gamma e l)
         (!-o gamma pcl c1 l1)
         (!-o gamma pcl c2 l2))])))

(test-check "lookupo-1"
  (run 5 (q)
    (fresh (x gamma l)
      (lookupo x gamma l)
      (== `(,x ,gamma ,l) q)))
  '((_.0 ((_.0 . _.1) (_.2 . _.3)) _.2)
    ((_.0 ((_.1 _.0 . _.2) (_.3 _.4 . _.5)) _.4) (=/= ((_.0 _.1))))
    ((_.0 ((_.1 _.2 _.0 . _.3) (_.4 _.5 _.6 . _.7)) _.6) (=/= ((_.0 _.1)) ((_.0 _.2))))
    ((_.0 ((_.1 _.2 _.3 _.0 . _.4) (_.5 _.6 _.7 _.8 . _.9)) _.8) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3))))
    ((_.0 ((_.1 _.2 _.3 _.4 _.0 . _.5) (_.6 _.7 _.8 _.9 _.10 . _.11)) _.10) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4))))))

(test-check "!-eo-1"
  (run 10 (q)
    (fresh (gamma e l)
      (!-eo gamma e l)
      (== `(,gamma ,e ,l) q)))
  '((_.0 (intexp _.1) LOW)
    ((((_.0 . _.1) (_.2 . _.3)) _.0 _.2) (sym _.0))
    ((((_.0 _.1 . _.2) (_.3 _.4 . _.5)) _.1 _.4) (=/= ((_.1 _.0))) (sym _.1))
    ((((_.0 _.1 _.2 . _.3) (_.4 _.5 _.6 . _.7)) _.2 _.6) (=/= ((_.2 _.0)) ((_.2 _.1))) (sym _.2))
    ((((_.0 _.1 _.2 _.3 . _.4) (_.5 _.6 _.7 _.8 . _.9)) _.3 _.8) (=/= ((_.3 _.0)) ((_.3 _.1)) ((_.3 _.2))) (sym _.3))
    ((((_.0 _.1 _.2 _.3 _.4 . _.5) (_.6 _.7 _.8 _.9 _.10 . _.11)) _.4 _.10) (=/= ((_.4 _.0)) ((_.4 _.1)) ((_.4 _.2)) ((_.4 _.3))) (sym _.4))
    ((((_.0 _.1 _.2 _.3 _.4 _.5 . _.6) (_.7 _.8 _.9 _.10 _.11 _.12 . _.13)) _.5 _.12) (=/= ((_.5 _.0)) ((_.5 _.1)) ((_.5 _.2)) ((_.5 _.3)) ((_.5 _.4))) (sym _.5))
    ((((_.0 _.1 _.2 _.3 _.4 _.5 _.6 . _.7) (_.8 _.9 _.10 _.11 _.12 _.13 _.14 . _.15)) _.6 _.14) (=/= ((_.6 _.0)) ((_.6 _.1)) ((_.6 _.2)) ((_.6 _.3)) ((_.6 _.4)) ((_.6 _.5))) (sym _.6))
    ((((_.0 . _.1) (HIGH . _.2)) (- _.0 (intexp _.3)) HIGH) (sym _.0))
    ((((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 . _.8) (_.9 _.10 _.11 _.12 _.13 _.14 _.15 _.16 . _.17)) _.7 _.16) (=/= ((_.7 _.0)) ((_.7 _.1)) ((_.7 _.2)) ((_.7 _.3)) ((_.7 _.4)) ((_.7 _.5)) ((_.7 _.6))) (sym _.7))))

(test-check "simple-!-eo-1"
  (run 1 (q)
    (fresh (gamma e l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== '(intexp ()) e)
      (!-eo gamma e l)
      (== `(,gamma ,e ,l) q)))
  '((((h low) (HIGH LOW)) (intexp ()) LOW)))

(test-check "simple-!-eo-2"
  (run 1 (q)
    (fresh (gamma e l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== '(> h (intexp ())) e)
      (!-eo gamma e l)
      (== `(,gamma ,e ,l) q)))
  '((((h low) (HIGH LOW)) (> h (intexp ())) HIGH)))

(test-check "simple-!-eo-3"
  (run 1 (q)
    (fresh (gamma e l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== '(- h low) e)
      (!-eo gamma e l)
      (== `(,gamma ,e ,l) q)))
  '((((h low) (HIGH LOW)) (- h low) HIGH)))

(test-check "!-o-0"
  (run 5 (q)
    (fresh (gamma pc c l x e)
      (== `(assign ,x ,e) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((_.0 . _.1) (HIGH . _.2))
     HIGH
     (assign _.0 (intexp _.3))
     LOW)
    ((((_.0 . _.1) (HIGH . _.2)) HIGH (assign _.0 _.0) LOW)
     (sym _.0))
    ((((_.0 _.1 . _.2) (_.3 HIGH . _.4))
      HIGH
      (assign _.1 (intexp _.5))
      LOW)
     (=/= ((_.1 _.0))))
    ((((_.0 _.1 . _.2) (HIGH _.3 . _.4))
      HIGH
      (assign _.0 _.1)
      LOW)
     (=/= ((_.1 _.0)))
     (sym _.1))
    ((((_.0 _.1 _.2 . _.3) (HIGH _.4 _.5 . _.6))
      HIGH
      (assign _.0 _.2)
      LOW)
     (=/= ((_.2 _.0)) ((_.2 _.1)))
     (sym _.2))))

(test-check "!-o-1"
  (run 10 (q)
    (fresh (gamma pc c l)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((_.0 _.1 skip LOW)
    (_.0 LOW (cast _.1 skip) LOW)
    (_.0 LOW (cast _.1 (output HIGH (intexp _.2))) LOW)
    (_.0 HIGH (output HIGH (intexp _.1)) LOW)
    ((((_.0 . _.1) (_.2 . _.3))
      LOW
      (cast _.4 (output HIGH _.0))
      LOW)
     (sym _.0))
    ((((_.0 . _.1) (_.2 . _.3)) HIGH (output HIGH _.0) LOW)
     (sym _.0))
    ((((_.0 _.1 . _.2) (_.3 _.4 . _.5))
      LOW
      (cast _.6 (output HIGH _.1))
      LOW)
     (=/= ((_.1 _.0)))
     (sym _.1))
    (_.0 LOW (output LOW (intexp _.1)) LOW)
    ((((_.0 _.1 _.2 . _.3) (_.4 _.5 _.6 . _.7))
      LOW
      (cast _.8 (output HIGH _.2))
      LOW)
     (=/= ((_.2 _.0)) ((_.2 _.1)))
     (sym _.2))
    (((_.0 . _.1) (HIGH . _.2))
     LOW
     (cast _.3 (assign _.0 (intexp _.4)))
     LOW)))

(test-check "simple-!-o-1"
  (run 1 (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(output ,LOW (intexp (1))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW)) LOW (output LOW (intexp (1))) LOW)))

(test-check "simple-!-o-2"
  (run 1 (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(while (> h (intexp ())) (assign h (- h low))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW)) HIGH (while (> h (intexp ())) (assign h (- h low))) LOW)))

(test-check "example-1"
  (run 1 (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(seq (while (> h (intexp ())) (assign h (- h low))) (output ,LOW (intexp (1)))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '())

(test-check "example-2"
  (run 1 (q)
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
