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

(define TERMINATE 'TERMINATE)
(define DIVERGE 'DIVERGE)
(define UNKNOWN 'UNKNOWN)

(define ext-envo
  (lambda (x v m m^)
    (fresh (x* v*)
      (== `(,x* ,v*) m)
      (== `((,x . ,x*) (,v . ,v*)) m^))))

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
         (== pcll^ term-level) ; the 'while' rule in Figure 3 of the
                               ; published paper contains a typo: the
                               ; term level should be pcll^, not l^.
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

(define typeo
  (lambda (gamma comm term-level)
    (!-o gamma LOW comm term-level)))

(define eval-expo
  (lambda (e m v)
    (conde
      [(fresh (n)
         (== `(intexp ,n) e)
         (== `(intval ,n) v))]
      [(symbolo e)
       (lookupo e m v)]
      [(fresh (bin-op e1 e2 n1 n2 n3)
         (== `(,bin-op ,e1 ,e2) e)
         (bin-opo bin-op)
         (eval-expo e1 m `(intval ,n1))
         (eval-expo e2 m `(intval ,n2))
         (conde
           [(== '- bin-op)
            (minuso n1 n2 n3)
            (== `(intval ,n3) v)]
           [(== '> bin-op)
            (conde
              [(<o n2 n1)
               (== '(intval (1)) v)]
              [(<=o n1 n2)
               (== '(intval ()) v)])]))])))

(define Oo
;;; Dummy termination oracle relation.  We skip the problem of
;;; actually implementing an oracle.
;;;
;;; Technically, Oo is implicitly supposed to be passed the original
;;; program c0 and the initial memory m0 as well.
  (lambda (p m o v)
    (conde
      [(== TERMINATE v)]
      [(== DIVERGE v)]
      [(== UNKNOWN v)])))

(define ->o
  (lambda (in-state out-state)
    (fresh (m o)
      (conde
        [(== `(skip ,m ,o) in-state)
         (== `(stop ,m ,o) out-state)]
        [(fresh (x e v m^)
           (== `((assign ,x ,e) ,m ,o) in-state)
           (== `(stop ,m^ ,o) out-state)
           (ext-envo x v m m^)
           (eval-expo e m v))]
        [(fresh (c1 c2 c1^ m^ o^)
           (== `((seq ,c1 ,c2) ,m ,o) in-state)
           (->o `(,c1 ,m ,o) `(,c1^ ,m^ ,o^))
           (conde
             [(== 'stop c1^) (== `(,c2 ,m^ ,o^) out-state)]
             [(=/= 'stop c1^) (== `((seq ,c1^ ,c2) ,m^ ,o^) out-state)]))]
        [(fresh (e m v c1 c2 ci n n*)
           (== `((if ,e ,c1 ,c2) ,m ,o) in-state)
           (conde
             [(== `(intval (,n . ,n*)) v) (== ci c1)]
             [(== '(intval ()) v) (== ci c2)])
           (eval-expo e m v)
           (->o `(,ci ,m ,o) out-state))]
        [(fresh (e c m o v n n*)
           (== `((while ,e ,c) ,m ,o) in-state)
           (conde
             [(== `(intval (,n . ,n*)) v) (== `((seq c (while ,e ,c)) ,m ,o) out-state)]
             [(== '(intval ()) v) (== `(stop ,m ,o) out-state)])
           (eval-expo e m v))]
        [(fresh (l e m o v o^)
           (== `((output ,l ,e) ,m ,o) in-state)
           (== `(stop ,m ,o^) out-state)
           (ext-envo v l o o^)
           (eval-expo e m v))]
        [(fresh (p c m o v)
           (== `((cast ,p ,c) ,m ,o) in-state)
           (== `(,c ,m ,o) out-state)
           (conde
             [(== TERMINATE v)]
             [(== DIVERGE v)])
           (Oo p m o v))]))))

(define ->*o
  (lambda (in-state out-state)
    (fresh (state)
      (conda
        [(->o in-state state)
         (->*o state out-state)]
        [else (== in-state out-state)]))))

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
  (run* (q)
    (fresh (gamma e l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== '(intexp ()) e)
      (!-eo gamma e l)
      (== `(,gamma ,e ,l) q)))
  '((((h low) (HIGH LOW)) (intexp ()) LOW)))

(test-check "simple-!-eo-2"
  (run* (q)
    (fresh (gamma e l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== '(> h (intexp ())) e)
      (!-eo gamma e l)
      (== `(,gamma ,e ,l) q)))
  '((((h low) (HIGH LOW)) (> h (intexp ())) HIGH)))

(test-check "simple-!-eo-3"
  (run* (q)
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
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(output ,LOW (intexp (1))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW)) LOW (output LOW (intexp (1))) LOW)))

(test-check "simple-!-o-4"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(assign low h) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '())

(test-check "simple-!-o-3"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(assign h (- h low)) c)
      (== HIGH pc)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW)) HIGH (assign h (- h low)) LOW)))

(test-check "simple-!-o-5"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(output ,LOW (intexp (1))) c)
      (== HIGH pc)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '())

(test-check "simple-!-o-6"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(output ,LOW (intexp (1))) c)
      (== LOW pc)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW)) LOW (output LOW (intexp (1))) LOW)))

(test-check "simple-!-o-2"
  (run* (q)
    (fresh (gamma pc c l)
      (== `((h low) (,HIGH ,LOW)) gamma)
      (== `(while (> h (intexp ())) (assign h (- h low))) c)
      (!-o gamma pc c l)
      (== `(,gamma ,pc ,c ,l) q)))
  '((((h low) (HIGH LOW))
     HIGH
     (while (> h (intexp ())) (assign h (- h low)))
     HIGH)
    (((h low) (HIGH LOW))
     LOW
     (while (> h (intexp ())) (assign h (- h low)))
     HIGH)))

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

(test-check "eval-expo-1"
  (run 10 (q)
    (fresh (e m v)
      (eval-expo e m v)
      (== `(,e ,m ,v) q)))
  '(((intexp _.0) _.1 (intval _.0))
    ((_.0 ((_.0 . _.1) (_.2 . _.3)) _.2) (sym _.0))
    ((_.0 ((_.1 _.0 . _.2) (_.3 _.4 . _.5)) _.4) (=/= ((_.0 _.1))) (sym _.0))
    ((_.0 ((_.1 _.2 _.0 . _.3) (_.4 _.5 _.6 . _.7)) _.6) (=/= ((_.0 _.1)) ((_.0 _.2))) (sym _.0))
    ((_.0 ((_.1 _.2 _.3 _.0 . _.4) (_.5 _.6 _.7 _.8 . _.9)) _.8) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3))) (sym _.0))
    ((_.0 ((_.1 _.2 _.3 _.4 _.0 . _.5) (_.6 _.7 _.8 _.9 _.10 . _.11)) _.10) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4))) (sym _.0))
    ((_.0 ((_.1 _.2 _.3 _.4 _.5 _.0 . _.6) (_.7 _.8 _.9 _.10 _.11 _.12 . _.13)) _.12) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))) (sym _.0))
    ((- (intexp _.0) (intexp _.0)) _.1 (intval ()))
    ((_.0 ((_.1 _.2 _.3 _.4 _.5 _.6 _.0 . _.7) (_.8 _.9 _.10 _.11 _.12 _.13 _.14 . _.15)) _.14) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5)) ((_.0 _.6))) (sym _.0))
    ((_.0 ((_.1 _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8) (_.9 _.10 _.11 _.12 _.13 _.14 _.15 _.16 . _.17)) _.16) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5)) ((_.0 _.6)) ((_.0 _.7))) (sym _.0))))

(test-check "eval-expo-2"
  (run 10 (q)
    (fresh (e e1 e2 bin-op m v)
      (== `(,bin-op ,e1 ,e2) e)
      (eval-expo e m v)
      (== `(,e ,m ,v) q)))
  '(((- (intexp _.0) (intexp _.0)) _.1 (intval ()))
    ((- (intexp (_.0 . _.1)) (intexp ())) _.2 (intval (_.0 . _.1)))
    ((- (intexp (0 1)) (intexp (1))) _.0 (intval (1)))
    (((- (intexp _.0) _.1) ((_.1 . _.2) ((intval _.0) . _.3)) (intval ())) (sym _.1))
    ((> (intexp _.0) (intexp _.0)) _.1 (intval ()))
    (((- (intexp (_.0 . _.1)) _.2) ((_.2 . _.3) ((intval ()) . _.4)) (intval (_.0 . _.1))) (sym _.2))
    ((- (intexp (1 _.0 . _.1)) (intexp (1))) _.2 (intval (0 _.0 . _.1)))
    ((> (intexp (_.0 . _.1)) (intexp ())) _.2 (intval (1)))
    (((- (intexp (0 1)) _.0) ((_.0 . _.1) ((intval (1)) . _.2)) (intval (1))) (sym _.0))
    ((- (intexp (0 0 1)) (intexp (1))) _.0 (intval (1 1)))))

(test-check "eval-expo-3"
  (run 10 (q)
    (fresh (e e1 e2 m v)
      (== `(> ,e1 ,e2) e)
      (eval-expo e m v)
      (== `(,e ,m ,v) q)))
  '(((> (intexp _.0) (intexp _.0)) _.1 (intval ()))
    ((> (intexp (_.0 . _.1)) (intexp ())) _.2 (intval (1)))
    (((> (intexp _.0) _.1) ((_.1 . _.2) ((intval _.0) . _.3)) (intval ())) (sym _.1))
    ((> (intexp ()) (intexp (_.0 . _.1))) _.2 (intval ()))
    ((> (intexp (_.0 _.1 . _.2)) (intexp (1))) _.3 (intval (1)))
    ((> (intexp (1)) (intexp (_.0 _.1 . _.2))) _.3 (intval ()))
    (((> _.0 (intexp _.1)) ((_.0 . _.2) ((intval _.1) . _.3)) (intval ())) (sym _.0))
    (((> (intexp (_.0 . _.1)) _.2) ((_.2 . _.3) ((intval ()) . _.4)) (intval (1))) (sym _.2))
    ((> (intexp (_.0 _.1 _.2 . _.3)) (intexp (_.4 1))) _.5 (intval (1)))
    (((> (intexp ()) _.0) ((_.0 . _.1) ((intval (_.2 . _.3)) . _.4)) (intval ())) (sym _.0))))
