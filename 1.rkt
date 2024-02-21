;; (require minikranen)
(load "trs.scm")

(run* (q) fail)

(run* (q) (== 'pea 'pod))

;; "give me the set of all q that solve the following"
(run* (q) (== q 'pea))

(run* (q) succeed)
;; '(_.0)

(run* (q) (== 'pea 'pea))
(run* (q) (== q q))

(run* (q)
  (fresh (x)
    (== q 'pea)))

(run* (q)
      (fresh (x)
             (== (cons q (cons x (cons 3 '())))
                 '(1 2 3))))

(run* (q)
      (fresh (x)
             (== `(,x) q))) ;; note the backtick

(run* (q)
      (fresh (x)
             (== x q)))

(run* (q)
      (fresh (x)
             (== `(((,q)) ,x) `(((,x)) 'pod))))

(run* (q)
      (fresh (x)
             (fresh (y)
                    (== `(,q ,y) `((,x ,y) ,x)))))

;; page 10
(run* (q)
      (fresh (x)
             (fresh (y)
                    (== `(,x ,y) q))))
;; '((_.0 _.1))

;; page 12
;; (run* (q)
;;      (conj2 succeed succeed))
;; conj2 doesn't seem to be implemented - just a pedogogical device - it's boolean and

(run* (r)
      (fresh (x)
             (fresh (y)
                    (== x 'split)
                    (== y 'pea)
                    (== `(,x ,y) r))))

(run* (r)
      (fresh (x y z) ;; fresh /can/ take multiple args
             (== `(,x ,y ,z) r)))

(run* (r)
      (fresh (x y z) ;; fresh /can/ take multiple args
             (== `(,x ,y ,z) r)))

(run* (r)
      (fresh (x)
             (fresh (y)
                    (== 'split x)
                    (fresh (z)
                           (== x 'split)
                           (== y 'pea)
                           (== z 'soup)
                           (== r `(,x ,y ,z))))))

(run* ((x y z)
       (== x 'split)
       (== y 'pea)
       (== z 'soup)))

;; not avail in canonical minikanren...
(run* q fail)
(run* (q r s) succeed)
(run* (x y)
  (conj2
    (== x 'pea)
    (== y 'pod)))
(run* (x)
  (disj2
    (== x 'pea)
    (== x 'pod)))

(defrel (teacupo t)
        (disj2
          (== 'tea t)
          (== 'cup t)))

(run* (x y)
      (disj2
        (conj2 (teacupo x) (== #t y))
        (conj2 (== #f x) (== #t y))))
