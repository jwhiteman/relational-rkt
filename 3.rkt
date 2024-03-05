(load "trs.scm")

;; PRELIMS:
(defrel (conso a b v)
        (== `(,a . ,b) v))

(defrel (pairo v)
        (fresh (a b)
               (conso a b v)))

(defrel (cdro l v)
        (fresh (d)
               (== (cons d v) l)))

(defrel (nullo l)
        (== l '()))

(defrel (debug)
        (and (print "made it") succeed))

;; LISTO
(defrel (listo l)
        (conde
          ((nullo l))
          ((fresh (d)
                  (cdro l d)
                  (listo d)))))

(run 1 x (listo `(a b c . ,x)))
(run 5 x (listo `(a b c . ,x)))
(run 5 zebra (listo `(a b c . ,zebra)))

;; scratch...
(run 1 x
     (fresh (a b c)
            (== `(,x,a,b,c) `(1 ,a 2 ,c))
            (and (print `(,x,a,b,c)) succeed)))

(defrel (check-fail x)
        (debug)
        (== x 'y)
        (debug)
        (== x 'z)
        (debug))
(run* x (check-fail x))
