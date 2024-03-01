(load "trs.scm")

;; CARO:
(defrel (caro l v)
        (fresh (_)
               (== (cons v _) l)))

(run* r
  (caro '(x y z) r))
(run* l
  (caro l 'z))

;; CDRO:
(defrel (cdro l v)
        (fresh (d)
               (== (cons d v) l)))

(run* r
  (cdro '(x y z) r))
(run* l
  (cdro l '(a b)))

;; CONSO:
;; "the head of the thing on the left is the head of the thing on the right,
;;  and the rest of the thing on the left is the rest of the thing on the right"
(defrel (conso e l v)
        (== `(,e . ,l) v))
(run* l
  (conso 'x '(y z) l))
(run* l
  (conso l '(y z) '(x y z)))
(run* l
      (conso 'x l '(x y z)))


;; define caro using conso
;; "the head of the thing on the left is the thing on the right"
'(a b c) x
x        '(a b c)
`(,l b c) 'q

(defrel (car-o l v)
        (fresh (d)
               (conso v d l)))
(run* r
  (car-o '(x y z) r))
(run* l
  (car-o l 'z))

;; define cdro using conso
;; "the rest of the thing on the left is the thing on the right"
(defrel (cdr-o l v)
        (fresh (d)
               (conso d v l)))

(run* r
  (cdr-o '(x y z) r))
(run* l
  (cdr-o l '(a b)))
