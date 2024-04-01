(run* q
  (conj2 succeed succeed))

(run* q
  (conj2 succeed (== 'corn q)))

(run* q
  (conj2 fail (== 'corn q)))

(run* q
  (conj2 (== 'corn q) (== 'meal q)))

(run* q
  (conj2 (== 'corn q) (== 'corn q)))

(run* q
  (disj2 fail fail))

(run* q
  (disj2 (== 'olive q) fail))

(run* q
  (disj2 fail (== 'oil q)))

(run* q
  (disj2 (== 'olive q) (== 'oil q)))

(run* q (fresh (x)
          (fresh (y) (disj2
                       (== `(,x ,y) q)
                       (== `(,y ,x) q)))))

(run* x
  (disj2 (== 'olive x) (== 'oil x)))

(run* x
  (disj2 (== 'oil x) (== 'olive x)))

(run* x
  (disj2
    (conj2 (== 'olive x) fail) (== 'oil x)))

(run* x
  (disj2
    (conj2 (== 'olive x) succeed) (== 'oil x)))

(run* x (disj2
          (== 'oil x)
          (conj2 (== 'olive x) succeed)))

(run* x
  (disj2
    (conj2 (== 'virgin x) fail)
    (disj2
      (== 'olive x) (disj2
                       succeed
                       (== 'oil x)))))

(run* r
  (fresh (x)
    (fresh (y) (conj2
                 (== 'split x) (conj2
                                  (== 'pea y)
                                  (== `(,x ,y) r))))))

(run* r
  (fresh (x)
    (fresh (y) (conj2
                 (conj2
                   (== 'split x)
                   (== 'pea y)) (== `(,x ,y) r)))))

(run* (x y)
  (disj2
    (conj2 (== 'split x) (== 'pea y))
    (conj2 (== 'red x) (== 'bean y))))

(run* r
  (fresh (x y)
    (conj2 (disj2
             (conj2 (== 'split x) (== 'pea y))
             (conj2 (== 'red x) (== 'bean y)))
           (== `(,x ,y soup) r))))

(defrel (teacupo t)
         (disj2 (== 'tea t) (== 'cup t)))

(define (teacupo t) (lambda (s)
                      (lambda ()
                        ((disj2 (== 'tea t) (== 'cup t))
                         s))))

(run* (x y)
  (disj2
    (conj2 (teacupo x) (== #t y))
    (conj2 (== #f x) (== #t y))))

(run* (x y)
  (teacupo x)
  (teacupo y))

(run* (x y)
  (teacupo x)
  (teacupo x))

(run* (x y)
  (disj2
    (conj2 (teacupo x) (teacupo x))
    (conj2 (== #f x) (teacupo y))))

(run* (x y) (conde
              ((fresh (z) (== 'lentil z))) ((== x y))))

(run* (x y) (conde
              ((== 'split x) (== 'pea y))
              ((== 'red x) (== 'bean y))
              ((== 'green x) (== 'lentil y))))
