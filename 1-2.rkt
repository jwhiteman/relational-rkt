(let ((q (var 'q))
      (x (var 'x)))
  (map (reify x)
       (run-goal #f (== q x))))

(let ((q (var 'q))
      (x (var 'x)))
  (run-goal #f (== `(((,q)) ,x) `(((,x)) pod))))

(let ((q (var 'q))
      (x (var 'x)))
  (run-goal #f (== `(,x ((,q))) `(pod ((,x))))))

(let ((q (var 'q))
      (x (var 'x)))
  (run-goal #f (== `(((,x)) ,x) `(((,q)) pod))))

(run* (x y) (conde
              ((fresh (z)
                 (== 'lentil z)))
              ((== x y))))

(let ((x (var 'x))
      (y (var 'y))
      (z (var 'z)))
  (run-goal #f
            (conde
              (== 'lentil z)
              (== x y))))
