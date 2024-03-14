;; *** VAR
(var? (var 'x))
(var? 'x)

(cdr (assv 'x '((x . 42))))

;; *** WALK
(walk 'v '())       ;; not a var
(let* ((v (var 'v))
       (x (var 'x))
       (bindings `((,x . 42))))
  (walk v bindings)) ;; a var, but not in bindings
(let* ((v (var 'v))
       (x (var 'x))
       (bindings `((,v . 42))))
  (walk v bindings)) ;; a var, in bindings, refers to a non-pair
(let* ((v (var 'v))
       (x (var 'x))
       (bindings `((,v . ,x) (,x . 42))))
  (walk v bindings)) ;; a var, in bindings, refers to a non-pair

;; *** OCCURS:
;; v does not walk to x
(let* ((v (var 'v))
       (x (var 'x))
       (s '()))
  (occurs? x v s))

;; v walks to x
(let* ((v (var 'v))
       (x (var 'x))
       (s `((,v . ,x))))
  (occurs? x v s))

;; v contains an element that walks to x
(let* ((v (var 'v))
       (x (var 'x))
       (z (var 'z))
       (s `((,v . (w ,z y))
            (,z . ,x))))
  (occurs? x v s))

;; *** UNIFY:
;; 1
(unify '(a b c) '(a b c) "bindings unchanged!")
;; 2
(let ((v (var 'v)))
  (unify v '(a b c) '()))
(let ((v (var 'v)))
  (cdr (assv v (unify v '(a b c) '()))))
(let ((v (var 'v)))
  (cdr (assv v (unify v 42 '()))))

(cdr (assv 'x '((a 1) (b 2) (x 3) (z 9))))
;; 4
(let ((x (var 'x))
      (y (var 'y)))
  (unify `(a ,x c) `(,y b c) '()))


;; *** ==:
(let* ((x (var 'x))
       (y (var 'y))
       (z (var 'z))
       (s `(,z . 42)))
  ((== `(,x b c) `(,z b ,y)) `(,s)))

(run* (x y z)
      (== z 42)
      (== `(,x b c) `(,z b ,y)))
