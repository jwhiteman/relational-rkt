(define succeed
  (lambda (s)
    `(,s)))

(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

;; (append-map vector->list '(#(1) #(2 3) #(4)))
(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
                 (append-map-inf g (cdr s-inf))))
    (else
      (lambda ()
        (append-map-inf g (s-inf))))))

(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf)))
    (else
      (lambda ()
        (append-inf t-inf (s-inf))))))

;; 1.
(run* q
  (conj2 succeed succeed))

(let ((q (var 'q)))
  (run-goal #f
            (conj2 succeed succeed)))

;; 1. reify is per substitution (probably via map)
;; 2. is done against a single variable [question: how does (run (q b ..) ...) work?]
;; 3. terminal meaning -> +for contained vars -> replace fresh vars w/ numbers
;;    note: _0 appears in a list here because we're mapping
(let ((q (var 'q)))
  (map (reify q)
       (run-goal #f
                 (conj2 succeed succeed))))

;; 2.
(run* q
  (conj2 succeed (== 'corn q)))

(let ((q (var 'q)))
  (map (reify q) (run-goal #f (conj2
                                succeed
                                (== 'corn q)))))

;; 3
;; fail => empty stream
;; append-map(empty-stream => '()
;; (reify '() q)           => '()
(run* q
  (conj2 fail (== 'corn q)))

;; 4
;; l evaluates to the stream w/ single substitution: '((,q . corn))
;; is mapped over, first s being (,q . corn)
;; ...which causes the map function/goal (== 'mail q) to fail
;; (append '()) => '()
(run* q
  (conj2 (== 'corn q) (== 'meal q)))

;; 5
;; append-map:
;; stream is ( ((,q . corn)) )
;; g is the unifictation (== 'corn q)
;; g is applied to the single substittion - unification succeeds -
;; output: '((,q . corn))
;; |> reify w/ respect to q:'(corn)
(run* q
  (conj2 (== 'corn q) (== 'corn q)))

;; 6
;; append '() '() => '()
(run* q
  (disj2 fail fail))

;; 7
;; append ( ((,q . olive)) ) () => (((,q . olive)))
;; map over stream using (reify q) => '(olive)
;; note: reify uses walk*/reify-s/walk*
(run* q
  (disj2 (== 'olive q) fail))

;; 8
;; append () ({q: oil}) => ({q: oil})
;; => '(oil)
(run* q
  (disj2 fail (== 'oil q)))

;; 9
;; append ({q: olive}) ({q: oil}) => ({q: olive}, {q: oil})
;; then map both substitutions, via reify => (olive oil)
(run* q
  (disj2 (== 'olive q) (== 'oil q)))

;; 10
;; append ({q: ($x $y)}) ({q: ($y, $x)) => ( {q: ($x $y)}, {q: ($y, $x)} )
;; map w/ (reify q) => ((0 1) (0 1))
(run* q (fresh (x)
          (fresh (y) (disj2
                       (== `(,x ,y) q)
                       (== `(,y ,x) q)))))

;; 11
;; append ({x: olive}) ({x: oil}) => ({x: olive}, {x: oil})
;; map w/ (reify x) => (olive oil)
(run* x
  (disj2 (== 'olive x) (== 'oil x)))

;; 12 - ditto, w/ result in different order
(run* x
  (disj2 (== 'oil x) (== 'olive x)))

;; 13
;; append
;;   1: append-map ({x: olive}), fail => '()
;;   2. ({x: oil})
;; append (), ({x: oil}) => ({x: oil})
;; map w/ (reify x) '(oil)
(run* x
  (disj2
    (conj2 (== 'olive x) fail) (== 'oil x)))

;; 14
;; append
;;   1. append-map ({x: olive}), succeed =>  ({x: olive})
;;   2. ({x: oil})
;; append ({x: olive}) ({x: oil}) => ({x: olive}, {x: oil})
;; map w/ (reify x) => '(olive oil)
(run* x
  (disj2
    (conj2 (== 'olive x) succeed) (== 'oil x)))

;; 15 - ditto, but w/ result in different order - (oil olive)
(run* x (disj2
          (== 'oil x)
          (conj2 (== 'olive x) succeed)))

;; 16
;; append
;;   - append-map: ({x: virgin}), fail           => ()
;;   - append:                                   => ({x: olive}, {}, {x: oil})
;;     - ({x: olive})
;;     - append: (()) ({x: oil})                 => ({}, {x: oil})
;; append (), ({x: olive}, {}, {x: oil})         => ({x: olive}, {}, {x: oil})
;; map w/ (reify x)                              => (olive _0 oil)
(run* x
  (disj2
    (conj2 (== 'virgin x) fail)
    (disj2
      (== 'olive x) (disj2
                       succeed
                       (== 'oil x)))))

;; 17
(run* r
  (fresh (x)
    (fresh (y) (conj2
                 (== 'split x) (conj2
                                  (== 'pea y)
                                  (== `(,x ,y) r))))))

;; 18
(run* r
  (fresh (x)
    (fresh (y) (conj2
                 (conj2 (== 'split x) (== 'pea y))
                 (== `(,x ,y) r)))))

;; 19
(run* (x y) ;; which macro is this using?
  (disj2
    (conj2 (== 'split x) (== 'pea y))
    (conj2 (== 'red x) (== 'bean y))))

;; 20
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
