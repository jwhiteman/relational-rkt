;; 1
(run* q fail)

(run-goal #f fail)
;; (fail empty-s)    => '()
;; (take-inf #f '()) => '()

;; 2
(run* q (== 'pea 'pod))

(let ((q (var 'q)))
  (run-goal #f (== 'pea 'pod)))
;; '()

;; 3
(run* q (== q 'pod))

(let ((q (var 'q)))
  (run-goal #f (== q 'pod)))
;; '(((#(q) . pod)))


;; 4
(run* q (== 'pea q))

(let ((q (var 'q)))
  (run-goal #f (== 'pea q)))
;; == -> unify -> ext-s -> occurs?
;;    -> `(,s) || '()
;;
;; '(((#(q) . pea)))

;; 5
(let ((q (var 'q)))
  (run-goal #f succeed))
;; '(())

(let ((x (var 'x)))
  (run-goal #f
            (disj2 (== 'olive x)
                   (== 'oil x))))

(let ((x (var 'x)))
  (run-goal #f
            (conj2 (== 'olive x)
                   (== 'oil x))))

;; 6
(run* x
      (disj2
        (conj2 (== 'olive x) fail)
        (== 'oil x)))

(let ((x (var 'x)))
  (run-goal #f
            (disj2
              (conj2 (== 'olive x) fail)
              (== 'oil x))))
;; '(((#(x) . oil)))

(let ((x (var 'x)))
  (run-goal #f
            (disj2
              (== 'oil x)
              (conj2 (== 'olive x) fail))))
;; '(((#(x) . oil)))

;; nesting of disj2 / conj2: 
;; unify/walk etc only take s, not streams
;; how is it (if it is the case) that goals only receive s, never streams?

;; hypothesis 1: via the macros, you're never not using either conj2 or disj2
;;   ...conda/i/u ?
