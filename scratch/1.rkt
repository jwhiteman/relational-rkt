(load "trs.scm")

(defrel (teacupo x)
        (disj2 (== x 'tea) (== x 'cup)))

(let ((x (var 'x))
      (y (var 'y)))
  (run-goal #f
            (disj2 (teacupo x)
                   (== y 'jones))))
;; x:tea | x:cup | y:jones

(let ((x (var 'x))
      (y (var 'y)))
  (run-goal #f
            (conj2 (teacupo x)
                   (disj2 (teacupo x)
                          (== y 'jones)))))

;; METHOD 1: empty-s all then reconcile
;; conj2
;;   x:tea | x:cup
;;   x:tea | x:cup | y:jones
;:   x:tea | ()    | x:tea, y:jones
;;   ()    | x:cup | xcup, y:jones

;;   => y:jones, x:tea | x:tea | y:jones, x:cup | x:cup

(run* (x y)
  (conj2 (teacupo x)
         (disj2 (teacupo x)
                (== y 'jones))))

(run* (x)
  (fresh (y)
    (conj2 (teacupo x)
           (disj2 (teacupo x)
                  (== y 'jones)))))

(run* (x y)
  (disj2 (teacupo x)
         (== y 'jones)))

(run* (x) (teacupo x))
;; why '((tea) (cup)) instead of (tea cup) ?
;; because the stream had two substitutions - it's a way of showing two alternates;
;; x can't be both tea & cup, except in the context of multiple substitutions;
;; having them wrapped just separates them.

(let ((x (var 'x)))
  (run-goal #f (teacupo x)))
;; x:tea | x:cup


; x:tea | x:cup
; x:tea | x:cup
(let ((x (var 'x)))
  (run-goal #f (conj2 (teacupo x) (teacupo x))))
