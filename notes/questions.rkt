;; these first two are the same
(let ((v (var 'v)))
  (unify v 'y '()))

(let ((v (var 'v)))
  (unify 'y v '()))

;; walk: return non-var or var not on key-side of bindings
;; let's say we have A -> B -> nothing
;; if A later -> 'x, the lookup (if things are working correctly)
;; will go A => B -> 'x, so bidirectionality shouldn't matter
(walk '(a b c) '())                ;; => '(a b c)
(unify '(a b c) '(a b c) '(1 . 2)) ;; just gives S bac

;; these two end up being different:
;; so i'm not sure how var fusing works, exactly
(let ((v (var 'v))
      (z (var 'z)))
  (unify z v '()))

(let ((v (var 'v))
      (z (var 'z)))
  (unify v z '()))

;; ok - this is false, thankfully
(let* ((v (var 'v))
       (s (unify v 'y '())))
  (unify v 'x s))

;; more on bi-directionality
;; v & z wil be vars
;; 1. fuse them together by having v -> z
;; 2. set v to something concrete: 'x
;; 3. walk v. what is the result? (=> 'x)
;; 4. walk z. what is the result? (=> 'x)
;;
;; what does s look like at the end?
;;   '((#(z) . x) (#(v) . #(z)))
;;
;; so v points to z, as per UNO
;; but when attempting to unify v with something else,
;; we walk it first and end up with #(z) - which is fresh
;; and we end up ext-s #(z) -> x. pretty cool.
;;
;; next: what if we then try to unify #(v) [or #(z)] with a new fresh var?
;; ...with a var that points to 'x
;; ...with a var that points to something contradictory?
;; ...with a contradictory primitive?
;; build up mental models here
;;
;; later: repeat this, but have v point to a list, with vars
(let* ((v (var 'v))
       (z (var 'z))
       (s (unify v z '())) ;; UNO
       (s (unify v 'x s))) ;; DOS
   s)
