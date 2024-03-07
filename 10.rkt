;; var
(define var (lambda (x) (vector x)))

;; var?
(define var? (lambda (x) (vector? x)))

;; walk     : step through an assoc list?
(define (walk v s)
  (let ((a (and (var? v) (assoc v s))))
    (if (pair? a)
      (walk (cdr a) s)
      v)))

;; 1. v needs to be a var and /in/ the assoc list, otherwise it just gets handed back
;;   - not a var? you get it back.
;;   - not in the assoc list? you get it back
;; 2. if v is a var, and it exists in the bindings, then you 100% get a pair back
;; 3. it recurs on the cdr (aka the meaning)
;; 4. if the meaning is itself a var and in the bindings, the process continues
;;    if it's not, then we're at terminal meaning, and it is returned
;; 5. what if '(3 #(x) z) is returned? will it step into that? no.
;;    1. it's not a var
;;    2. even if it was (and it's not) it would need to be in the assoc list
;;    - without conditions one & two, "a" will never be anything other than false
;;      which means its destined to be returned back: terminal meaning.

;; one of two things are going to happen: we're either going to end up with
;; some non-var primitive (we don't allow a non-var to be on the left hand
;; side of this semantic structure - i.e it can't refer to anything else),
;; or we get to a var that doesn't refer to anything (fresh?)

(let* ((x (var 'x))
       (y (var 'y))
       (z (var 'z))
       (s (list `(,x . ,y) `(,y . ,z) `(,z . 42))))
  (walk y s))

(let* ((x (var 'x))
       (y (var 'y))
       (z (var 'z))
       (s (list `(,x . ,y) `(,y . (1 ,z 3)))))
  (walk y s))

;; this is an invalid s list (due to the semantic cycle) but it's still walkable
;; best guess: this is enforced elsewhere and would break elsewhere
(let* ((x (var 'x))
       (y (var 'y))
       (z (var 'z))
       (w (var 'w))
       (s (list `(,x . (a ,y)) `(,z . ,w) `(,y . (,x)))))
  (map (lambda (v) (walk v s))
       (list x y z w)))


;; occurs?  : given the bindings, is the meaning of the left already contained
;;            in the meaning of the right?

;; ext-s    : extend bindings; checks for cycles w/ occurs
;; unify    : reconcile two "things" against the bindings
