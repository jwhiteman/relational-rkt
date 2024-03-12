;; VAR:
(define var
  (lambda (v) (vector v)))

(var 'x)
(var 'y)
(var 'z)

;; VAR?:
(define var?
  (lambda (v) (vector? v)))

(var? 'x)
(var? (var 'x))

;; ASSV:
(assv 'y '((x . 1) (y . 2) (z . 3))) ;; '(y . 2)
(assv 'q '((x . 1) (y . 2) (z . 3))) ;; #f

;; WALK:
;; get to terminal meaning
;; any v must be on the key (left) side of the s/assoc-list/bindings
;; RE: terminal meaning
;; 1. v is not a var
;; 2. v is a var, but not a key
;; 3. v is a var, and a key, but the value is not a pair
(define walk ;; [3]
  (lambda (v bindings)
    (let ((a (and (var? v) (assv v bindings)))) ;; is v a var that is a key?
      (if (and a (pair? a))                     ;; ...and is it a pair?
        (walk (cdr a) bindings)                 ;; ...recur with the cdr
        v))))                                   ;; no? we're at terminal meaning

;; macro idea: combination of let* and #'and

;; OCCURS?  #t | #f     [2]
;; 1. does v walk to x?
;; 2. does v contain anything that walks to x?
(define occurs?
  (lambda (x v bindings)
    (let ((v (walk v bindings)))
      (cond
        ((var? v) (eqv? v x))
        ((pair? v)
         (or (occurs? x (car v) bindings)
             (occurs? x (cdr v) bindingss)))
        (else #f)))))

;; NOTE: s is never cdr'd or otherwise divided; meaning only has a single level;

;; EXT-S:  new-bindings | #f   [1]
;; add an entry to the bindings
(define (ext-s v x bindings)
  (if (occurs? x v bindings)
    #f
    (cons `(,v . ,x) bindings)))

;; UNIFY:  new-bindings | #f   [5]
;; 1. are l & r equal? a tautology: return the bindings
;; 2. is l a var? attempt to add l -> r to the bindings
;; 3. is r a var? ditto
;; 4. are l & r both pairs? unify the cars of each & the cdrs of each
;; note: the resulting bindings of the former will be the bindings of the latter
;; 5. false
(define (unify l r bindings)
  (let ((l (walk l bindings))  ;; this pre-walking does the heavy lifting of bidirectionality
        (r (walk r bindings))) ;; ditto
    (cond
      ((eqv? l r) bindings)
      ((var? l) (ext-s l r bindings))
      ((var? r) (ext-s r l bindings))
      ((and (pair? l) (pair? r))
       (let ((bindings (unify (car l) (car r) bindings)))
         (and bindings
              (unify (cdr l) (cdr r) bindings))))
      (else #f))))
