;; goal:
;;   a function that takes a substitution and returns a goal
;; association
;;   key => value
;; substitution
;;   a list of associations; acyclic; keys must be vars
;; stream
;;   1. an empty list
;;   2. a pair whose cdr is a stream
;;   3. a suspension

;; empty-s
(define empty-s '())

;; var/var?
(define var (lambda (x) (vector x)))
(define var? (lambda (v) (vector? v)))

(var? (var 'x))

;; walk
;; terminal meaning
(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (if (pair? a)
      (walk (cdr a) s)
      v)))

;; walk*
(define (walk* v s)
  (let ((v (walk v s)))
    (if (pair? v)
      (cons (walk* (car v) s)
            (walk* (cdr v) s))
      v)))

;; occurs?
(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((eqv? x v) #t)
      ((pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else #f))))

;; ext-s
(define (ext-s x v s)
  (if (occurs? x v s)
    #f
    (cons `(,x . ,v) s)))

;; unify: reconcile
(define (unify l r s)
  (let ((l (walk l s))
        (r (walk r s)))
    (cond
      ((eqv? l r) s)
      ((var? l) (ext-s l r s))
      ((var? r) (ext-s r l s))
      ((pair? r)
       (let ((s (unify (car l) (car r) s)))
         (and s (unify (cdr l) (cdr r) s))))
      (else #f))))

;; ==
(define (== l r)
  (lambda (s)
    (let ((s (unify l r s)))
      (if s
        `(,s)
        s))))

;; why not (succeed) ?
(define succeed
  (lambda (s)
    `(,s)))

;; why not (fail) ?
(define fail
  (lambda (s)
    '()))

;; disj2
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

;; append-inf
;; question: under what circumstances is s1 a suspension vs contains a suspension?
(define append-inf
  (lambda (s1-inf s2-inf)
    (cond
      ((null? s1-inf) s2-inf)
      ((pair? s1-inf)
       (cons (car s1-inf)
             (append-inf (cdr s1-inf) s2-inf)))
      (else
        (lambda ()
          (append-inf s2-inf (s1-inf)))))))

;; take-inf
(define take-inf
  (lambda (n s-inf)
    (cond
      ((and n (zero? n)) '())
      ((null? s-inf) '())
      ((pair? s-inf)
       (cons (car s-inf)
             (take-inf (and n (sub1 n))
                       (cdr s-inf))))
      (else
        (take-inf n (s-inf))))))


;; conj2
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g1 (g2 s))))

;; append-map-inf
;; question: what would happen if cons was used instead of append-inf?
;; difference between append & append-map?
(define append-map-inf
  (lambda (g s-inf)
    (cond
      ((null? s-inf) '())
      ((pair? s-inf)
       (append-inf (g (car s-inf))
                   (append-map-inf g (cdr s-inf))))
      (else
        (lambda ()
          (append-map-inf g (s-inf)))))))

;; call/fresh
(define call/fresh
  (lambda (f name)
    (f (var name))))

;; reify-name
(define (reify-name n)
  (string->symbol
    (string-append "_"
                   (number->string n))))

;; reify-s
;; var? cons it w/ reify
;; pair?
;; else return as is
(define (reify-s v r)
  (let ((v (walk v r))) ; FLUB: use walk instead of walk* - only interested in keys
    (cond
      ((var? v)
       (let ((n (reify-name (length r))))
         (cons `(,v . ,n) r)))
      ((pair? v)
       (reify-s (cdr v)
                (reify-s (car v) r)))
      (else r))))
;; '((#(y) . _2) (#(z) . _1) (#(x) . _0))

(define (run-goal n g)
  (take-inf n (g empty-s)))


(let ((x (var 'x))
      (y (var 'y))
      (z (var 'z)))
  (reify-s `((,x . ,z) (,z . 42) (,y . '(a b c))) empty-s))

(let ((v (var 'v)))
  (reify-s v empty-s))

;; reify
; 1. ,x -> '(a ,b c)
; 2.    -> '((b . _0))
; 3.    -> '(a _0 c)
(define (reify v)
  (lambda (s)                         ;; can't walk w/o an s
    (let* ((v (walk* v s))            ;; walk the var
           (r (reify-s v empty-s)))   ;; transform to reify-name s
      (walk* v r))))                  ;; walk v w/ respect to r

;; run-goal
(define run-goal
  (lambda (n g)
    (take-inf n (g empty-s))))
