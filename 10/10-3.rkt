#lang racket
(define var (lambda (x) (vector x)))
(define var? (lambda (v) (vector? v)))

(assv 'v '((v . 42)))

(define empty-s '())

;; walk: get to terminal meaning
;; 1. is v a var?
;; 2. ...does the var have a value in bindings?
;; 3. ...is it a pair? recur. else: terminal meaning
(define walk
  (lambda (v bindings)
    (let ((a (and (var? v) (assv v bindings))))
      (if (pair? a)
        (walk (cdr a) bindings)
        v))))

;; occurs?
;; 1. does x walk to v?
;; 2. does any element of v walk to x?
(define occurs?
  (lambda (x v bindings)
    (let ((v (walk v bindings)))
      (cond
        ((eqv? v x) #t)
        ((pair? v)
         (or (occurs? x (car v) bindings)
             (occurs? x (cdr v) bindings)))
        (else #f)))))

;; ext-js
;; allow x -> v if v has no reference of x
(define ext-s
  (lambda (x v bindings)
    (if (occurs? x v bindings)
      #f
      (cons `(,x . ,v) bindings))))

;; unify
;; 1. l & r are equivalent: tautaology
;; 2. l is a var
;; 3. r is a var
;; 4. l & r are pairs
;; 5. else: #f
(define unify
  (lambda (l r bindings)
    (let ((l (walk l bindings))
          (r (walk r bindings)))
      (cond
        ((eqv? l r) bindings)
        ((var? l) (ext-s l r bindings))
        ((var? r) (ext-s r l bindings))
        ((and (pair? l) (pair? r))
         (let ((bindings
                 (unify (car l) (car r) bindings)))
           (and bindings
                (unify (cdr l) (cdr r) bindings))))
        (else #f)))))

;; fail
;; GOAL that takes an s (stream?) and returns the empty list
(define fail
  (lambda (s)
    '()))

;; succeed
;; GOAL that takes an s and returns it within a list
(define succeed
  (lambda (s)
    `(,s)))

;; ==
;; GOAL: that attempts to unify l & r
;; if successful, it will return a new s, in a list
;; if not, the empty list
(define (== l r)
  (lambda (s)
    (let ((s (unify l r s))) ;; needed to account for the fact that unify can return #f
      (if s `(,s) '()))))

;; disj2
;; GOAL: given two other goals, apply each to s and append the left to the right.
;; NOTE: what is the difference between a substitution list and a stream?
;; what do these represent? bindings? lists of bindings?
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

;; append-inf
;; not a goal
;; given two streams, recursively append the left stream to the right stream.
;; if a suspension is encountered in the left stream, return a suspension that,
;; once release, will append the right stream to the encountered left suspension
;; in released form
(define append-inf
  (lambda (s1-inf s2-inf)
    (cond
      ((null? s1-inf) s2-inf)
      ((pair? s1-inf)
       (cons (car s1-inf)
             (append-inf (cdr s1-inf)
                         s2-inf)))
      (else
        (lambda ()
          (append-inf s2-inf (s1-inf)))))))

;; take-inf
;; not a goal
;; like the normal take, but works with a stream and can take through a suspension
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

;; never-o
;; GOAL -> endless suspension
(define (never-o)
  (lambda (s)
    (lambda ()
      ((never-o) s))))

;; always-o
;; GOAL
(define (always-o)
  (lambda (s)
    (lambda ()
      ((disj2 succeed (always-o)) s))))
;;            '()  .  <procedure>
;;                       '()     .   <procedure>
;;                                       '()       .  <procedure>

(take-inf 3 ((always-o) empty-s)) ;; '(() () ())
(take-inf 7 ((always-o) empty-s)) ;; '(() () () () () () ())

(take-inf 5
          ((disj2
             (== 'olive (var 'x))
             (== 'oil (var 'x)))
           empty-s))

;; bindings: solution set or relational set
;; we can have multiple solutions / relations for a given goal or set of goals
;; do bindings ever contain suspensions? best guess: no
;; why? take-inf iterates through them; 
;; but disj2 calls to append-inf, which /can/ return a suspension
;; but this isn't returned to the client;
;; but goals can produce suspensions that can produce still more suspensions
;; to what degree do suspensions, via their closures, represent backtracking?


;; 1: uno
(let ((v (var 'x))
      (s empty-s))
  ((disj2 (== v 'tea)
          (== v 'cup))
   s))

;; 2: dos
(let* ((v (var 'x))
       (s `((,v . 'p1pz))))
  ((disj2 (== v 'tea)
          (== v 'cup))
   s))


(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
                 (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

;; tres
(let ((v (var 'x))
      (s empty-s))
  ((conj2 (== v 'tea)
          (== v 'cup))
   s))

;; cuatro
(let ((v (var 'v))
      (w (var 'w))
      (s empty-s))
  ((conj2 (== v 'tea)
          (== w 'cup))
   s))

;; question: can a stream ever be fed to a goal?

;; COURSE 5:

;; apply the passed f to the var'd name:
(define (call/fresh name f)
  (f (var name)))

(call/fresh 'x
            (lambda (v)
              (print "made it!")))

;; num -> string -> symbol:
(define (reify-name n)
  (string->symbol
    (string-append "_"
                   (number->string n))))

(reify-name 42)

;; walk it. is it a pair? create a new pair from walking both parts of the
;; current.
(define (walk* v s)
  (let ((v (walk v s)))
    (if (pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s))
       v)))

(let* ((w (var 'w))
       (x (var 'x))
       (y (var 'y))
       (z (var 'z))
       (s `((,x . b) (,z . ,y) (,w . (,x e ,z)))))
  (walk w s))

(let* ((w (var 'w))
       (x (var 'x))
       (y (var 'y))
       (z (var 'z))
       (s `((,x . b) (,z . ,y) (,w . (,x e ,z)))))
  (walk* w s))

(define (reify-s v r)
  (let ((v (walk v r)))
    (cond
      ((var? v)
       (let* ((n  (length r))
              (rn (reify-name n)))
         (cons `(,v . ,rn) r)))
      ((pair? v)
       (let ((r (reify-s (car v) r)))
         (reify-s (cdr v) r)))
      (else r))))

(define (reify v)
  (lambda (s)
    (let* ((v (walk* v s))
           (r (reify-s v empty-s)))
      (walk* v r))))

(let* ((u   (var 'u))
       (v   (var 'v))
       (w   (var 'w))
       (x   (var 'x))
       (y   (var 'y))
       (z   (var 'z))
       (a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
       (a2 `(,y . corn))
       (a3 `(,w . (,v ,u)))
       (s  `(,a1 ,a2 ,a3)))
  ((reify x) s))
;; => '(_0 (_1 _0) corn _2 ((ice) _2))

(let* ((u   (var 'u))
       (v   (var 'v))
       (w   (var 'w))
       (x   (var 'x))
       (y   (var 'y))
       (z   (var 'z))
       (a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
       (a2 `(,y . corn))
       (a3 `(,w . (,v ,u)))
       (s  `(,a1 ,a2 ,a3)))
  (reify-s x s))

;; 1. for a given var & a given substitution
;; 2. walk the var, through its pair if it refers to one
;;    note: the result of this, is not a substituion;
;;    but terminal-meaning, and for all its contained values.
;; 3. |> create a reifed-name substitution
;;    i.e create a lookup w/ var => _n
;; 4. take the meaning and replace all vars with the values of the lookup ^
(let* ((u   (var 'u))
       (v   (var 'v))
       (w   (var 'w))
       (x   (var 'x))
       (y   (var 'y))
       (z   (var 'z))
       (a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
       (a2 `(,y . corn))
       (a3 `(,w . (,v ,u)))
       (s  `(,a1 ,a2 ,a3)))
  (reify-s (walk* x s) empty-s))
;;       x     =>  (,u ,w ,y ,z ((ice) ,z)))
;; walk* x s   => '(#(u) (#(v) #(u)) corn #(z) ((ice) #(z)))
;; |> reify-s  => '((#(z) . _2) (#(v) . _1) (#(u) . _0))
;;             => '(_0 (_1 _0) corn _2 ((ice) _2))

(let ((x (var 'x)))
  (map (reify x)
       (take-inf 5
                 ((disj2 (== 'olive x) (== 'oil x))
                  empty-s))))
;; => '(olive oil)

(define (run-goal n g)
  (take-inf n (g empty-s)))
