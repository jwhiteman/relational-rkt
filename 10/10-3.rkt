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
