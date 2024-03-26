;; goal:           a function that, when given a substitution, produces a stream
;; association:    a key/value pair, using a dotted pair
;; substitution:   a list of substitutions; keys must be a set
;; stream:         empty list / pair whose cdr is a stream / a suspension

;; empty-s
(define empty-s '())

;; succeed
(define succeed
  (lambda (s)  ;; s
    `(,s)))    ;; stream

;; fail
(define fail
  (lambda (s)
    '()))

;; var
(define var (lambda (x) (vector x)))
;; var?
(define var? (lambda (v) (vector? v)))

;; walk: get to terminal meaning for a given var
(define (walk v s)
  (let ((m (and (var? v) (assv v s))))
    (if (pair? m)
      (walk (cdr m) s)
      v)))

;; walk*: walk it. is it a pair? cons the walk of the car onto the walk
;; of the cdr
(define (walk* v s)
  (let ((v (walk v s)))
    (if (pair? v)
      (cons (walk (car v) s)
            (walk (cdr v) s))
      v)))

;; occurs?: does v have a reference to x?
;; 1. are x & terminal v equal? true
;; 2. is terminal v a pair? check the car/cdr for x
;; 3. false
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

;; unify: produce new bindings or false
(define (unify x v s)
  (let ((x (walk x s))
        (v (walk v s)))
    (cond
      ((eqv? x v) s)
      ((var? x) (ext-s x v s))
      ((var? v) (ext-s v x s))
      ((and (pair? x)
            (pair? v))
       (let ((s (unify (car x)
                       (car v) s)))
         (and s ;; account for possibility that unification on the car failed.
              (unify (cdr x) (cdr v) s))))
      (else #f))))

;; ==
(define (== x y)
  (lambda (s) ;; substitution
    (let ((s (unify x y s)))
      (if s
        `(,s)    ;; stream success
        '()))))  ;; stream fail

;; disj2
;; a goal that takes two goals and adds their subtitutions
;; each success, an alternate possibility.
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

;; append-inf: "merge w streams"
(define (append-inf s1 s2) ;; two streams
  (cond
    ((null? s1) s2)
    ((pair? s1)
     (cons (car s1)
           (append-inf (cdr s1) s2)))
    (else
      (lambda ()
        (append-inf s2 (s1))))))

;; take-inf : just take, but can iterate through suspensions
(define take-inf
  (lambda (n s)
    (cond
      ((and n (zero? n)) '())
      ((null? s) '())
      ((pair? s)
       (cons (car s)
             (take-inf (and n (sub1 n))
                       (cdr s))))
      (else
        (take-inf n (s))))))

;; conj2: a goal that takes two goals
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g1 (g2 s))))

;; append-map: goal & s-inf
;; map over an inf stream, applying g to each subtitution
(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf)) ;; append-inf as cons, here
                 (append-map-inf g (cdr s-inf))))
    (else
      (lambda ()
        (append-map-inf g (s-inf)))))))

;; call/fresh
(define (call/fresh f name)
  (f (var name)))

;; reify-name: 0 -> "_0"
;; FLUB 1: should go from string to symbol
(define (reify-name n)
  (string->symbol
    (string-append "_"
                   (number->string n))))

;; reify-s - this is entirely wrong. see below.
(define (reify-s v r)
  (cond
    ((null? v) '())
    ((var? v)
     (cons (reify-name (car v))
           (reify-s (cdr v))))
    (else
      (reify-s (cdr v)))))

;; correct
;; _. walk v w/ respect to r (which is init'd to '())
;; 1. is v a var? cons reified-name onto r; use the length of r - brilliant
;; 2. is v a pair? recur on the cdr using the r from recuring on the car
;; 3. else return r
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

;; test
(let ((b (var 'b)))
  (reify-s `(a ,b c)))

;; reify (a goal)
;; 1. `(a ,b c)
;; 2. `(,b . 0) : a different 'type, than s; built from empty-s
;; 3. `(a _0 c)
(define (reify v)
  (lambda (s)                        ;; FLUB 1: this is a goal
    (let* ((v (walk* v s))
           (r (reify-s v empty-s)))  ;; FLUB 2&3: takes empty-s, returns a reified-name-subsitution
      (walk* v s))))

;; run-goal: (pass)
(define (run-goal n g)
  (take-inf n (g empty-s)))
