;; FAIL.

(define (var x) (vector x))
(define (var? v) (vector? v))

;; empty-s
(define empty-s '())

;; walk
(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (if (pair? a)
      (walk (cdr a) s)
      v)))

;; occurs?
(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((eqv? x v) #t)
      ((pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else
        #f))))

;; ext-s
(define (ext-s x v s)
  (if (occurs? x v s)
    #f
    (cons `(,x . ,v) s)))

;; unify
(define (unify l r s)
  (let ((l (walk l s))
        (r (walk r s)))
    (cond
      ((eqv? l r) s)
      ((var? l) (ext-s l r s))
      ((var? r) (ext-s r l s))
      ((and (pair? l) (pair? r))
       (let ((s (unify (car l) (car r) s)))
         (and s (unify (cdr l) (cdr r) s))))
      (else
        #f))))

;; ==
(define ==
  (lambda (l r)
    (lambda (s)
      (let ((s (unify l r s)))
        (if s
          `(,s)
          '())))))

;; succeed
(define succeed
  (lambda ()
    (lambda (s)
      `(,s))))

;; fail
(define fail
  (lambda ()
    (lambda (s)
      '())))

;; disj2
(define disj2
  (lambda (g1 g2)
    (lambda (s)
      (append-inf (g1 s) (g2 s)))))

;; append-inf
(define append-inf
  (lambda (s-inf t-inf)
    (cond
      ((null? s-inf) t-inf)
      ((pair? s-inf)
       (cons (car s-inf)
             (append-inf (cdr s-inf) t-inf)))
      (else
        (lambda ()
          (append-inf t-inf (s-inf)))))))

;; take-inf
(define take-inf
  (lambda (n s-inf)
    (cond
      ((null? s-inf) '())
      ((and n (zero? n)) '())
      ((pair? s-inf)
       (cons (car s-inf)
             (take-inf (and n (sub1 n))
                       (cdr s-inf))))
      (else
        (take-inf n (s-inf))))))

;; conj2
(define conj2
  (lambda (g1 g2)
    (lambda (s)
      (append-map-inf g2 (g1 s)))))

;; append-map-inf
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

;; reify-name
(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_"
                     (number->string n)))))

;; walk*
(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (if (pair? v)
        (cons (walk* (car v) s)
              (walk* (cdr v) s))
        v))))

;; reify-s
;; (,x c ,y) => {$x: 0, $y: 1}
(define reify-s
  (lambda (v r)
    (let ((v (walk v r)))
      (cond
        ((var? v)
         (let* ((n (length r))
                (rn (reify-name n)))
               (cons `(,v . ,n)  r)))
        ((pair? v)
         (reify-s (cdr v)
                  (reify-s (car v) r)))
        (else r)))))

;; reify
;; (,x c ,y)
(define reify
  (lambda (s)
    (lambda (v)
      (let* ((v (walk* v s))
             (r (reify-s v empty-s)))
        (walk* v r)))))

;; run-goal
(define run-goal
  (lambda (g)
    (take-inf #f (g empty-s))))
