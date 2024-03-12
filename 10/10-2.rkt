;; var
;; var?
(define var (lambda (x) (vector x)))
(define var? (lambda (v) (vector? v)))

(var? (var 'x))
(var? 'x)

(cdr (assv 'x '((x . 42))))

(define (walk v bindings)
  "walk -> get to terminal meaning"
  "1. v is not a var: v"
  "2. v is a var, but not a key in the bindings: v"
  "3. v is a var, in the bindings, but doesn't refer to a pair: v"
  "4. v is a var, in the bindings, and refers to a pair: recur with what v points to"
  (let ((a (and (var? v) (assv v bindings))))
    (if (pair? a)
      (walk (cdr a) bindings)
      v)))

(walk 'v '())       ;; not a var
(let* ((v (var 'v))
       (x (var 'x))
       (bindings `((,x . 42))))
  (walk v bindings)) ;; a var, but not in bindings
(let* ((v (var 'v))
       (x (var 'x))
       (bindings `((,v . 42))))
  (walk v bindings)) ;; a var, in bindings, refers to a non-pair
(let* ((v (var 'v))
       (x (var 'x))
       (bindings `((,v . ,x) (,x . 42))))
  (walk v bindings)) ;; a var, in bindings, refers to a non-pair

;; occurs?
(define (occurs? x v bindings)
  "1. does v walk to x?"
  "2. does v contain an element that walks to x?"
  "...if we allowed 'z -> '(z), we'd get a cycle"
  (let ((v (walk v bindings)))
    (cond
      ((eqv? x v) #t)
      ((pair? v)
       (or (occurs? x (car v) bindings)
           (occurs? x (cdr v) bindings)))
      (else #f))))

;; v does not walk to x
(let* ((v (var 'v))
       (x (var 'x))
       (s '()))
  (occurs? x v s))

;; v walks to x
(let* ((v (var 'v))
       (x (var 'x))
       (s `((,v . ,x))))
  (occurs? x v s))

;; v contains an element that walks to x
(let* ((v (var 'v))
       (x (var 'x))
       (z (var 'z))
       (s `((,v . (w ,z y))
            (,z . ,x))))
  (occurs? x v s))

;; ext-s
(define (ext-s x v bindings)
  (if (occurs? x v bindings)
    #f
    (cons `(,x . ,v) bindings)))

;; unify
(define (unify x y bindings)
  "1. x & y are equal? tautology -> return bindings"
  "2. x is a var? add x -> y if x doesn't occur in y"
  "3. y is a var? add y -> x if y doesn't occur in x"
  "4. x & y are pairs: unify the head of each & the rest of each"
  "5. false"
  (let ((x (walk x bindings))
        (y (walk y bindings)))
    (cond
      ((eqv? x y) bindings)
      ((var? x) (ext-s x y bindings))
      ((var? y) (ext-s y x bindings))
      ((and (pair? x)
            (pair? y))
       (let ((bindings (unify (car x) (car y) bindings)))
         (and bindings
              (unify (cdr x) (cdr y) bindings))))
      (else #f))))

;; 1
(unify '(a b c) '(a b c) "bindings unchanged!")
;; 2
(let ((v (var 'v)))
  (unify v '(a b c) '()))
(let ((v (var 'v)))
  (cdr (assv v (unify v '(a b c) '()))))
(let ((v (var 'v)))
  (cdr (assv v (unify v 42 '()))))

(cdr (assv 'x '((a 1) (b 2) (x 3) (z 9))))
;; 4
(let ((x (var 'x))
      (y (var 'y)))
  (unify `(a ,x c) `(,y b c) '()))
