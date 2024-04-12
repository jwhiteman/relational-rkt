(load "trs.scm")

;; PRELIMS:
(defrel (conso conso-h conso-t conso-l)
        (== `(,conso-h . ,conso-t) conso-l))

(defrel (pairo v)
        (fresh (a b)
               (conso a b v)))

(defrel (caro caro-l caro-h)
        (fresh (caro-t)
          (== (cons caro-h caro-t) caro-l)))

(defrel (cdro cdro-l cdro-t)
        (fresh (cdro-h)
          (== (cons cdro-h cdro-t) cdro-l)))

(defrel (nullo nullo-l)
        (== nullo-l '()))

;; LISTO
(defrel (listo listo-l)
        (conde
          ((nullo listo-l))
          ((fresh (fresh-t)
                  (cdro listo-l fresh-t)
                  (listo fresh-t)))))

(run 1 x (listo `(a b c . ,x)))

(let ((x (var 'x)))
  (run-goal 3 (listo `(a b c . ,x))))

(run 5 x (listo x))

;; LOLO
(defrel (lolo l)
        (conde
          ((nullo l))         ;; a
          ((fresh (a)
             (caro l a)
             (listo a))       ;; b
           (fresh (d)
             (cdro l d)
             (lolo d)))))     ;; c

;; this seems clearer to me:
(defrel (lolo-2 l)
        (conde
          ((nullo l))
          ((fresh (a b)
             (conso a b l)
             (listo a)
             (lolo-2 b)))))

(run* q
  (fresh (x y)
    (lolo `((a b) (,x c) (d ,y)))))

(run 10 l (lolo l))
(run 20 l (lolo-2 l))

;; dat interleaving:
;; ()             ;; a
;; (())           ;; b
;; ((_0))         ;; b
;; (() ())        ;; c: a
;; ((_0 _1))      ;; b
;; (() (_0))      ;; c: b
;; ((_0) ())      ;; c: b
;; (() () ())     ;; c: c: a
;; ((_0 _1 _2))   ;; b
;; (() (_0 _1))   ;; c: b


(let ((l (var 'l)))
  (run-goal 3 (lolo l)))

;; investigations...
(defrel (breako x) (== x "BREAKO!"))


(defrel (rando x)
        (conde
          ((== x (random 10000)))
          ((rando x))))
(run 20 x (rando x))

(defrel (nexto x n)
        (conde
          ((== x (add1 n)))
          ((nexto x (add1 n)))))

(run 10 x (nexto x 0))
(run 10 x (nexto x -100))
(run 10 x (nexto x 0))

(defrel (repeato x n)
        (conde
          ((== x n))
          ((repeato x n))))

(defrel (intero x n)
        (conde
          ((repeato x n))
          ((repeato x (+ 1 n)))
          ((repeato x (+ 2 n)))
          ((intero x (+ 3 n)))))

(run 30 x (intero x 0))

(define-values (intero)
  (lambda (x n)
    (lambda (s)
      (lambda ()
        (#%app (#%app disj2
                (#%app (#%top . repeato) x n)
                (#%app disj2
                 (#%app (#%top . repeato) x (#%app + (quote 1) n))
                 (#%app (#%top . intero) x (#%app + (quote 2) n)))) s)))))>

(defrel (intero-z x n)
        (conde
          ((repeato x n))
          ((repeato x (+ 1 n)))
          ((repeato x (+ 2 n)))))

(run 20 x (intero-z x 0))
(run 20 x (intero x 0))


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

(defrel (intero-2 x)
        (conde
          ((nexto x 0))
          ((nexto x 100))
          ((nexto x 200))
          ;; ((nexto x 300))
          ;;((nexto x 400))
          ;;((nexto x 500))
          ))

(run 20 x (intero-2 x))

(defrel (intero-3 x)
        (conde
          ((nexto x 0))
          ((nexto x 100))
          ((breako x))
          ((nexto x 200))))

(run 30 x (intero-3 x))

;;(expand-syntax #'(...definition of intero-2...)
(define-values (intero-2)
  (lambda (x)
    (lambda (s)
      (lambda ()
        (#%app (#%app disj2
                (#%app nexto x (quote 0))
                (#%app disj2
                 (#%app nexto x (quote 100))
                 (#%app nexto x (quote 200)))) s)))))>

;; singletono
(defrel (singletono l)
        (fresh (a)
          (== `(,a) l)))

;; loso
(defrel (loso l)
        (conde
          ((nullo l))
          ((fresh (a b)
             (conso a b l)
             (singletono a)
             (loso b)))))

(run 5 l (loso l))
(run 5 x (loso (list 42 x)))  ;; 14 fails singletono

;; membero
;; proper-membero
