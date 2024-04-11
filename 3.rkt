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
          ((nullo l))
          ((fresh (a)
             (caro l a)
             (listo a))
           (fresh (d)
             (cdro l d)
             (lolo d)))))

(run* q
  (fresh (x y)
    (lolo `((a b) (,x c) (d ,y)))))

(run 3 l (lolo l))

(let ((l (var 'l)))
  (run-goal 3 (lolo l)))

;; investigations...
(defrel (breako x)
        (== x "BREAKO!"))


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


(1 2 101 3 201 4   102 5   202 6   103 7   203 8   104 9   204 10  105 11)
(1 2 101 3 4   102 5   201 6   103 7   301 8   104 9   202 10  105 11  302)

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
;; loso
;; membero
;; proper-membero
