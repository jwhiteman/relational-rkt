(defrel (caro p a)
        (fresh (d)
               (== (cons a d) p)))

(run* q (caro '(a c o r n) q))

(defrel (cdro p a)
        (fresh (d)
               (== p
                   (cons (caro p d) a))))

(run* q (cdro '(a c o r n) q))
