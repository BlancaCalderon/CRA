; 1. Operaciones de listas

(define concatenacion (lambda (n)
                        (lambda (m)
                          ())))













(define (longitud lista)
  (if (equal? lista '())
      0
      (+ 1 (longitud (cdr lista)))))

(longitud lst)