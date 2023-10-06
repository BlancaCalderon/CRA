#lang racket/gui

;-------------------------------------------------------------
; Booleanos
(define true (lambda (x y) x))

(define false (lambda (x y) y))

(define neg (lambda (x) (x false true)))
                         
(define myand (lambda (x y) (x y false)))

(define or (lambda (x y) (x true y)))

; Pares ordenados
              
(define par (lambda (x)
              (lambda (y)
                (lambda (f) (f x y)))))

(define primero (lambda (p) (p true)))

(define segundo (lambda (p) (p false)))

;;;;; Combinador de punto fijo

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;;;;; Orden en naturales y test de nulidad

(define esmenoroigualnat (lambda (n)
                             (lambda (m)
                                (escero ((restanat n) m)))))
                         
(define esmayoroigualnat (lambda (n)
                            (lambda (m)
                               (escero ((restanat m) n)))))
                         
(define esmenornat (lambda (n)
                     (lambda (m)
                       (myand ((esmenoroigualnat n) m) (noescero ((restanat m) n))))))

(define esmayornat (lambda (n)
                     (lambda (m)
                       (myand ((esmayoroigualnat n) m) (noescero ((restanat n) m))))))

(define esigualnat (lambda (n)
                     (lambda (m)
                       (myand ((esmayoroigualnat n) m) ((esmenoroigualnat n) m)))))

(define escero (lambda (n)
                 ((n (lambda (x) false)) true)))

(define noescero (lambda (n)
                    (neg (escero n))))

; Aritmética natural. Se define también comprobar para verificar que la cosa va bien. Defino algunos naturales para hacer comprobaciones. Los escribo en francés para distinguirlos de los enteros 
; que escribiré en español.

(define zero (lambda (f)
               (lambda (x) x)))

(define sucesor (lambda (n)
                  (lambda (f)
                    (lambda (x)
                     (f((n f) x))))))

(define un (sucesor zero))

(define deux (sucesor un))

(define trois (sucesor deux))

(define quatre (sucesor trois))

(define cinq (sucesor quatre))

(define six (sucesor cinq))

(define sept (sucesor six))

(define huit (sucesor sept))

(define neuf (sucesor huit))

(define dix (sucesor neuf))

(define onze (sucesor dix))

(define douze (sucesor onze))

(define treize (sucesor douze))

(define quatorze (sucesor treize))

(define quinze (sucesor quatorze))

(define seize (sucesor quinze))

(define dix-sept (sucesor seize))

(define dix-huit (sucesor dix-sept))

(define dix-neuf (sucesor dix-huit))

(define vingt (sucesor dix-neuf))

(define comprobar (lambda (n)
                    ((n (lambda (x) (+ 1 x))) 0)))

(define sumnat (lambda (n)
                 (lambda (m)
                   ((n (lambda (x) (sucesor x))) m))))

(define prodnat (lambda (n)
                   (lambda (m)
                     (lambda (f)
                       (lambda (x) ((m (n f)) x))))))
                     
(define prefn (lambda (f)
                (lambda (p)
                  ((par (f (primero p))) (primero p)))))

(define predecesor (lambda (n)
                     (lambda (f)
                       (lambda (x)
                            (segundo ((n ((lambda (g)
                                             (lambda (p) ((prefn g) p))) f)) ((par x) x)))))))
                         
(define restanat (lambda (n)
                     (lambda (m)
                        ((m (lambda (x) (predecesor x))) n))))                                                 

(define restonataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                 (lambda (x)
                    ((((esmayoroigualnat x) m)  
                        (lambda (no_use)
                            (f ((restanat x) m))
                        )
                        (lambda (no_use)
                            x
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
))

(define restonat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((restonataux n) m))) zero))))


(define cocientenataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (x)
                    ((((esmayoroigualnat x) m)  
                        (lambda (no_use)
                            (sucesor (f ((restanat x) m)))  
                        )
                        (lambda (no_use)
                            zero
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
    )
)

(define cocientenat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((cocientenataux n) m))) zero))))


(define mcdnat
    (lambda (n)
        (lambda (m)
            (((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                      (((escero y)  
                       (lambda (no_use)
                            x
                        ) 
                       (lambda (no_use)
                            ((f y)((restonat x) y)) 
                        )
                        
                    )
                        zero)    ; Pasa zero como argumento de no_use
                ))
            ))
                n) ; Pasa n como el valor inicial de x.
          m)       ; Pasa m como el valor inicial de y.
    )
))

;;;;;; Definición de algunos enteros

(define cero ((par zero) zero))

(define -uno ((par zero) un))

(define -dos ((par zero) deux))

(define -tres ((par zero) trois))

(define -cuatro ((par zero) quatre))

(define -cinco ((par zero) cinq))

(define -seis ((par zero) six))

(define -siete ((par zero) sept))

(define -ocho ((par zero) huit))

(define -nueve ((par zero) neuf))

(define -diez ((par zero) dix))

(define -once ((par zero) onze))

(define -doce ((par zero) douze))

(define -trece ((par zero) treize))

(define -catorce ((par zero) quatorze))

(define -quince ((par zero) quinze))

(define -dieciseis ((par zero) seize))

(define -diecisiete ((par zero) dix-sept))

(define -dieciocho ((par zero) dix-huit))

(define -diecinueve ((par zero) dix-neuf))

(define -veinte ((par zero) vingt))

(define uno ((par un) zero))

(define dos ((par deux) zero))

(define tres ((par trois) zero))

(define cuatro ((par quatre) zero))

(define cinco ((par cinq) zero))

(define seis ((par six) zero))

(define siete ((par sept) zero))

(define ocho ((par huit) zero))

(define nueve ((par neuf) zero))

(define diez ((par dix) zero))

(define once ((par onze) zero))

(define doce ((par douze) zero))

(define trece ((par treize) zero))

(define catorce ((par quatorze) zero))

(define quince ((par quinze) zero))

(define dieciseis ((par seize) zero))

(define diecisiete ((par dix-sept) zero))

(define dieciocho ((par dix-huit) zero))

(define diecinueve ((par dix-neuf) zero))

(define veinte ((par vingt) zero))

;;;;; Orden, valor absoluto y tests de nulidad, positividad y negatividad. 
;;;
;;; m-n > m'-n' si y solo si m+n' > m'+n e igual con el resto

(define esmayoroigualent (lambda (r)
                           (lambda (s)
                             ((esmayoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r)))))) 

(define esmenoroigualent (lambda (r)
                           (lambda (s)
                             ((esmenoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmayorent (lambda (r)
                           (lambda (s)
                             ((esmayornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenorent (lambda (r)
                           (lambda (s)
                             ((esmenornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esigualent (lambda (r)
                           (lambda (s)
                             ((esigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define absoluto (lambda (r)
                    (((esmayoroigualnat (primero r)) (segundo r)) ((par ((restanat (primero r)) (segundo r))) zero) ((par ((restanat (segundo r)) (primero r))) zero))))

(define negativo (lambda (r)
                   ((esmenorent r) cero)))

(define positivo (lambda (r)
                   ((esmayorent r) cero)))

(define esceroent (lambda (r)
                     ((esigualnat (primero r)) (segundo r))))
                      
(define noesceroent (lambda (r)
                       (neg (esceroent r))))

;;;;; Reducción a representante canónico de la clase de equivalencia.

(define reducir (lambda (r)
                  (((esmayoroigualnat (primero r)) (segundo r)) 
                        ((par ((restanat (primero r)) (segundo r))) zero)
                        ((par zero) ((restanat (segundo r)) (primero r))))))

;;;;; Aritmética entera. La respuesta está siempre dada por el representante canónico de la clase de equivalencia. 

(define testenteros (lambda (r)
                      (- (comprobar (primero r)) (comprobar (segundo r)))))

(define sument (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat (primero r)) (primero s))) ((sumnat (segundo r)) (segundo s)))))))

(define prodent (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat ((prodnat (primero r)) (primero s))) ((prodnat (segundo r)) (segundo s))))
                          ((sumnat ((prodnat (primero r)) (segundo s))) ((prodnat (segundo r)) (primero s))))))))                       

(define restaent (lambda (r)
                   (lambda (s)
                     (reducir ((par ((sumnat (primero r)) (segundo s))) ((sumnat (segundo r)) (primero s)))))))

;; Lo siguiente reduce la división de enteros a división de naturales. Si m \ge 0 y n> 0, y si q y r son cociente y resto de la división de m entre n, se tiene
;;  m  = q       * n        + r
;;  m  = (-q)    * (-n)     + r
;; -m  = (-(q+1))* n        + (n-r)
;; -m  = (q+1)   * (-n)     + (n-r),
;; siempre y cuando el resto no sea cero. Cuando el divisor es cero, la función cocienteent devuelve false.

(define cocienteent_aux (lambda (r)
                          (lambda (s)
                            ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))

; Caso1: resto cero. Si m= q*n, entonces -m= (-q)*n, -m = q* (-n) y m= (-q)*(-n).

(define cocienteentaux-caso1 (lambda (r)
                               (lambda (s)
                                  ((or (myand ((esmayoroigualent r) cero) (positivo s)) (myand (negativo r) (negativo s))) ((par ((cocientenat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                                       ((par zero) ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))))
                              
; Caso 2: resto no nulo

(define cocienteentaux-caso2 (lambda (r)
                                (lambda (s)
                                    (((esmayoroigualent r) cero) ((positivo s) ((par ((cocienteent_aux r) s)) zero) ((par zero) ((cocienteent_aux r) s)))
                                                                 ((positivo s) ((par zero) (sucesor ((cocienteent_aux r) s))) ((par (sucesor ((cocienteent_aux r) s))) zero))))))
; Cociente cuando no hay división por cero

(define cocienteentaux (lambda (r)
                         (lambda (s)
                           ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) ((cocienteentaux-caso1 r) s) ((cocienteentaux-caso2 r) s)))))

; Cociente considerando la división por cero

(define cocienteent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((cocienteentaux r) s))) zero))))

; Resto. Si se divide por cero, devuelve false

(define restoentaux1 (lambda (r)
                        (lambda (s)
                          ((or (myand ((esmayoroigualent r) cero) (positivo s)) (myand ((esmayoroigualent r) cero) (negativo s))) ((par ((restonat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                           ((par ((restanat (primero (absoluto s)))((restonat (primero (absoluto r))) (primero (absoluto s))))) zero)))))

(define restoentaux (lambda (r)
                       (lambda (s)
                          ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) cero ((restoentaux1 r) s)))))

(define restoent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((restoentaux r) s))) zero))))

;; Como mcd (r,s)=mcd(|r|,|s|), se tiene

(define mcdent (lambda (r)
                 (lambda (s)
                   ((par ((mcdnat (primero (absoluto r))) (primero (absoluto s)))) zero))))



;-------------------------------------------------------------------------------------------------------------------------
;Operaciones base de las listas
;-------------------------------------------------------------------------------------------------------------------------

(define nil (lambda (z) z))

;Lista vacia
(define null primero)

;Constructor de la lista
(define const (lambda (x)
                      (lambda (y)
                        ((par false) ((par x) y)))))
;Primer elemento de la lista
(define hd (lambda (z)
             (primero(segundo z))))

;Cola de la lista
(define tl (lambda (z)
             (segundo (segundo z))))

;-------------------------------------------------------------------------------------------------------------------------
;Operaciones de las listas
;-------------------------------------------------------------------------------------------------------------------------

;Operacion que calcula la longitud de una lista pasada
(define longitud (lambda (l)
                   (((null l) (lambda (no_use) zero) (lambda (no_use) (longitudaux l))) zero)))		;si la lista no es vacia llama a longitudaux para seguir recorriendo la lista

;Operacion auxiliar de longitud para simular la recursividad
(define longitudaux (lambda (l)
                   ((Y (lambda (f)
                         (lambda (x)
                           ((
                             (null x)         								;si la lista es vacia se termina la ejecucion     
                             (lambda (no_use)
                               zero
                               )
                             (lambda (no_use)
                               (sucesor (longitud (tl x)))						;si no es vacia se suma uno y se hace llamada a longitud con el resto de la lista para recorrerla
                               )
                             )
                            zero)
                           )
                         ))
                    l)
                   ))

;-------------------------------------------------------------------------------------------------------------------------

;Operacion que concatena dos listas l1 y l2
(define concatenar (lambda (l1)
                     (lambda (l2)
                       (((null l1) (lambda (no_use) l2) (lambda (no_use) ((concatenaraux  l1) l2))) zero) )))	;si l1 no es vacia llama de nuevo a concatenaraux para seguir recorriendo la lista

;Operacion auxiliar de concatenar para simular la recursividad
(define concatenaraux (lambda (l1)
                        (lambda (l2)
                          ((Y (lambda (f)
                                (lambda (x)
                                  ((
                                    (null x)   						;Si la lista 1 es vacia se termina y devuelve l2  (l2 contiene la union de los elementos de l1)          
                                    (lambda (no_use)
                                      l2
                                      )
                                    (lambda (no_use)
                                      ((const (hd x)) ((concatenar (tl x)) l2))		;si la lista 1 no es vacia se forma lista con su cabeza y la llamada a concatenar pasandole la cola de l1 para recorrer la lista
                                      )
                                    )
                                   zero)
                                  )
                                ))
                           l1)
                          )))

;-------------------------------------------------------------------------------------------------------------------------

;Operacion que muestra los elementos que contiene lista dada
(define mostrar (lambda (l)
                   (((null l) (lambda (no_use) (display "")) (lambda (no_use) (mostraraux l))) zero)))		;si la lista es vacia no muestra nada y termina, si no llama a mostraraux

;operacion auxiliar para recorrer lista de mostrar
(define mostraraux (lambda (l)
                   ((Y (lambda (f)
                         (display (testenteros(hd l))) (display " ")						;muestra cabeza de la lista por consola
                         (lambda (x)	
                           ((
                             (null x)              								;si la lista es vacia se termina
                             (lambda (no_use)
                               (display "")
                               )
                             (lambda (no_use)
                               (mostrar (tl x))									;si no es vacia se llama a mostrar con el resto de la lista continuando su recorrido
                               )
                             )
                            zero)
                           )
                         ))
                    l)
                   ))
;-------------------------------------------------------------------------------------------------------------------------

;Operacion inversion que da la vuelta a una lista dada
(define inversion (lambda (l)
                    (((null l) (lambda (no_use) nil) (lambda (no_use) (inversionaux l))) zero)))               ;si la lista pasada no es vacia se llama a inversionaux

;operacion auxiliar de inversion para simular recursividad
(define inversionaux (lambda (l)
                       ((Y (lambda (f)
                             (lambda (x)
                               ((
                                 (null x)              							       ;si la lista es vacia se termina la ejecucion y devuelve nil para indicar final de la lista
                                 (lambda (no_use)
                                   nil
                                   )
                                 (lambda (no_use)
                                   ((concatenar (inversion (tl x))) ((const (hd x)) nil))		       ;si no es nula se concatena nueva llamada con el resto de la lista con la cabeza de la lista invirtiendo asi el orden
                                   )
                                 )
                                zero)
                               )
                             ))
                        l)
                       ))

;-------------------------------------------------------------------------------------------------------------------------

;Operacion que determina si elementos recibido pertenece a lista pasada
(define pertenece (lambda (l)
                    (lambda (e)
                      (((null l) (lambda (no_use) false) (lambda (no_use) ((perteneceaux l) e))) zero)))) 	;si lista es vacio no encuentra elemento y devuelve falso, si no es vacia se llama a perteneceaux

;operacion auxiliar de pertenece para simular recursividad
(define perteneceaux (lambda (l)
                       (lambda (e)
                         ((Y (lambda (f)
                               (lambda (x)
                                 ((
                                   ((esigualent (hd l)) e)        						;revisa si el elemento es igual a la cabeza de la lista y si lo es devuelve true   
                                   (lambda (no_use)
                                     true
                                     )
                                   (lambda (no_use)
                                     ((pertenece (tl x)) e)							;si el elemento no es igual sigue recorriendo la lista llamando a pertenece con la cola de la lista
                                     )
                                   )
                                  zero)
                                 )
                               ))
                          l)
                         )))

;-------------------------------------------------------------------------------------------------------------------------
;Operaciones de la codificacion de los enteros
;-------------------------------------------------------------------------------------------------------------------------

;Operacion que devuelve la suma de todos los elementos de una lista
(define sumarlista (lambda (l)
                   (((null l) (lambda (no_use) cero) (lambda (no_use) (sumarlistaaux l))) cero)))              ;si no es vacia llama a sumarlistaaux

;operacion auxiliar para realizar recursividad de sumarlista
(define sumarlistaaux (lambda (l)
                        ((Y (lambda (f)
                              (lambda (x)
                                ((
                                  (null x)              
                                  (lambda (no_use)								;si la lista es vacia se termina devolviendo cero
                                    cero
                                    )
                                  (lambda (no_use)
                                    ((sument (hd x)) (sumarlista (tl x)))					;si no es vacia se suma la cabeza de la lista a la llamada a sumarlista que recibe el resto de la lista para recorrerla
                                    )
                                  )
                                 cero)
                                )
                              ))
                         l)
                        ))

;-------------------------------------------------------------------------------------------------------------------------

;Operacion que devuelve el maximo de una lista, recibe la lista y el elemento maximo actual
(define maxlista (lambda (l)
                   (lambda (n)
                     (((null l) (lambda (no_use) n) (lambda (no_use) ((maxlistaaux l)n))) cero))))		;si la lista es vacia devuelve el elemento y si no llama a maxlistaaux

;operacion auxiliar para simular recursividad de maxlista
(define maxlistaaux (lambda (l)
                      (lambda (n)
                        ((Y (lambda (f)
                            (lambda (x)
                              ((
                                ((esmayorent (hd l)) n)        							;comprueba si la cabeza de la lista es mayor que el maximo actual      
                                (lambda (no_use)
                                  ((maxlista (tl l)) (hd l))							;si lo es llama a maxlista con el nuevo maximo que es la cabeza de la lista y el resto de la lista
                                  )
                                (lambda (no_use)
                                  ((maxlista (tl l)) n)								;si la cabeza no es mayor se llama a maxlista con el mismo elemento pasado y el resto de la lista
                                  )
                                )
                               cero)
                              )
                            ))
                       l)
                      )))

;Operacion que devuelve el minimo de una lista, recibe la lista y el elemento minimo actual
(define minlista (lambda (l)
                   (lambda (n)
                     (((null l) (lambda (no_use) n) (lambda (no_use) ((minlistaaux l)n))) cero))))		;si la lista es vacia devuelve el elemento y si no llama a minlistaaux

(define minlistaaux (lambda (l)
                      (lambda (n)
                        ((Y (lambda (f)
                            (lambda (x)
                              ((
                                ((esmenorent (hd l)) n)              						;comprueba si la cabeza de la lista es menor que el minimo actual 
                                (lambda (no_use)
                                  ((minlista (tl l)) (hd l))							;si lo es llama a minlista con el nuevo minimo que es la cabeza de la lista y el resto de la lista
                                  )
                                (lambda (no_use)
                                  ((minlista (tl l)) n)								;si la cabeza no es menor se llama a minlista con el mismo elemento pasado y el resto de la lista
                                  )
                                )
                               cero)
                              )
                            ))
                       l)
                      )))

;-------------------------------------------------------------------------------------------------------------------------
;Mejoras
;-------------------------------------------------------------------------------------------------------------------------

;Operacion que suma los elementos de dos listas del mismo tamaño como la suma de dos vectores
(define sumar2listas (lambda (l1)
                       (lambda (l2)
                         (((null l1) (lambda (no_use) nil) (lambda (no_use) ((sumar2listasaux l1) l2))) zero))))                        ;si listas no vacias se llama a sumar2listasaux

;operacion auxilir de sumar2listas para simular recursividad
(define sumar2listasaux (lambda (l1)
                          (lambda (l2)
                            ((Y (lambda (f)
                                  (lambda (x)
                                    ((
                                      (null x)      											;si la lista es vacia se termina devolviendo nil	        
                                      (lambda (no_use)
                                        nil
                                        )
                                      (lambda (no_use)
                                        ((concatenar ((const ((sument (hd x)) (hd l2))) nil)) ((sumar2listas (tl x)) (tl l2)))         ;si no es vacia se suma las cabezas de ambas y se concatenan con siguiente ejecucion pasando el resto de ambas listas
                                        )
                                      )
                                     cero)
                                    )
                                  ))
                             l1)
                            )))

;-------------------------------------------------------------------------------------------------------------------------
;Operacion que devuelve el elemento en la posición pasada
(define buscarelem (lambda (l)
                         (lambda (p)
                           (((((esmenorent p) cero) or (null l)) (lambda (no_use) -uno) (lambda (no_use) ((buscarelemaux l)p))) zero))))  ;si la posicion es menor que cero o la lista es vacia se termina devolviendo -1 que indica error, si no se llama a buscarelemaux

;operacion auxiliar para simular recursividad de buscarelem
(define buscarelemaux (lambda (l)
                        (lambda (p)                       
                          ((Y (lambda (f)
                                (lambda (x)
                                  ((
                                    ((esmayorent p) cero)                                                                                 ;si la posicion es mayor que cero todavia no la hemos encontrado por lo que se sigue recorriendo la lista llamando a buscarelem con el resto de la lista y la posicion disminuida   						
                                    (lambda (no_use)
                                      ((buscarelem (tl x)) ((restaent p) uno))							
                                      )
                                    (lambda (no_use)
                                      (hd x)					                                                          ;si no es mayor es que se ha encontrado la posicion por lo que se devuelve la cabeza de la lista la cual corresponde al elemento buscado			
                                      )
                                    )
                                   zero)
                                  )
                                ))
                           l)
                          )))


;-------------------------------------------------------------------------------------------------------------------------
;Operacion que devuelve los primeros elementos hasta el número indicado (p)
(define obtenerinicio (lambda (l)
                         (lambda (p)
                           (((((esmenorent p) cero) or (null l)) (lambda (no_use) nil) (lambda (no_use) ((obtenerinicioaux l) p))) zero))))           ;si la posicion es menor que cero o la lista es vacia se termina devolviendo nil, si no se llama a obtenerinicioaux

(define obtenerinicioaux (lambda (l)
                           (lambda (p)
                             ((Y (lambda (f)
                                   (lambda (x)
                                     ((
                                       ((esmayorent p) cero)              						
                                       (lambda (no_use)
                                         ((concatenar ((const (hd l)) nil)) ((obtenerinicio (tl l)) ((restaent p) uno)))	                      ;si la posicion es mayor que cero todavia no la hemos encontrado por lo que se sigue recorriendo la lista concatenando los elementos para guardarlos y llamando a obtenerinicio con el resto de la lista						
                                         )
                                       (lambda (no_use)
                                         nil                                                                                                          ;si se ha encontrado la posicion se devuelve lista vacia ya que ya tenemos todos los elementos que queriamos
                                         )
                                       )
                                      zero)
                                     )
                                   ))
                              l)
                             )))

;-------------------------------------------------------------------------------------------------------------------------
;Operacion que devuelve los elementos de la lista que vienen despues de la posicion indicada
(define obtenerfinal (lambda (l)
                         (lambda (p)
                           (((((esmenorent p) cero) or (null l)) (lambda (no_use) nil) (lambda (no_use) ((obtenerfinalaux l) p))) zero))))          ;si la posicion es menor que cero o la lista es vacia se termina devolviendo nil, si no se llama a obtenerfinalaux

(define obtenerfinalaux (lambda (l)
                           (lambda (p)
                             ((Y (lambda (f)
                                   (lambda (x)
                                     ((
                                       ((esmayorent p) cero)              						
                                       (lambda (no_use)
                                         ((obtenerfinal (tl x)) ((restaent p) uno))	                                                            ;si la posicion es mayor que cero todavia no la hemos encontrado por lo que se sigue recorriendo la lista sin guaradar los elementos (solo queremos los que esten despues) y llamando a obtenerfinal con el resto de la lista										
                                         )
                                       (lambda (no_use)
                                         x                                                                                                          ;si se ha encontrado la posicion se devuelve la lista restante que corresponde a los elementos despues de la posicion
                                         )
                                       )
                                      zero)
                                     )
                                   ))
                              l)
                             )))

;-------------------------------------------------------------------------------------------------------------------------
;Operacion que elimina de la lista todas las apariciones del elemento pasado
(define eliminarelem (lambda (l)
                         (lambda (p)
                           (((null l) (lambda (no_use) nil) (lambda (no_use) ((eliminarelemaux l) p))) zero))))                                    ;se comprueba si la lista esta vacia en cuyo caso se devuelve nil y si no llama a elimarelemaux

(define eliminarelemaux (lambda (l)
                           (lambda (p)
                             ((Y (lambda (f)
                                   (lambda (x)
                                     ((
                                       ((esigualent p) (hd x))                                                                                    ;si el elemento pasado es igual a la cabeza de la lista no se guarda elemento y se hace llama a eliminarelem con el restod e esta lista (elimina elemento al no guardarlo)  						
                                       (lambda (no_use)
                                         ((eliminarelem (tl x)) p)							
                                         )
                                       (lambda (no_use)
                                         ((concatenar ((const(hd x)) nil)) ((eliminarelem (tl x)) p))                                             ;si no es igual si que se guarda la cabeza de la lista para no borrarlo haciendo tambien llamada a eliminarelem con el resto de la lista
                                         )
                                       )
                                      zero)
                                     )
                                   ))
                              l)
                             )))

;-------------------------------------------------------------------------------------------------------------------------
;Operacion que elimina elemento de la lista que se encuentra en la posición pasada
(define eliminarpos (lambda (l)
                         (lambda (p)
                           (((((esmenorent p) cero) or (null l)) (lambda (no_use) nil) (lambda (no_use) ((eliminarposaux l) p))) zero))))    ;si la posicion es menor que cero o la lista es vacia se termina devolviendo nil, si no se llama a eliminarposaux

(define eliminarposaux (lambda (l)
                           (lambda (p)
                             ((Y (lambda (f)
                                   (lambda (x)
                                     ((
                                       ((esmayorent p) cero)              		                                                      ;si la posicion es mayor que cero no la hemos encontrado todavia por lo que guardamos la cabeza actual y se llama a eliminarpos con el resto de la lista y devolviendo la posicion				
                                       (lambda (no_use)
                                         ((concatenar ((const(hd x)) nil)) ((eliminarpos (tl x)) ((restaent p) uno)))	
                                         )
                                       (lambda (no_use)
                                         (tl x)                                                                                               ;si se encuentra la posicion se devuelve el resto de la lista borrando asi el elemento de esa posicion
                                         )
                                       )
                                      zero)
                                     )
                                   ))
                              l)
                             )))


;-------------------------------------------------------------------------------------------------------------------------
;Operaciones que ordenan una lista de menor a mayor
;-------------------------------------------------------------------------------------------------------------------------
;Operacion que recorre elemento y se lo pasa a una lista vacia (l2) y a colocar el cual lo coloca en la posicion que corresponde
(define ordenacion (lambda (l1)
                         (lambda (l2)
                           (((null l1) (lambda (no_use) l2) (lambda (no_use) ((ordenacionaux l1) l2))) zero))))    ;si la lista es vacia se termina devolviendo la ordenada y si no se llama a ordenacionaux

;Operacion que permite a ordenacion simular la recursividad
(define ordenacionaux (lambda (l1)
                           (lambda (l2)
                             ((Y (lambda (f)
                                   (lambda (x)
                                     ((
                                       (null l2)              		                                                      ;si la lista ordenadada (l2) esta vacia se llama a ordenacion con el resto de la lista entrante y se introduce a la lista ordenada (l2)				
                                       (lambda (no_use)
                                         ((ordenacion (tl x)) ((const(hd l1)) l2))	
                                         )
                                       (lambda (no_use)
                                         ((ordenacion (tl x)) ((colocar l2) (hd x)))                                          ;si la lista ordenada no esta vacia se llama a ordenacion con el resto de la lista original y a colocar para introducir la cabeza de la lista original en la nueva lista                                                                                      
                                         )
                                       )
                                      zero)
                                     )
                                   ))
                              l1)
                             )))


;-------------------------------------------------------------------------------------------------------------------------
;Operacion que coloca elemento dado en la lista dada
(define colocar (lambda (l)
                         (lambda (p)
                           (((null l) (lambda (no_use) ((const p) nil)) (lambda (no_use) ((colocaraux l) p))) zero))))    ;si la lista es vacia se introduce directamente el elemento y si no llama a colocaraux

;operacion que permite a colocar simular la recursividad
(define colocaraux (lambda (l)
                           (lambda (p)
                             ((Y (lambda (f)
                                   (lambda (x)
                                     ((
                                       ((esmayorent p) (hd x))              		                                   ;compara si el elemento es mayor que la cabeza actual y si lo es concatena la cabeza (porque es mayor e ira despues) y llama a concatenar con el resto de la lista ordenando asi de menor a mayor				
                                       (lambda (no_use)
                                         ((concatenar ((const(hd x)) nil)) ((colocar (tl x)) p) )	
                                         )
                                       (lambda (no_use)
                                         ((concatenar ((const p) nil)) x)                                                  ;si el elemento es menor se llama a concatenar con el resto de la lista introduciendo el elemento al principio porque es mayor                                              
                                         )
                                       )
                                      zero)
                                     )
                                   ))
                              l)
                             )))

;-------------------------------------------------------------------------------------------------------------------------
;Listas de ejemplos
;-------------------------------------------------------------------------------------------------------------------------
(define prueba ((const dos) nil))
(define lista1 ((const cinco) ((const tres) ((const dos) nil))) )
(define lista2 ((const siete) ((const dos) ((const cinco) ((const uno) ((const dos) nil))))) )
(define lista3 ((const dos) ((const dos) ((const cuatro) ((const cinco) ((const cero) nil))))) )


(define (test)
(display "-----------------------------------")
(display "\n\t- Test de funciones -")
(display "\nDatos de prueba:")
(display "\nLista1 = ( ") (mostrar lista1)
(display ")\nLista2 = ( ") (mostrar lista2)
(display ")\nLista3 = ( ") (mostrar lista3)
(display ")\n-----------------------------------")
(display "\n\n- Concatenar lista1 - lista2\n( ")
(mostrar ((concatenar lista1) lista2))
(display ")\n\n- Longitud lista1\n")
(display (comprobar (longitud lista1)))
(display "\n\n- Inversion lista1\n( ")
(mostrar (inversion lista1))
(display ")\n\n- Pertenencia lista1 - cinco\n")
(display ((pertenece lista1) cinco))
(display "\n\n- Sumar elementos de lista1\n")
(display (testenteros (sumarlista lista1)))
(display "\n\n- Maximo lista1\n")
(display (testenteros ((maxlista lista1) (hd lista1))))
(display "\n\n- Minimo lista1\n")
(display (testenteros ((minlista lista1) (hd lista1))))
(display "\n\n- Sumar lista2 - lista3\n( ")
(mostrar ((sumar2listas lista2) lista3))
(display ")\n\n- Obtener elemento de posición - lista2 - posicion 2\n ")
(display (testenteros ((buscarelem lista2) dos)))
(display "\n\n- Devolver elementos antes de - lista2 - posicion 3\n( ")
(mostrar ((obtenerinicio lista2) tres))
(display ")\n\n- Devolver elementos sin los primeros de - lista2 - posicion 2\n( ")
(mostrar ((obtenerfinal lista2) dos))
(display ")\n\n- Eliminar elemento de la posicion - lista2 - posicion 2\n( ")
(mostrar ((eliminarpos lista2) dos))
(display ")\n\n- Eliminar elemento - lista3 - elemento 2\n( ")
(mostrar ((eliminarelem lista3) dos))
(display ")\n\n- Ordenar lista2\n( ")
(mostrar ((ordenacion lista2) nil))
(display ")\n-----------------------------------")
)

;----------------------------------------------------------------------------------

(define frame (new frame% [label "Interfaz de usuario"][width 500] [height 500])) ;Crea la interfaz

(define msg (new message% [parent frame]
                          [label "Elige una de las opciones de listas de ejemplo"])) ;Msg

(define aliner (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en el centro los rb
(define rb (new radio-box%  ;Selector de listas
              [parent aliner]
              [label "Selecciona Lista1:  "]
              [choices '("532" "72512" "22450")]
              [selection #f]
              [min-width 150]
              [min-height 30]
              [vert-margin 10]
              [horiz-margin 10]
              [stretchable-width #t]
              [stretchable-height #f]
              [callback (lambda (rb event)
                          (cond
                            [(= (send rb get-selection) 0)
                             (begin
                               (mostrar lista1)
                               (display "\n"))]

                            [(= (send rb get-selection) 1)
                             (begin
                               (mostrar lista2)
                               (display "\n"))]
                            [(= (send rb get-selection) 2)
                             (begin
                               (mostrar lista3)
                               (display "\n"))]
))]))

(define rb2 (new radio-box%  ;Selector de listas
              [parent aliner]
              [label "Selecciona Lista2:  "]
              [choices '("532" "72512" "22450")]
              [selection #f]
              [min-width 150]
              [min-height 30]
              [vert-margin 10]
              [horiz-margin 10]
              [stretchable-width #t]
              [stretchable-height #f]
              [callback (lambda (rb2 event)
                          (cond
                            [(= (send rb2 get-selection) 0)
                             (begin
                               (mostrar lista1)
                               (display "\n"))]

                            [(= (send rb2 get-selection) 1)
                             (begin
                               (mostrar lista2)
                               (display "\n"))]
                            [(= (send rb2 get-selection) 2)
                             (begin
                               (mostrar lista3)
                               (display "\n"))]
))]))

;-------------------- SUMA LISTA -------------------------
(define spacer (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer]  ;Boton para seleccionar la suma de una lista
     [label "Suma de la Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (send msg2 set-label (format "La suma es: ~a" (testenteros (sumarlista lista1))))]
                     [(= (send rb get-selection) 1) (send msg2 set-label (format "La suma es: ~a" (testenteros (sumarlista lista2))))]
                     [(= (send rb get-selection) 2) (send msg2 set-label (format "La suma es: ~a" (testenteros (sumarlista lista3))))])
                     ;[(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 2)) (send msg2 set-label (format "La suma es: ~a" (testenteros (sumarlista lista2))))]
 )])

(define msg2 (new message% [parent spacer]
                          [label "Resultado de la suma: "]))
;---------------------------------------------------------

;-------------------- MAXIMO LISTA -------------------------
(define spacer2 (new horizontal-panel% [parent frame][min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer2] ;Boton para seleccionar el maximo de una lista
     [label "Maximo de la Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (send msg3 set-label (format "Maximo: ~a" (testenteros ((maxlista lista1) (hd lista1)))))]
                     [(= (send rb get-selection) 1) (send msg3 set-label (format "Maximo: ~a" (testenteros ((maxlista lista2) (hd lista2)))))]
                     [(= (send rb get-selection) 2) (send msg3 set-label (format "Maximo: ~a" (testenteros ((maxlista lista3) (hd lista3)))))])
 )])

(define msg3 (new message% [parent spacer2] ; Msg
                          [label "Maximo de la lista: "]))
;---------------------------------------------------------

;-------------------- MINIMO LISTA -------------------------
(define spacer3 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer3] ;Boton para seleccionar el minimo de una lista
     [label "Minimo de la Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (send msg4 set-label (format "Minimo: ~a" (testenteros ((minlista lista1) (hd lista1)))))]
                     [(= (send rb get-selection) 1) (send msg4 set-label (format "Minimo: ~a" (testenteros ((minlista lista2) (hd lista2)))))]
                     [(= (send rb get-selection) 2) (send msg4 set-label (format "Minimo: ~a" (testenteros ((minlista lista3) (hd lista3)))))])
 )])

(define msg4 (new message% [parent spacer3]; Msg
                          [label "Minimo de la lista: "]))
;---------------------------------------------------------

;-------------------- LONGITUD LISTA -------------------------
(define spacer4 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer4] ;Boton para seleccionar la longitud de una lista
     [label "Longitud de la Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (send msg5 set-label (format "Longitud: ~a" (comprobar (longitud lista1))))]
                     [(= (send rb get-selection) 1) (send msg5 set-label (format "Longitud: ~a" (comprobar (longitud lista2))))]
                     [(= (send rb get-selection) 2) (send msg5 set-label (format "Longitud: ~a" (comprobar (longitud lista2))))])
 )])

(define msg5 (new message% [parent spacer4]; Msg
                          [label "Longitud de la Lista: "]))
;---------------------------------------------------------

;-------------------- PERTENECE LISTA -------------------------
(define spacer5 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(define c (new choice% [parent spacer5] ;Elector de numeros para comprobar pertenecencia
                       [label ""]
                       [choices '("uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve" "cero")]
                       [callback (lambda (c e)
                                   (displayln (send c get-string-selection)))]))
(new button% [parent spacer5] ;Boton para seleccionar si x pertenece a una lista
     [label "Pertenece Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (send msg6 set-label (format "R: ~a" ((pertenece lista1) (eval(string->symbol (send c get-string-selection))))) )]
                     [(= (send rb get-selection) 1) (send msg6 set-label (format "R: ~a" ((pertenece lista2) (eval(string->symbol (send c get-string-selection))))))]
                     [(= (send rb get-selection) 2) (send msg6 set-label (format "R: ~a" ((pertenece lista3) (eval(string->symbol (send c get-string-selection))))))])
 )])

(define msg6 (new message% [parent spacer5]; Msg
                          [label "Resultado pertenece: "]))
;---------------------------------------------------------

;-------------------- ELEMENTO LISTA -------------------------
(define spacer6 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(define c2 (new choice% [parent spacer6] ;Elector de numeros
                       [label ""]
                       [choices '("uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve" "cero")]
                       [callback (lambda (c2 e)
                                   (displayln (send c2 get-string-selection)))]))
(new button% [parent spacer6] ;Boton para seleccionar el elemento de la lista
     [label "Elemento Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (send msg7 set-label (format "Elemento: ~a" (testenteros((buscarelem lista1) (eval(string->symbol (send c2 get-string-selection)))))))]
                     [(= (send rb get-selection) 1) (send msg7 set-label (format "Elemento: ~a" (testenteros((buscarelem lista2) (eval(string->symbol (send c2 get-string-selection)))))))]
                     [(= (send rb get-selection) 2) (send msg7 set-label (format "Elemento: ~a" (testenteros((buscarelem lista3) (eval(string->symbol (send c2 get-string-selection)))))))])
 )])

(define msg7 (new message% [parent spacer6]; Msg
                          [label "Elemento de la lista: "]))
;---------------------------------------------------------

;-------------------- AUX -------------------------
(define variable-vacia '())

(define (lista-a-texto lst)
  (cond
    [(empty? lst) ""]
    [(pair? lst) 
     (string-append 
      (lista-a-texto (car lst)) 
      " " 
      (lista-a-texto (cdr lst)) 
      )]
    [else (format "~a" lst)]))

;limpiar (set! variable-vacia '())
;reverse (set! variable-vacia (reverse variable-vacia))

;Operacion que muestra los elementos que contiene lista dada
(define mostrar2 (lambda (l)
                   (((null l) (lambda (no_use) (display "")) (lambda (no_use) (mostraraux2 l))) zero)))		;si la lista es vacia no muestra nada y termina, si no llama a mostraraux

;operacion auxiliar para recorrer lista de mostrar
(define mostraraux2 (lambda (l)
                   ((Y (lambda (f)
                         (display (testenteros(hd l))) (display " ") (set! variable-vacia (cons (testenteros(hd l)) variable-vacia)) 						;muestra cabeza de la lista por consola
                         (lambda (x)	
                           ((
                             (null x)              								;si la lista es vacia se termina
                             (lambda (no_use)
                               (display "")
                               )
                             (lambda (no_use)
                               (mostrar2 (tl x))									;si no es vacia se llama a mostrar con el resto de la lista continuando su recorrido
                               )
                             )
                            zero)
                           )
                         ))
                    l)
                   ))

;---------------------------------------------------------          	

;-------------------- CONCATENAR LISTAS -------------------------
(define spacer7 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer7] ;Boton para seleccionar la longitud de una lista
     [label "Concatenar listas"]
     [callback (lambda (button event)
                   (cond
                     [(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 0)) (begin (mostrar2 ((concatenar lista1) lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 1)) (begin (mostrar2 ((concatenar lista1) lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 2)) (begin (mostrar2 ((concatenar lista1) lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     
                     [(and (= (send rb get-selection) 1) (= (send rb2 get-selection) 0)) (begin (mostrar2 ((concatenar lista2) lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 1) (= (send rb2 get-selection) 1)) (begin (mostrar2 ((concatenar lista2) lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 1) (= (send rb2 get-selection) 2)) (begin (mostrar2 ((concatenar lista2) lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     
                     [(and (= (send rb get-selection) 2) (= (send rb2 get-selection) 0)) (begin (mostrar2 ((concatenar lista3) lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 2) (= (send rb2 get-selection) 1)) (begin (mostrar2 ((concatenar lista3) lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 2) (= (send rb2 get-selection) 2)) (begin (mostrar2 ((concatenar lista3) lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg8 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]                   
                     )
 )])

(define msg8 (new message% [parent spacer7]; Msg
                          [label "Lista Resultante: "]))

;---------------------------------------------------------

;-------------------- REVERSE LISTA -------------------------
(define spacer8 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer8] ;Boton para seleccionar reverse list
     [label "Reverse lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (begin (mostrar2 (inversion lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg9 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(= (send rb get-selection) 1) (begin (mostrar2 (inversion lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg9 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(= (send rb get-selection) 2) (begin (mostrar2 (inversion lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg9 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
 ))])

(define msg9 (new message% [parent spacer8]; Msg
                          [label "Resultado lista reverse: "]))
;---------------------------------------------------------

;-------------------- SUMAR LISTAS -------------------------
(define spacer9 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer9] ;Boton para seleccionar sumar listas
     [label "Sumar Listas"]
     [callback (lambda (button event)
                   (cond
                     [(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 0)) (begin (mostrar2 ((sumar2listas lista1) lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 1)) (begin (mostrar2 ((sumar2listas lista1) lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 0) (= (send rb2 get-selection) 2)) (begin (mostrar2 ((sumar2listas lista1) lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     
                     [(and (= (send rb get-selection) 1) (= (send rb2 get-selection) 0)) (begin (mostrar2 ((sumar2listas lista2) lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 1) (= (send rb2 get-selection) 1)) (begin (mostrar2 ((sumar2listas lista2) lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 1) (= (send rb2 get-selection) 2)) (begin (mostrar2 ((sumar2listas lista2) lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     
                     [(and (= (send rb get-selection) 2) (= (send rb2 get-selection) 0)) (begin (mostrar2 ((sumar2listas lista3) lista1)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 2) (= (send rb2 get-selection) 1)) (begin (mostrar2 ((sumar2listas lista3) lista2)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(and (= (send rb get-selection) 2) (= (send rb2 get-selection) 2)) (begin (mostrar2 ((sumar2listas lista3) lista3)) (set! variable-vacia (reverse variable-vacia)) (send msg10 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]                   
))])

(define msg10 (new message% [parent spacer9]; Msg
                          [label "Resultado sumar listas: "]))
;---------------------------------------------------------

;-------------------- INICIO LISTA -------------------------
(define spacer10 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(define c3 (new choice% [parent spacer10] ;Elector de numeros para obtener comienzo
                       [label ""]
                       [choices '("uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve" "cero")]
                       [callback (lambda (c3 e)
                                   (displayln (send c3 get-string-selection)))]))
(new button% [parent spacer10] ;Boton para seleccionar funcion
     [label "Obtener Iniciales Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (begin (mostrar2 ((obtenerinicio lista1) (eval(string->symbol (send c3 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg11 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 1) (begin (mostrar2 ((obtenerinicio lista2) (eval(string->symbol (send c3 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg11 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 2) (begin (mostrar2 ((obtenerinicio lista3) (eval(string->symbol (send c3 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg11 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ])
 )])

(define msg11 (new message% [parent spacer10]; Msg
                          [label "Obtener iniciales: "]))
;---------------------------------------------------------

;-------------------- ELIMINAR INICIO LISTA -------------------------
(define spacer11 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(define c4 (new choice% [parent spacer11] ;Elector de numeros para quitar inicio
                       [label ""]
                       [choices '("uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve" "cero")]
                       [callback (lambda (c4 e)
                                   (displayln (send c4 get-string-selection)))]))
(new button% [parent spacer11] ;Boton para seleccionar funcion
     [label "Quitar Iniciales Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (begin (mostrar2 ((obtenerfinal lista1) (eval(string->symbol (send c4 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg12 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 1) (begin (mostrar2 ((obtenerfinal lista2) (eval(string->symbol (send c4 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg12 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 2) (begin (mostrar2 ((obtenerfinal lista3) (eval(string->symbol (send c4 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg12 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ])
 )])

(define msg12 (new message% [parent spacer11]; Msg
                          [label "Quitar iniciales: "]))
;---------------------------------------------------------

;-------------------- ELIMINAR ELEMENTO POSICION LISTA -------------------------
(define spacer12 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(define c5 (new choice% [parent spacer12] ;Elector de numeros para quitar inicio
                       [label ""]
                       [choices '("uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve" "cero")]
                       [callback (lambda (c5 e)
                                   (displayln (send c5 get-string-selection)))]))
(new button% [parent spacer12] ;Boton para seleccionar funcion
     [label "Quitar Indice Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (begin (mostrar2 ((eliminarpos lista1) (eval(string->symbol (send c5 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg13 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 1) (begin (mostrar2 ((eliminarpos lista2) (eval(string->symbol (send c5 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg13 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 2) (begin (mostrar2 ((eliminarpos lista3) (eval(string->symbol (send c5 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg13 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ])
 )])

(define msg13 (new message% [parent spacer12]; Msg
                          [label "Quitar indice: "]))
;---------------------------------------------------------

;-------------------- ELIMINAR ELEMENTO LISTA -------------------------
(define spacer13 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(define c6 (new choice% [parent spacer13] ;Elector de numeros para quitar inicio
                       [label ""]
                       [choices '("uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve" "cero")]
                       [callback (lambda (c6 e)
                                   (displayln (send c6 get-string-selection)))]))
(new button% [parent spacer13] ;Boton para seleccionar funcion
     [label "Quitar Elemento Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (begin (mostrar2 ((eliminarelem lista1) (eval(string->symbol (send c6 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg14 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 1) (begin (mostrar2 ((eliminarelem lista2) (eval(string->symbol (send c6 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg14 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ]
                     [(= (send rb get-selection) 2) (begin (mostrar2 ((eliminarelem lista3) (eval(string->symbol (send c6 get-string-selection))))) (set! variable-vacia (reverse variable-vacia)) (send msg14 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '()) ) ])
 )])

(define msg14 (new message% [parent spacer13]; Msg
                          [label "Quitar elemento: "]))
;---------------------------------------------------------

;-------------------- ORDENAR LISTA -------------------------
(define spacer14 (new horizontal-panel% [parent frame] [min-width 10][alignment '(center center)])) ;Parent para poner en horizontal el boton y msg
(new button% [parent spacer14] ;Boton para seleccionar ordenar lista
     [label "Ordenar Lista1"]
     [callback (lambda (button event)
                   (cond
                     [(= (send rb get-selection) 0) (begin (mostrar2 ((ordenacion lista1) nil)) (set! variable-vacia (reverse variable-vacia)) (send msg15 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(= (send rb get-selection) 1) (begin (mostrar2 ((ordenacion lista2) nil)) (set! variable-vacia (reverse variable-vacia)) (send msg15 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
                     [(= (send rb get-selection) 2) (begin (mostrar2 ((ordenacion lista3) nil)) (set! variable-vacia (reverse variable-vacia)) (send msg15 set-label (lista-a-texto variable-vacia)) (set! variable-vacia '())  )]
))])

(define msg15 (new message% [parent spacer14]; Msg
                          [label "Lista Ordenada: "]))
;---------------------------------------------------------



;-------------------- FINAL -------------------------
(send frame show #t) ; Manda la interfaz
;---------------------------------------------------------