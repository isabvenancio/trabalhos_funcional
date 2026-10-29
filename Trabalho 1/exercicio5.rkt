#lang racket
(require rackunit)

;fila
;#f, que representa a fila vazia ou (fila valor proximo), onde valor é o primeiro elemento da fila e proximo é o restante da fila (outra fila).
(struct fila (valor proximo) #:transparent)

;consultar_primeiro
;; Devolve o primeiro elemento da fila f (o próximo a sair), sem alterá-la e se a fila estiver vazia, devolve a mensagem "A fila está vazia".
(define (consultar_primeiro f)
    (cond
        [(fila? f) (fila-valor f)]
        [else "A fila está vazia"]))

;enfileirar
;Devolve uma nova fila com valor inserido no final da fila f.
;;mantém o primeiro elemento e enfileira valor no restante da fila, até chegar à fila vazia, onde cria o último nó.
;; A fila f original não é alterada
(define (enfileirar f valor)
    (cond
        [(fila? f) (fila (fila-valor f)(enfileirar (fila-proximo f) valor))]
        [else (fila valor #f)]))

;desenfileirar
;Devolve a fila sem o primeiro elemento (o restante da fila f). A fila f original não é alterada. Se f estiver vazia, devolve #f (a própria fila vazia).
(define (desenfileirar f)
    (cond
        [(fila? f) (fila-proximo f)]
        [else #f]))

; Testes
(define f0 #f) ;fila vazia
(define f1 (enfileirar f0 10))
(define f2 (enfileirar f1 20))
(define f3 (enfileirar f2 30))
; (fila 10 (fila 20 (fila 30 #f)))

(check-equal? (consultar_primeiro f3) 10)

(define f4 (enfileirar f3 40))
(check-equal? (consultar_primeiro f4) 10)

(check-equal? (consultar_primeiro (desenfileirar f4)) 20)

(check-equal? (consultar_primeiro f0) "A fila está vazia")
(check-equal? (desenfileirar f0) #f)

(check-equal? (enfileirar f0 7) (fila 7 #f))

(check-equal? f3 (fila 10 (fila 20 (fila 30 #f))))
(check-equal? f4 (fila 10 (fila 20 (fila 30 (fila 40 #f)))))

(check-equal? f3 (fila 10 (fila 20 (fila 30 #f))))

(check-equal? (consultar_primeiro (desenfileirar (desenfileirar f4))) 30)
(check-equal? (consultar_primeiro (desenfileirar (desenfileirar (desenfileirar f4)))) 40)

(check-equal? (desenfileirar f1) #f)
(check-equal? (consultar_primeiro (desenfileirar f1)) "A fila está vazia")

(check-equal? (enfileirar (desenfileirar f3) 50) (fila 20 (fila 30 (fila 50 #f))))