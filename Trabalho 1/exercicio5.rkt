#lang racket
(require rackunit)

;fila
(struct fila (valor proximo) #:transparent)

;consultar_primeiro
(define (consultar_primeiro f)
    (cond
        [(fila? f) (fila-valor f)]
        [else "A fila está vazia"]))

;enfileirar
(define (enfileirar f valor)
    (cond
        [(fila? f) (fila (fila-valor f)(enfileirar (fila-proximo f) valor))]
        [else (fila valor #f)]))

;desenfileirar
(define (desenfileirar f)
    (cond
        [(fila? f) (fila-proximo f)]
        [else #f]))

; Testes
(define f0 #f) ;fila vazia
(define f1 (enfileirar f0 10))
(define f2 (enfileirar f1 20))
(define f3 (enfileirar f2 30))
; '(10 (20 (30)))

(check-equal? (consultar_primeiro f3) 10)

(define f4 (enfileirar f3 40))
(check-equal? (consultar_primeiro f4) 10)

(check-equal? (consultar_primeiro (desenfileirar f4)) 20)

(check-equal? (consultar_primeiro f0) "A fila está vazia")
(check-equal? (desenfileirar f0) #f)