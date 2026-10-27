#lang racket
(require rackunit)

;pilha
(struct pilha (valor anterior) #:transparent)

;consultar_topo
(define (consultar_topo p)
  (cond
    [(pilha? p) (pilha-valor p)]
    [else "A pilha está vazia"]))
  
;Empilhar
(define (empilhar p valor)
  (pilha valor p))

;Desempilhar
(define (desempilhar p)
  (cond
    [(pilha? p) (pilha-anterior p)]
    [else #f]))

; Testes
(define p0 #f) ;pilha vazia
(define p1 (empilhar p0 5))
(define p2 (empilhar p1 4))
(define p3 (empilhar p2 3))
(define p4 (empilhar p3 2))
(define p5 (empilhar p4 1))
; '(1 . (2 . (3 . (4 . (5 . #f)))))

(check-equal? (consultar_topo p5) 1)

(define p6 (empilhar p5 0))
(check-equal? (consultar_topo p6) 0)

(check-equal? (consultar_topo (desempilhar p6)) 1)

(check-equal? (consultar_topo p0) "A pilha está vazia")
(check-equal? (desempilhar p0) #f)