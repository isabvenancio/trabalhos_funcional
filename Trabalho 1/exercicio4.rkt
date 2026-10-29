#lang racket
(require rackunit)

;pilha
;;#f, que representa a pilha vazia ou (pilha valor anterior), onde valor é o elemento do topo e anterior é o restante da pilha (outra pilha).
(struct pilha (valor anterior) #:transparent)

;consultar_topo
;; Devolve o elemento que está no topo da pilha p, e se a pilha estiver vazia, devolve a mensagem "A pilha está vazia".
(define (consultar_topo p)
  (cond
    [(pilha? p) (pilha-valor p)]
    [else "A pilha está vazia"]))
  
;Empilhar
;;Devolve uma nova pilha com valor no topo e a pilha p embaixo dele e a pilha p original não é alterada.
(define (empilhar p valor)
  (pilha valor p))

;Desempilhar
;;Devolve a pilha sem o elemento do topo (o restante da pilha p).
;;A pilha p original não é alterada. Se p estiver vazia, devolve #f (a própria pilha vazia).
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
; (pilha 1 (pilha 2 (pilha 3 (pilha 4 (pilha 5 #f)))))

(check-equal? (consultar_topo p5) 1)

(define p6 (empilhar p5 0))
(check-equal? (consultar_topo p6) 0)

(check-equal? (consultar_topo (desempilhar p6)) 1)

(check-equal? (consultar_topo p0) "A pilha está vazia")
(check-equal? (desempilhar p0) #f)

(check-equal? (empilhar p0 7) (pilha 7 #f))
(check-equal? (consultar_topo (empilhar p0 7)) 7)

(check-equal? p3 (pilha 3 (pilha 4 (pilha 5 #f))))

(check-equal? (desempilhar (empilhar p3 9)) p3)

(check-equal? (consultar_topo (desempilhar p5)) 2)
(check-equal? (consultar_topo p5) 1)

(check-equal? (consultar_topo (desempilhar (desempilhar p5))) 3)
(check-equal? (consultar_topo (desempilhar (desempilhar (desempilhar (desempilhar p5))))) 5)

(check-equal? (desempilhar p1) #f)
(check-equal? (consultar_topo (desempilhar p1)) "A pilha está vazia")