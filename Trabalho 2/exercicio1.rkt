#lang racket
(require rackunit)

;;Struct "nó" com valor, subárvore da esquerda e subárvore da direita
(struct no (valor esq dir) #:transparent)

;; a) Inserir nó
(define (inserirNo arv valor)
  (cond
    [(empty? arv) (no valor '() '())]
    [(= valor (no-valor arv)) arv]
    [(< valor (no-valor arv)) (no (no-valor arv) (inserirNo (no-esq arv) valor) (no-dir arv))]
    [else (no (no-valor arv) (no-esq arv) (inserirNo (no-dir arv) valor))]))

;; c) Buscar nó
(define (buscaNo arv valor)
  (cond
    [(empty? arv) #f]
    [(= valor (no-valor arv) arv)]
    [(< valor (no-valor arv) (buscarNo (no-esq arv) valor))]
    [else (buscarNo (no-dir arv) valor)]))
    