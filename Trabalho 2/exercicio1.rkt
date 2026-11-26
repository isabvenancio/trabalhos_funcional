#lang racket
(require rackunit)

;; Uma ABB é
;; - empty (árvore vazia); ou
;; - (no Número ABB ABB), onde:
;;  valor: a chave armazenada no nó;
;;  esq: ABB cujas chaves são todas MENORES que valor;
;;  dir: ABB cujas chaves são todas MAIORES que valor
;; Não existem chaves repetidas
;; Struct "nó" com valor, subárvore da esquerda e subárvore da direita
(struct no (valor esq dir) #:transparent)

;; a) Inserir nó
;; ABB Número -> ABB
;; Devolve uma nova ABB com a chave valor inserida em arv
;; Se valor já existe na árvore, devolve a árvore sem alterações
;; (não há chaves repetidas)
(define (insere-no arv valor)
    (cond
        [(empty? arv) (no valor empty empty)]
        [(= valor (no-valor arv)) arv]
        [(< valor (no-valor arv)) (no (no-valor arv) (insere-no (no-esq arv) valor) (no-dir arv))]
        [else (no (no-valor arv) (no-esq arv) (insere-no (no-dir arv) valor))]))

;; b) Remover nó
;; ABB -> Número
;; Devolve a menor chave de uma ABB não vazia (a chave do nó mais à esquerda
; requer que arv não seja vazia
(define (minimo arv)
    (cond
        [(empty? (no-esq arv)) (no-valor arv)]
        [else (minimo (no-esq arv))]))
        
;; ABB Número -> ABB
;; Devolve uma nova ABB sem o nó de chave x Se x não está na árvore,
;; devolve a árvore sem alterações Considera os casos:
;; - nó folha: é simplesmente removido;
;; - nó com um único filho: é substituído por esse filho;
;; - nó com dois filhos: sua chave é substituída pela do sucessor (menor chave da subárvore direita) e o sucessor é removido da subárvore direita
(define (remove-no arv x)
  (cond
    [(empty? arv) empty]
    [(< x (no-valor arv)) (no (no-valor arv) (remove-no (no-esq arv) x) (no-dir arv))]
    [(> x (no-valor arv)) (no (no-valor arv) (no-esq arv) (remove-no (no-dir arv) x))]
    [(and (empty? (no-esq arv)) (empty? (no-dir arv))) empty]
    [(empty? (no-esq arv)) (no-dir arv)]
    [(empty? (no-dir arv)) (no-esq arv)]
    [else
        (define sucessor (minimo (no-dir arv)))
        (no sucessor (no-esq arv) (remove-no (no-dir arv) sucessor))]))

;; c) Buscar nó
;; ABB Número -> Boolean
;; Devolve #t se existe um nó com chave valor em arv, #f caso contrário
;; propriedade da ABB: compara com a raiz e continua a busca
;; somente na subárvore esquerda (se menor) ou direita (se maior)
(define (busca-no arv valor)
    (cond
        [(empty? arv) #f]
        [(< valor (no-valor arv)) (busca-no (no-esq arv) valor)]
        [(> valor (no-valor arv)) (busca-no (no-dir arv) valor)]
        [else #t]))


;testes
(define a1 (insere-no empty 8))
(define a2 (insere-no a1 3))
(define a3 (insere-no a2 10))
(define a4 (insere-no a3 1))
(define a5 (insere-no a4 6))
(define abb (insere-no a5 14))

(check-equal? (remove-no empty 5) empty)
(check-equal? (remove-no abb 7) abb)
(check-equal? (busca-no abb 6) #t)
(check-equal? (busca-no abb 14) #t)
(check-equal? (busca-no abb 7) #f)
(check-equal? (busca-no empty 10) #f)

(define abb-sem-1 (remove-no abb 1))
(check-equal? (busca-no abb-sem-1 1) #f)
(check-equal? (busca-no abb-sem-1 3) #t)

(define abb-sem-10 (remove-no abb 10))
(check-equal? (busca-no abb-sem-10 10) #f)
(check-equal? (busca-no abb-sem-10 14) #t)

(define abb-sem-8 (remove-no abb 8))
(check-equal? (busca-no abb-sem-8 8) #f)
(check-equal? (busca-no abb-sem-8 6) #t)
(check-equal? (busca-no abb-sem-8 10) #t)