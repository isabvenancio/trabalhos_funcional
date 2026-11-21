#lang racket
(require rackunit)

;; Uma ABB é
;; - empty (árvore vazia); ou
;; - (no Número ABB ABB), onde:
;;  valor: a chave armazenada no nó;
;;  esq: ABB cujas chaves são todas MENORES que valor;
;;  dir: ABB cujas chaves são todas MAIORES que valor.
;; Não existem chaves repetidas.
;; Struct "nó" com valor, subárvore da esquerda e subárvore da direita
(struct no (valor esq dir) #:transparent)

;; a) Inserir nó
;; ABB Número -> ABB
;; Devolve uma nova ABB com a chave valor inserida em arv.
;; Se valor já existe na árvore, devolve a árvore sem alterações
;; (não há chaves repetidas).
(define (insereNo arv valor)
    (cond
        [(empty? arv) (no valor '() '())]
        [(= valor (no-valor arv)) arv]
        [(< valor (no-valor arv)) (no (no-valor arv) (insereNo (no-esq arv) valor) (no-dir arv))]
        [else (no (no-valor arv) (no-esq arv) (insereNo (no-dir arv) valor))]))

;; b) Remover nó
;; ABB -> Número
;; Devolve a menor chave de uma ABB NÃO vazia (a chave do nó mais à
;; esquerda). Requer que arv não seja vazia.
(define (minimo arv)
    (cond
        [(empty? (no-esq arv)) (no-valor arv)]
        [else (minimo (no-esq arv))]))
        
;; ABB Número -> ABB
;; Devolve uma nova ABB sem o nó de chave x. Se x não está na árvore,
;; devolve a árvore sem alterações. Considera os casos:
;; - nó folha: é simplesmente removido;
;; - nó com um único filho: é substituído por esse filho;
;; - nó com dois filhos: sua chave é substituída pela do sucessor (menor chave da subárvore direita) e o sucessor é removido da subárvore direita.
(define (removeNo arv x)
  (cond
    [(empty? arv) '()]
    [(< x (no-valor arv)) (no (no-valor arv) (removeNo (no-esq arv) x) (no-dir arv))]
    [(> x (no-valor arv)) (no (no-valor arv) (no-esq arv) (removeNo (no-dir arv) x))]
    [else
        (cond
            [(and (empty? (no-esq arv)) (empty? (no-dir arv))) '()]
            [(empty? (no-esq arv)) (no-dir arv)]
            [(empty? (no-dir arv)) (no-esq arv)]
            [else
                (define sucessor (minimo (no-dir arv)))
                (no sucessor (no-esq arv) (removeNo (no-dir arv) sucessor))])]))

;; c) Buscar nó
;; ABB Número -> Boolean
;; Devolve #t se existe um nó com chave valor em arv, #f caso contrário.
;; Aproveita a propriedade da ABB: compara com a raiz e continua a busca
;; somente na subárvore esquerda (se menor) ou direita (se maior).
(define (buscaNo arv valor)
    (cond
        [(empty? arv) #f]
        [(= valor (no-valor arv)) #t]
        [(< valor (no-valor arv)) (buscaNo (no-esq arv) valor)]
        [else (buscaNo (no-dir arv) valor)]))


;testes
(define a1 (insereNo '() 8))
(define a2 (insereNo a1 3))
(define a3 (insereNo a2 10))
(define a4 (insereNo a3 1))
(define a5 (insereNo a4 6))
(define abb (insereNo a5 14))

(check-equal? (buscaNo abb 6) #t)
(check-equal? (buscaNo abb 14) #t)
(check-equal? (buscaNo abb 7) #f)
(check-equal? (buscaNo '() 10) #f)

(define abb-sem-1 (removeNo abb 1))
(check-equal? (buscaNo abb-sem-1 1) #f)
(check-equal? (buscaNo abb-sem-1 3) #t)

(define abb-sem-10 (removeNo abb 10))
(check-equal? (buscaNo abb-sem-10 10) #f)
(check-equal? (buscaNo abb-sem-10 14) #t)

(define abb-sem-8 (removeNo abb 8))
(check-equal? (buscaNo abb-sem-8 8) #f)
(check-equal? (buscaNo abb-sem-8 6) #t)
(check-equal? (buscaNo abb-sem-8 10) #t)