#lang racket
(require rackunit)

;; função natal_desconto
;; calcula o preco de venda de um jogo na promoção de natal, sendo q o preco final não pode ser mais caro que o da loja concorrente
;; calculamos o preço com desconto multiplicando o preço original por (1 - desconto/100). Ex: 20% de desconto é (1 - 20/100) = 0.80
;; função if verifica se o preço calculado é MAIOR que o preço da loja concorrente
;; Se for maior, a função retorna o preço do concorrente
;; Se for menor ou igual, retorna o preço com o desconto aplicado

(define (natal_desconto preco-original preco-concorrente desconto)
    (if (> (* preco-original (- 1 (/ desconto 100))) preco-concorrente) ;; dado um percentual de desconto, se o preco original com desconto for maior que o da concorrente, retorna o preco da concorrente
        preco-concorrente
        (* preco-original (- 1 (/ desconto 100))))) ;; senao, retorna o preco original com desconto


;; testes
(check-equal? (natal_desconto 100 90 20) 80) ;; o preço com desconto (80) é menor que o da concorrente (90), então retorna 80
(check-equal? (natal_desconto 100 75 20) 75) ;; o preço com desconto (80) é maior que o da concorrente (75), então retorna 75
(check-equal? (natal_desconto 100 80 20) 80) ;; o preço com desconto (80) é igual ao da concorrente (80), então retorna 80
(check-equal? (natal_desconto 250 200 10) 200) ;; o preço com desconto (225) é maior que o da concorrente (200), então retorna 200
(check-equal? (natal_desconto 150 100 50) 75) ;; o preço com desconto (75) é menor que o da concorrente (100), então retorna 75
