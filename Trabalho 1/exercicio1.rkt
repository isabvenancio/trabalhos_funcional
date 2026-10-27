#lang racket
(require rackunit)

;; função arredonda
;; faz o arredondamento de um número para o inteiro mais próximo
;; (expt 10 n) é calculado e multiplicado por x para deslocar a vírgula 
;; round para remover as casas decimais
;; o resultado é dividido por (expt 10 n) para retornar o valor arredondado

(define (arredonda x n)
    (/ (round (* x (expt 10 n)))(expt 10 n))) ;; round(((10 expt n) * x)) / (10 expt n)


;; testes
(check-equal? (arredonda 3.14159265359 1) 3.1)
(check-equal? (arredonda 3.14159265359 2) 3.14)
(check-equal? (arredonda 3.14159265359 3) 3.142)
(check-equal? (arredonda 3.14159265359 4) 3.1416)
(check-equal? (arredonda 2.71828 0) 3.0)
(check-equal? (arredonda -3.14159 2) -3.14)
(check-equal? (arredonda 2.55 1) 2.6)
(check-equal? (arredonda 10.0 2) 10.0) ;; ou é 10.0 ou 10 nos dois
(check-equal? (arredonda 2.55 1) 2.6)
(check-equal? (arredonda 2.3 0) 2.0)