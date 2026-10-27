#lang racket
(require rackunit)

;funcao scan
;devolve uma lista S em q o i-ésimo elemento é f aplicada a ini e aos elementos das posições até i de lst, acumulando da esquerda para a direita
; S1 = (f ini x1), S2 = (f S1 x2), S3 = (f S2 x3)
; cada resultado parcial ("novo") é calculado uma única vez e passado como o novo valor inicial da chamada recursiva. Assim, em 
; (scan + 2 (list 9 3 1 7)) a soma 2 + 9 acontece só uma vez, e as somas seguintes partem de 11, 14 e 15. Ao todo são feitas n aplicações de f para uma lista de n elementos.
(define (scan f ini lst)
    (cond
        [(empty? lst) empty]
        [else
            (define novo (f ini (first lst)))
            (cons novo (scan f novo (rest lst)))]
    )
)

;testes
(check-equal? (scan + 0 (list)) '())
(check-equal? (scan + 2 (list 9 3 1 7)) '(11 14 15 22))
(check-equal? (scan * 1 (list 2 3 5)) '(2 6 30))
(check-equal? (scan append (list) (list (list 1 2) (list 3) (list 4 5)))'((1 2) (1 2 3) (1 2 3 4 5)))
(check-equal? (scan append (list 0) (list (list 1 2) (list 3) (list 4 5)))'((0 1 2) (0 1 2 3) (0 1 2 3 4 5)))
(check-equal? (scan + 0 (list 5)) '(5))
(check-equal? (scan - 10 (list 1 2 3)) '(9 7 4))
(check-equal? (scan max 0 (list 3 1 5 2)) '(3 3 5 5))
(check-equal? (scan string-append "" (list "a" "b" "c")) '("a" "ab" "abc"))
(check-equal? (scan (λ (acc x) (cons x acc)) empty (list 1 2 3))'((1) (2 1) (3 2 1)))