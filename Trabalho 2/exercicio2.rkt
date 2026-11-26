#lang racket
(require rackunit)

;o depósito de r reais é feito no fim de cada mes, e o saldo q já estava na aplicação rende p% naquele mes
; S(0) = 0
; S(t) = S(t-1) * (1 + p/100) + r

; funcao total-acumulado
;; Número Número Inteiro -> Número
;; Calcula o total acumulado após t meses, depositando r reais por mês em uma aplicação que rende p% ao mês (p dado em porcentagem, por exemplo 10 significa 10% ao mês). Se t <= 0, nenhum mês se passou e o total é 0.
(define (total-acumulado r p t)
    (cond
        [(<= t 0) 0]
        [else (+ (* (total-acumulado r p (sub1 t))(+ 1 (/ p 100))) r)]
    )
)

; funcao tempo
;; Número Número Número -> Natural
;; Devolve quantos meses são necessários para o total acumulado ultrapassar (>) m reais (atingir m exatamente não basta), depositando r reais por mês em uma aplicação que rende p% ao mês.
;; Requer p >= 0. 
;; Se r <= 0 e m >= 0 o total nunca passa de m, então a função sinaliza um erro em vez de entrar em recursão infinita
; S(t-1) > (m - r) / (1 + p/100)
(define (tempo-ultrapassar r p m)
    (cond
        [(< m 0) 0]
        [(<= r 0) (error "com depósito r <= 0 o total nunca ultrapassa m")]
        [else (add1 (tempo-ultrapassar r p (/ (- m r) (+ 1 (/ p 100)))))]
    )
)

;testes
(check-equal? (total-acumulado 100 100 0) 0) ;nenhum mes
(check-equal? (total-acumulado 100 0 5) 500) ;sem rendimento r*t
(check-equal? (total-acumulado 100 100 1) 100) ;0 * 2 + 100
(check-equal? (total-acumulado 100 100 3) 700) ;100 -> 300 -> 700
(check-equal? (total-acumulado 100 10 2) 210) ;100 * 1,1 + 100
(check-equal? (total-acumulado 50 20 2) 110) ;50 * 1,2 + 50
(check-equal? (total-acumulado 0 10 5) 0)
(check-equal? (total-acumulado 100 10 1) 100)
(check-equal? (total-acumulado 100 10 3) 331)    ; 210 * 1,1 + 100
(check-equal? (total-acumulado 50 20 2) 110)     ; 50 * 1,2 + 50
(check-equal? (total-acumulado 100 1 2) 201)     ; 100 * 1,01 + 100
(check-equal? (total-acumulado 200 50 2) 500)    ; 200 * 1,5 + 200
(check-equal? (total-acumulado 0 10 5) 0) 

(check-equal? (tempo-ultrapassar 100 10 -1) 0) ; 0 já é maior que -1
(check-equal? (tempo-ultrapassar 100 0 450) 5) ; 500 > 450
(check-equal? (tempo-ultrapassar 100 0 500) 6) ; precisa ultrapassar, não só atingir
(check-equal? (tempo-ultrapassar 100 100 650) 3) ; 100, 300, 700
(check-equal? (tempo-ultrapassar 100 100 700) 4) ; 700 não ultrapassa 700; 1500 sim
(check-equal? (tempo-ultrapassar 100 1 1000) 10) ; 9 meses: 936,85 e 10 meses: 1046,22
(check-equal? (tempo-ultrapassar 50 20 110) 3)     ; 50, 110, 182
(check-equal? (tempo-ultrapassar 200 50 500) 3)    ; 200, 500, 950
(check-equal? (tempo-ultrapassar 100 1 1000) 10)   ; 9 meses: 936,85; 10 meses: 1046,22
(check-equal? (tempo-ultrapassar 100 5 5000) 26)
(check-equal? (tempo-ultrapassar 100 100 650) 3)   ; 100, 300, 700
(check-equal? (tempo-ultrapassar 100 100 700) 4)   ; 700 não ultrapassa 700; 1500 sim