#lang racket
(require rackunit)

;o depósito de r reais é feito no fim de cada mes, e o saldo q já estava na aplicação rende p% naquele mes
; S(0) = 0
; S(t) = S(t-1) * (1 + p/100) + r

; funcao total-acumulado
; calcula o total acumulado apos t meses, depositando r reais por mes numa aplicação q rende %p ao mes
(define (total-acumulado r p t)
    (cond
        [(<= t 0) 0]
        [else (+ (* (total-acumulado r p (sub1 t))(+ 1 (/ p 100))) r)]
    )
)

; funcao tempo
; devolve quantos meses sao necessarios para o total acumulador ultrapassar m reais, depositando r por mes com rendimento de p%
; p >= 0. Se r <= 0 e m >= 0 o saldo nunca passa de m, então a função sinaliza um erro em vez de entrar em recursão infinita
(define (tempo r p m)
    ;funcao tempo-aux
    ; saldo é o total acumulado ate agora e meses são os já passados
    (define (tempo-aux saldo meses)
        (cond
            [(> saldo m) meses]
            [else (tempo-aux (+ (* saldo (+ 1 (/ p 100))) r)(add1 meses))]
        )
    )
    (if (and (<= r 0)(>= m 0))
        (error "tempo: com deposito r <= 0 o total nunca ultrapassa m")
        (tempo-aux 0 0))
)

;testes
(check-equal? (total-acumulado 100 100 0) 0) ;nenhum mes
(check-equal? (total-acumulado 100 0 5) 500) ;sem rendimento r*t
(check-equal? (total-acumulado 100 100 1) 100) ;0 * 2 + 100
(check-equal? (total-acumulado 100 100 3) 700) ;100 -> 300 -> 700
(check-equal? (total-acumulado 100 10 2) 210) ;100 * 1,1 + 100
(check-equal? (total-acumulado 50 20 2) 110) ;50 * 1,2 + 50

(check-equal? (tempo 100 10 -1) 0) ; 0 já é maior que -1
(check-equal? (tempo 100 0 450) 5) ; 500 > 450
(check-equal? (tempo 100 0 500) 6) ; precisa ultrapassar, não só atingir
(check-equal? (tempo 100 100 650) 3) ; 100, 300, 700
(check-equal? (tempo 100 100 700) 4) ; 700 não ultrapassa 700; 1500 sim
(check-equal? (tempo 100 1 1000) 10) ; 9 meses: 936,85 e 10 meses: 1046,22

