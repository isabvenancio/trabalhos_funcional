#lang racket
(require rackunit)

;; item a)
;; struct aluno
;; agrupar os dados do aluno
(struct aluno (ra nome nota1 nota2 nota3 nota4 total_faltas) #:transparent)

;; item b)

;; função media-aluno
;; calcula a media do aluno a
(define (media-aluno a)
  (/ (+ (aluno-nota1 a) (aluno-nota2 a) (aluno-nota3 a) (aluno-nota4 a)) 4))

;; funcao status-aluno
;; verifica o status do aluno a, retornando uma string com o status do aluno
(define (status-aluno a nota-aprovacao carga-horaria perc-max-faltas)
  (if (and (< (media-aluno a) nota-aprovacao)
           (> (aluno-total_faltas a) (* carga-horaria (/ perc-max-faltas 100))))
      "Reprovado por falta e nota"
      (if (< (media-aluno a) nota-aprovacao)
          "Reprovado por nota"
          (if (> (aluno-total_faltas a) (* carga-horaria (/ perc-max-faltas 100)))
              "Reprovado por falta"
              "Aprovado"))))

;; Nossa disciplina de teste terá: Aprovação = 6.0, Carga = 60h, Máx Faltas = 25% (ou seja, 15 faltas)
(define a1 (aluno 101 "Carol" 10 10 10 10 0))   ; Aprovada (Média 10, 0 faltas)
(define a2 (aluno 102 "Isa" 5 5 5 5 0))      ; Reprovado por Nota (Média 5, 0 faltas)
(define a3 (aluno 103 "Akira" 10 10 10 10 20)); Reprovado por Falta (Média 10, 20 faltas)
(define a4 (aluno 104 "Moura" 4 4 4 4 20))    ; Reprovado por Falta e Nota (Média 4, 20 faltas)

;; testes
(check-equal? (status-aluno a1 6 60 25) "Aprovado")
(check-equal? (status-aluno a2 6 60 25) "Reprovado por nota")
(check-equal? (status-aluno a3 6 60 25) "Reprovado por falta")
(check-equal? (status-aluno a4 6 60 25) "Reprovado por falta e nota")

;; item c)
;; struct resultado-turma
;; agrupar os dados do resultado da turma
(struct resultado-turma (total-reprovados percentual-reprovados reprovados-nota percentual-reprovados-nota reprovados-faltas percentual-reprovados-faltas media-reprovados-nota) #:transparent)

;; função estatistica-turma
;; calcula a estatística da turma, retornando um struct resultado-turma
(define (estatistica-turma turma nota-aprovacao carga-horaria perc-max-faltas)
    (define (processa-turma turma total-alunos total-reprovados total-reprovados-nota total-reprovados-faltas soma-notas)
        (processa-turma turma 0 0 0 0 0)

        (if (empty? turma) 
            ;; caso base: se a turma estiver vazia, retorna o resultado da turma
            (resultado-turma total-reprovados
                (if (= total-alunos 0) 0 (* (/ total-reprovados total-alunos) 100))
                total-reprovados-nota
                (if (= total-alunos 0) 0 (* (/ total-reprovados-nota total-alunos) 100))
                total-reprovados-faltas
                (if (= total-alunos 0) 0 (* (/ total-reprovados-faltas total-alunos) 100))
                (if (= total-reprovados-nota 0) 0 (/ soma-notas total-reprovados-nota)))
        
            ;; caso recursivo: processa o primeiro aluno da turma e chama a função recursivamente para o restante da turma
            (if (equal? "Aprovado" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas))

                ;; aprovado: soma a nota do aluno e chama a função recursivamente para o restante da turma
                (processa-turma (+ total-alunos 1) total-reprovados total-reprovados-nota total-reprovados-faltas soma-notas)

                ;; reprovado: verifica o motivo da reprovação e chama a função recursivamente para o restante da turma
                (processa-turma (rest turma) (+ total-alunos 1) total-reprovados total-reprovados-nota total-reprovados-faltas soma-notas)
                    (if (or (equal? "Reprovado por nota" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas))
                            (equal? "Reprovado por falta e nota" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas)))
                        (+ total-reprovados-nota 1)
                        total-reprovados-nota)
                    (if (or (equal? "Reprovado por falta" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas))
                            (equal? "Reprovado por falta e nota" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas)))
                        (+ total-reprovados-faltas 1) 
                        total-reprovados-faltas)
                    (if (or (equal? "Reprovado por nota" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas))
                            (equal? "Reprovado por falta e nota" (status-aluno (first turma) nota-aprovacao carga-horaria perc-max-faltas)))
                        (+ soma-notas (media-aluno (first turma)))
                        soma-notas)))))
        

;; testes
(check-equal? (estatistica-turma (list a1 a2 a3 a4) 6 60 25) (resultado-turma 3 75 2 50 2 50 9/2))
(check-equal? (estatistica-turma (list a1 a2) 6 60 25) (resultado-turma 1 50 1 50 0 0 5))
(check-equal? (estatistica-turma (list a1 a3) 6 60 25) (resultado-turma 1 50 0 0 1 50 0))
(check-equal? (estatistica-turma (list a2 a4) 6 60 25) (resultado-turma 2 100 2 100 1 50 9/2))
