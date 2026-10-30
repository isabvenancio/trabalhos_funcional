#lang racket

(require racket/file)

;; Funcao que compara dois arquivos de texto.
;; string string -> boolean
(define (conferir arquivo-gerado arquivo-esperado)
  (equal? (file->string arquivo-gerado)
          (file->string arquivo-esperado)))

;; Funcao que informa se o teste passou ou falhou.
(define (testar-saida arquivo-gerado arquivo-esperado)
  (cond
    [(conferir arquivo-gerado arquivo-esperado)
        (displayln "Teste passou! Os arquivos sao iguais.")]
    [else
        (displayln "Teste falhou! Os arquivos sao diferentes.")]))

(conferir "output.txt" "esperado.txt")