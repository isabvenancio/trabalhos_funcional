#lang racket
(require rackunit)

;lista de palavras (string-split)
;lista de Contagem (gerar-contagens)
;lista ordenada (ordenar-contagens)
;N primeiros (primeiros-n)
;string de saída (formatar-saida)

;; Uma Contagem é (contagem palavra quantidade)
;;palavra: string
;;quantidade: inteiro positivo
;; guardar = palavra + contagem
(struct contagem (palavra quantidade) #:transparent)
;;contagem-quantidade = retorna a quantidade armazenada
;;contagem-palavra = retorna a palavra armazenada

;;funcao que soma 1 na contagem da palavra, ou adiciona a palavra com
;;quantidade 1 caso ela ainda nao esteja na lista.
;; string (listof Contagem) -> (listof Contagem)
(define (incrementar-palavra palavra contagens)
    (cond
        [(empty? contagens) (list (contagem palavra 1))] ;;Se a lista estiver vazia, significa que a palavra ainda não existe. Então, cria uma nova contagem com quantidade 1
        [(equal? palavra (contagem-palavra (first contagens))) ;;se a palavra for encontrada, cria uma nova estrutura com a quant incrementada em 1 e manter o restante da lista
            (cons (contagem palavra (add1 (contagem-quantidade (first contagens))))(rest contagens))]
        [else (cons (first contagens)(incrementar-palavra palavra (rest contagens)))] ;;se nao for a palavra atual, mantem o primeiro elemento e continua procurando o restante da lista
    )
)

;;funcao que recebe a lista de palavras do texto e retorna a lista
;;de contagens de cada palavra.
;; (listof string) -> (listof Contagem)
(define (gerar-contagens palavras)
    (cond
        [(empty? palavras) empty] ;;Se a lista de palavras estiver vazia, retorna uma lista vazia
        [else (incrementar-palavra (first palavras)(gerar-contagens (rest palavras)))] ;;caso contrário, chama gerar-contagens para processar o restante da lista e usa o incrementar-palavra para add a palavra atual ou aumentar sua quantidade
    )
)

;;funcao que diz se contagem1 deve aparecer antes de contagem2 na saida:
;;maior quantidade primeiro; em caso de empate, menor palavra (string<? -> função usada para comparar strings e determinar se uma string é lexicograficamente menor que outra).
;; Contagem Contagem -> boolean
(define (vem-antes? c1 c2)
    (cond
        [(> (contagem-quantidade c1)(contagem-quantidade c2)) #t]
        [(< (contagem-quantidade c1)(contagem-quantidade c2)) #f]
        [else (string<? (contagem-palavra c1)(contagem-palavra c2))] ;;se as quantidades forem iguais, a palavra em orgem alfabética aparece primeiro, respeitando maiuscula e minuscula (string<?)
    )
)

;;insertion-sort O(n²) - escolhi esse algoritmo de ordenação (mas podemos mudar para outro) 
;;funcao que insere c na posicao correta de uma lista ja ordenada.
;; Contagem (listof Contagem) -> (listof Contagem)
(define (inserir-ordenado c contagens-ordenadas)
    (cond
        [(empty? contagens-ordenadas) (list c)] ;;caso base: se a lista contagens ordenadas estiverem vazias, retorna uma lista com a contagem
        [(vem-antes? c (first contagens-ordenadas)) (cons c contagens-ordenadas)] ;; se contagem vem antes do primeiro, insere no inicio
        [else (cons (first contagens-ordenadas)(inserir-ordenado c (rest contagens-ordenadas)))] ;;caso contário, avança na lista
    )
)
 
;;funcao que ordena a lista de contagens usando inserir-ordenado.
;; (listof Contagem) -> (listof Contagem)
(define (ordenar-contagens contagens)
    (cond
        [(empty? contagens) empty]
        [else (inserir-ordenado (first contagens)(ordenar-contagens (rest contagens)))]
    )
)
 
;;funcao que retorna os n primeiros elementos da lista, ou a lista
;;inteira caso ela tenha menos que n elementos.
;; (listof Contagem) inteiro -> (listof Contagem)
(define (primeiros-n contagens n)
    (cond
        [(or (empty? contagens)(= n 0)) empty] ;; se a lsita estiver vazia ou n igual a zero ent retorna empty
        [else (cons (first contagens)(primeiros-n (rest contagens)(sub1 n)))]; caso contrário, mantém o primeiro elemento com cons e chama a função novamente com o restante da lista, diminuindo n em 1
    )
)
 
;;funcao que transforma uma contagem em uma linha da saida.
;;Ex.: (contagem "casa" 3) -> "casa 3\n"
;;format monta uma string indicando onde cada informação deve entrar.
;;ele ja adiciona o \n (~n) e ~a exibe o valor como texto
;; Contagem -> string
(define (formatar-linha c)
    (format "~a ~a~n" (contagem-palavra c)(contagem-quantidade c)))

;;funcao que concatena as linhas de todas as contagens da lista.
;; string-append: você junta cada pedaço manualmente.
;; (listof Contagem) -> string
(define (formatar-saida contagens)
    (cond
        [(empty? contagens) ""]
        [else (string-append (formatar-linha (first contagens))(formatar-saida (rest contagens)))]))

;;funcao que recebe a string contendo as palavras a serem contados e retorna
;;a string a ser escrita no arquivo de saida, onde essa string eh formada
;;por n linhas, onde cada linha contem uma palavra e sua contagem de aparicao na
;;string de entrada.
; (string-split text) separa o texto em uma lista de palavras.
; (contar-palavras) conta quantas vezes cada palavra aparece.
; (ordenar-contagens) organiza as contagens pela maior quantidade e, em caso de empate, pela ordem alfabética.
; (primeiros-n ... n) seleciona as N primeiras contagens.
; (formatar-saida) transforma tudo em uma string, com cada palavra e sua quantidade em uma linha.
;; string inteiro -> string
(define (conta-palavras text n)
    (formatar-saida (primeiros-n (ordenar-contagens (gerar-contagens (string-split text))) n)))

; ------------------------------------------------------------------------- 

;;Parser de argumentos da linha de comando
(define output-function (make-parameter "output.txt"))
(define number-of-words (make-parameter "5"))
(define input-function (make-parameter "texto.txt"))

(define args (command-line #:program "contador-palavras"
                           #:once-each
                           [("-i" "--input") if
                             "nome do arquivo de entrada (default: texto.txt)"
                             (input-function if)]
                           [("-o" "--output") of
                             "nome do arquivo de saida (default: output.txt)"
                             (output-function of)]
                           [("-n") n
                             "numero de palavras (default: 5)"
                             (number-of-words n)]
                           #:args ()
                           (list (input-function) (output-function) (number-of-words))))

(define arg-lst args)

(define input-file (first arg-lst))  
(define output-file (second arg-lst))
(define number-words (third arg-lst))

;;leitura do arquivo de entrada
(define in-port (open-input-file input-file))
(define input-text (port->string in-port))
(close-input-port in-port)

;;criacao do texto de saida
;;Essa criacao eh feita pela chamada da funcao conta palavras
(define output-text (conta-palavras input-text (string->number number-words)))
;;escrita do resultado no arquivo de saida.
(define out-port (open-output-file output-file #:exists 'replace))
(display output-text out-port)
(close-output-port out-port)

;-----------------------------------------------
;;testes
;; incrementar-palavra
(check-equal? (incrementar-palavra "casa" '())(list (contagem "casa" 1)))
(check-equal? (incrementar-palavra "casa" (list (contagem "casa" 1)))(list (contagem "casa" 2)))
(check-equal? (incrementar-palavra "Casa" (list (contagem "casa" 1)))(list (contagem "casa" 1) (contagem "Casa" 1)))

;; gerar-contagens
(check-equal? (gerar-contagens '())'())
(check-equal? (gerar-contagens (list "a" "b" "a"))(list (contagem "a" 2) (contagem "b" 1)))

;; vem-antes?
(check-true  (vem-antes? (contagem "z" 5) (contagem "a" 2)))  ; maior quantidade
(check-false (vem-antes? (contagem "a" 2) (contagem "z" 5)))
(check-true  (vem-antes? (contagem "a" 3) (contagem "b" 3)))  ; empate: alfabética
(check-true  (vem-antes? (contagem "B" 3) (contagem "a" 3)))  ; maiúscula < minúscula

;; ordenar-contagens
(check-equal? (ordenar-contagens (list (contagem "b" 1) (contagem "a" 3)(contagem "c" 3) (contagem "B" 1)))
                                (list (contagem "a" 3) (contagem "c" 3)(contagem "B" 1) (contagem "b" 1)))

;;formatar-saida
(check-equal? (formatar-saida (list (contagem "a" 2) (contagem "b" 1))) "a 2\nb 1\n")
(check-equal? (formatar-saida '()) "")

;;primeiros-n
(check-equal? (primeiros-n (list (contagem "a" 2)) 5) (list (contagem "a" 2)))

;;conta-palavras
(conta-palavras "casa bola casa gato bola casa" 2)

;racket base.rkt -i textos/blake-poems.txt -o arquivo_de_saida.txt -n 5
