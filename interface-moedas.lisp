;; acrescentei algumas funcoes auxiliares que vao dar jeito para testar automaticamente o codigo dos alunos
(defun ignore-value (x)
	(declare (ignore x))
	NIL)
	
;; dois fios sao iguais se tem o mesmo id, a mesma origem e o mesmo destino
(defun fios-iguais-p (f1 f2)
	(and (equal (fio-id f1) (fio-id f2))
		 (posicoes-iguais-p (fio-origem f1) (fio-origem f2))
		 (posicoes-iguais-p (fio-destino f1) (fio-destino f2))))
		 
;; muito dificil fazer esta funcao... :D
(defun conjuntos-iguais-p (l1 l2 &key (test #'equal))
	(and (null (set-difference l1 l2 :test test))
		 (null (set-difference l2 l1 :test test))))
		 

;;; joga: tabuleiro x funcao x funcao x inteiro --> inteiro
;;; funcao que recebe um tabuleiro; duas funcoes correspondentes a jogadores, i.e. funcoes que recebem um jogo e um id de jogador e devolvem 
;;; uma jogada; e um inteiro opcional que representa o tempo limite (em segundos) que cada jogador tem para tomar uma decisao (se nao for especificado
;;; e usado o tempo limite por omissao de 30 segundos). Esta funcao executa um jogo entre os dois jogadores recebidos, criando um jogo a partir
;:; do tabuleiro recebido, e pedindo a cada jogador (alternadamente ou nao) uma jogada. A funcao acaba quando o jogo e considerado terminado
;;; e retorna a diferenca de pontos entre o jogador 1 e 2. Ou seja, se o jogador 1 ganhou, retorna um valor positivo, se o jogador 2 ganhou
;;; retorna um valor negativo, e retorna 0 em caso de empate. 
;;; Importante: esta funcao nao altera o tabuleiro recebido, pois faz uma copia dele
(defun joga (tabuleiro jogador1 jogador2 &optional (tempo-limite 30))
	(let ((jogo-aux (cria-jogo (copia-tabuleiro tabuleiro))))
		(loop while (not (jogo-terminado-p jogo-aux))
			do 
			(desenha-jogo jogo-aux)
			(if (= 1 (jogo-jogador jogo-aux))
				(jogo-aplica-jogada! jogo-aux (funcall jogador1 jogo-aux 1 tempo-limite))
				(jogo-aplica-jogada! jogo-aux (funcall jogador2 jogo-aux 2 tempo-limite)))
			(format T "~%"))
		(desenha-jogo jogo-aux)
		(cond ((> (jogo-pontos-jogador1 jogo-aux) (jogo-pontos-jogador2 jogo-aux)) (format T "O Jogador 1 venceu!~%"))
			  ((< (jogo-pontos-jogador1 jogo-aux) (jogo-pontos-jogador2 jogo-aux)) (format T "O Jogador 2 venceu!~%"))
			  (T (format T "Os jogadores empataram!~%")))
		(- (jogo-pontos-jogador1 jogo-aux) (jogo-pontos-jogador2 jogo-aux))))

;;; jogador-humano: jogo x inteiro x inteiro --> inteiro 
;;; funcao que recebe um jogo, um inteiro correspondente ao ID de um jogador, e um inteiro opcional que indica o tempo
;;; limite para tomar uma decisao. Retorna um inteiro correspondente ao numero do fio que o jogador pretende cortar.
;;; Esta funcao serve de interface com o utilizador, pedindo-lhe para escrever o codigo do fio desejado, e verificando
;;; se este e valido. Se o numero introduzido nao for valido, esta funcao escreve uma mensagem de erro no ecra e
;;; volta a pedir um novo numero.
;;; Apesar de receber o limite de tempo, nao sera tido em conta nesta funcao. Assim o jogador pode levar o tempo que
;;; entender para tomar sua decisao. Esta funcao recebe o limite de tempo apenas para ser chamado da mesma maneira que
;;; um jogador automatico.
(defun jogador-humano (jogo jogador &optional tempo-limite)
	(declare (ignore tempo-limite)) ;; isto indica ao lisp que o tempo-limite nao e usado, e assim nao gera warning na compilacao
	(let ((jogada NIL)
		  (input NIL))
		(loop while (null jogada)
			do
			(format T "Jogador ~D, escolha um fio a remover: " jogador)
			(setf input (read-from-string (read-line) NIL))
			(if (not (integerp input))
				(format T "O valor introduzido nao e um inteiro valido.~%")
				(progn
					(setf jogada (tabuleiro-fio-com-id (jogo-tabuleiro jogo) input))
					(when (null jogada) (format T "Jogada invalida. O fio inserido nao existe.~%")))))
		(fio-id jogada)))
		
	
;;; desenha-jogo: jogo --> {}
;;; funcao que recebe um jogo, e desenha o estado do jogo no ecra, incluindo informacao acerca do numero de pontos por jogador
;;; nao retorna nada
(defun desenha-jogo (jogo)
	(desenha-tabuleiro (jogo-tabuleiro jogo))
	(format T "Jogador 1:~4D moedas     Jogador 2:~4D moedas~%~%" (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo)))
	
;;; desenha-tabuleiro: tabuleiro --> {}
;;; funcao que recebe um tabuleiro e desenha o tabuleiro no ecra, com todas as moedas e fios
;;; nao retorna nada 
(defun desenha-tabuleiro (tabuleiro)	
	(desenha-barra-horizontal tabuleiro)
	(dotimes (linha (tabuleiro-linhas tabuleiro))
		(desenha-linha-tabuleiro tabuleiro linha))
	(desenha-barra-horizontal tabuleiro))
	
;;; funcoes auxiliares para desenhar o tabuleiro, nao as defini com flet/labels porque a indentacao ia ficar muito dificil
;;; de perceber

;;; desenha-barra-horizontal: tabuleiro --> {}
;;; funcao que recebe um tabuleiro e desenha uma linha horizontal com o caracter "="
;;; a linha tem o tamanho correspondente ao tamanho do tabuleiro recebido
;;; esta funcao nao retorna nada
(defun desenha-barra-horizontal (tabuleiro)
	(format T "====")
	(dotimes (coluna (- (tabuleiro-colunas tabuleiro) 1))
		(format T "=========="))
	(format T "===") ;; para a ultima coluna so existem 3 subcolunas
	(format T "====~%"))

;;; desenha-linha-tabuleiro: tabuleiro x inteiro positivo --> {}
;;; funcao que recebe um tabuleiro e um inteiro >= 0 que indica uma linha do tabuleiro, e desenha no ecra as moedas e fios 
;;; correspondente a linha do tabuleiro
;;; Uma linha do tabuleiro corresponde a 6 linhas impressas (chamadas de sublinhas) no ecra. Podemos ver um exemplo das 6 sublinhas
;;; impressas para a linha 0 e coluna 0, com uma moeda na posicao 0,0 e varios fios ligando as moedas adjacentes a posicao 0,0
;;; (3)---04-- 
;;;  | \     /  
;;;  | 03      
;;;  02   x    
;;;  |  04     
;;;  | /     \ 
(defun desenha-linha-tabuleiro (tabuleiro linha)
	(let ((array-ids-fios-a-imprimir (make-array (list (tabuleiro-colunas tabuleiro)))))
		(dotimes (coluna (tabuleiro-colunas tabuleiro))
		;;; calcula-se os id's dos fios que tem de ser desenhados em cada linha e coluna 
			(setf (aref array-ids-fios-a-imprimir coluna) (calcula-id-fios-a-imprimir linha coluna (tabuleiro-fios tabuleiro))))
		(if (< linha (- (tabuleiro-linhas tabuleiro) 1)) ;;se nao estivermos na ultima linha desenhamos 6 sublinhas
			(dotimes (sublinha 6)
				;;;desenhamos uma sublinha
				(desenha-sublinha-tabuleiro tabuleiro linha sublinha array-ids-fios-a-imprimir))
			;; na ultima linha desenhamos apenas a sublinha 0, isto porque a ultima linha nao vai ter fios a ligar moedas para baixo
			(desenha-sublinha-tabuleiro tabuleiro linha 0 array-ids-fios-a-imprimir))))
			
;;; obtem-vector-fio: inteiro positivo x inteiro positivo x inteiro positivo --> posicao
;;; recebe um inteiro correspondente a uma linha do tabuleiro, um inteiro correspondente
;;; a uma coluna, e um inteiro correspondente a um fio, e verifica se o fio esta ligado
;;; a linha e coluna do tabuleiro. Se o fio nao estiver ligado a funcao retorna NIL, se o fio estiver
;;; ligado retorna uma posicao que representa o vector do fio. Por exemplo, um fio horizontal
;;; e representado por (1,0) ou (-1,0). Existe um caso especial usado para detectar um fio que embora
;;; nao esteja ligado a linha/coluna, corresponde a um fio diagonal inverso (este fio tem que ser desenhado
;;; juntamente com os fios ligados). Neste caso e retornado o vector (-2,2).  			
(defun obtem-vector-fio (linha coluna fio)
		(let* ((origem (fio-origem fio)) 
			   (destino (fio-destino fio))
			   (linha-destino (posicao-linha destino))
			   (coluna-destino (posicao-coluna destino))
			   (linha-origem (posicao-linha origem))
			   (coluna-origem (posicao-coluna origem)))
			(cond ((and (eq linha linha-origem)
						(eq coluna coluna-origem)) (obtem-vector origem destino))
				  ((and (eq linha linha-destino)
						(eq coluna coluna-destino)) (obtem-vector destino origem))
				  ((and (eq linha linha-origem)
						(eq (+ coluna 1) coluna-origem)
						(eq (+ linha 1) linha-destino)
						(eq coluna coluna-destino))(cria-posicao -2 2))
				  ((and (eq linha linha-destino)
					    (eq (+ coluna 1) coluna-destino)
						(eq (+ linha 1) linha-origem)
						(eq coluna coluna-origem))(cria-posicao -2 2))
				  (T NIL))))
				  
;;; obtem-vector: posicao x posicao --> posicao
;;; recebe uma posicao pOrigem, e outra posicao pDestino e devolve o vector
;;; que corresponde a diferenca entre o destino e a origem. O vector retornado
;;; e representado tambem atraves de uma posicao
(defun obtem-vector (pOrigem pDestino)
	(cria-posicao (- (posicao-linha pDestino) (posicao-linha pOrigem))
		  (- (posicao-coluna pDestino) (posicao-coluna pOrigem))))
 
;;;	calcula-id-fios-a-imprimir: inteiro positivo x inteiro positivo x lista --> lista			  
;;;	recebe um inteiro correspondente a uma linha, um inteiro correspondente a uma coluna,
;;;	uma lista com todos os id's de fios do tabuleiro, e determina qual o id do fio horizontal/vertical/diagonal que deve
;;; ser desenhado juntamente com a linha e coluna recebidas. Retorna uma lista com 4 elementos.
;;; O 1 elemento corresponde ao id do fio horizontal (ou NIL se nao existir),
;;; o 2 ao id do fio vertical (ou NIL se nao existir), o 3 ao id do fio diagonal (ou NIL se nao existir),
;;; o 4 ao id do fio diagonal inverso (ou NIL se nao existir).
(defun calcula-id-fios-a-imprimir (linha coluna fios)
	(let ((fio-horizontal NIL)
		  (fio-vertical NIL)
		  (fio-diagonal NIL)
		  (fio-diagonal-inv NIL)
		  (vec-diagonal-inv (cria-posicao -2 2))
		  (vec-diagonal (cria-posicao 1 1))
		  (vec-vertical (cria-posicao 1 0))
		  (vec-horizontal (cria-posicao 0 1))
		  (vec NIL))
		(dolist (fio fios)
			(setf vec (obtem-vector-fio linha coluna fio))
			(when (not (null vec))
				(cond ((posicoes-iguais-p vec-diagonal-inv vec) (setf fio-diagonal-inv (fio-id fio)))
					  ((posicoes-iguais-p vec-diagonal vec) (setf fio-diagonal (fio-id fio)))
					  ((posicoes-iguais-p vec-vertical vec) (setf fio-vertical (fio-id fio)))
					  ((posicoes-iguais-p vec-horizontal vec) (setf fio-horizontal (fio-id fio))))))
			(list fio-horizontal fio-vertical fio-diagonal fio-diagonal-inv)))


;;; desenha-sublinha-tabuleiro: tabuleiro x inteiro positivo x inteiro positivo x vector --> {}
;;; funcao que recebe um tabuleiro, um inteiro >= 0 correspondente a uma linha,
;;; um inteiro >= 0 correspondente a sublinha, e um array unidimensional com tamanho igual ao numero
;;; de colunas do tabuleiro, que contem em cada posicao uma lista com os ids dos fios (horizontais, verticais e 
;;; diagonais que estao ligados a coluna correspondente
;;; a funcao vai desenhar a sublinha recebida no ecra, tendo em conta os fios que estao ligados as moedas da linha 
;;; nao retorna nada
(defun desenha-sublinha-tabuleiro (tabuleiro linha sublinha array-ids-fios-a-imprimir)
	(let ((colunas (tabuleiro-colunas tabuleiro)))
		(format T "||  ")
		(dotimes (coluna (- colunas 1)) ;;desenhamos todas menos a ultima coluna
			(desenha-sublinha-celula 
								sublinha 
								(tabuleiro-moeda-posicao tabuleiro (cria-posicao linha coluna)) 
								(aref array-ids-fios-a-imprimir coluna)))
		(desenha-sublinha-celula-ultima-coluna  ;;a ultima coluna tem de ser desenhada de forma diferente
							sublinha 
							(tabuleiro-moeda-posicao tabuleiro (cria-posicao linha (- colunas 1)))
							(aref array-ids-fios-a-imprimir (- colunas 1)))
		(format T "  ||~%")))
		

;;; desenha-sublinha-celula: inteiro positivo x inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma sublinha de uma celula, um inteiro correspondente a uma moeda ou NIL se
;;; nao existir moeda na celula, e uma lista de 4 elementos com os ids dos fios horizontal,vertical,diagonal e diagonal inverso
;;; a desenhar na celula. Esta funcao desenha uma sublinha da celula em questao.
;;; Uma celula corresponde ao conjunto de todas as sublinhas e subcolunas correspondentes a uma linha e coluna do tabuleiro
;;; Por exemplo, esta celula corresponde a linha 0 e coluna 0 do tabuleiro, e tem uma moeda de valor - 3, um fio horizontal - 4,
;;; um fio vertical - 2, um fio diagonal - 3, e um fio diagonal inversa 5.
;;; (3)---04-- 
;;;  | \     /  
;;;  | 03      
;;;  02   x    
;;;  |  05     
;;;  | /     \
(defun desenha-sublinha-celula (sublinha moeda ids-fios-a-imprimir)
	;;em vez de fazer um cond com 6 condicoes, posso simplemente fazer isto
	(funcall (nth sublinha (list #'desenha-celula-sl0 #'desenha-celula-sl1 
								 #'desenha-celula-sl2 #'desenha-celula-sl3
								 #'desenha-celula-sl4 #'desenha-celula-sl5))
				moeda
				ids-fios-a-imprimir))

;;; desenha-sublinha-celula-ultima-coluna: inteiro positivo x inteiro positivo x lista --> {}
;;; ver definicao da funcao anterior. Esta funcao e um caso particular da funcao anterior,
;;; porque as celulas correspondentes a ultima coluna do tabuleiro so tem uma unica subcoluna.
;;; Isto acontece porque nao existe nenhum fio que ligue moedas da ultima coluna a uma coluna mais a 
;;; direita.				
(defun desenha-sublinha-celula-ultima-coluna (sublinha moeda ids-fios-a-imprimir)
	(funcall (nth sublinha (list #'desenha-celula-sl0-sc0 #'desenha-celula-sl1-sc0 
								 #'desenha-celula-sl1-sc0 #'desenha-celula-sl3-sc0
								 #'desenha-celula-sl1-sc0 #'desenha-celula-sl1-sc0))
				moeda
				ids-fios-a-imprimir))	

			
;;; desenha-celula-sl0-sc0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a subcoluna 0 da sublinha 0 da celula. Nao retorna nada.
(defun desenha-celula-sl0-sc0 (moeda ids-fios-a-imprimir)
	(declare (ignore ids-fios-a-imprimir))
	(if moeda (format T "(~d)" moeda)(format T "   ")))
	
;;; desenha-celula-sl0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 0 da celula. Nao retorna nada.
(defun desenha-celula-sl0 (moeda ids-fios-a-imprimir)
	(desenha-celula-sl0-sc0 moeda ids-fios-a-imprimir)
	(if (first ids-fios-a-imprimir) 
		(format T "---~2,'0D--" (first ids-fios-a-imprimir))
		(format T "       ")))

;;; desenha-celula-sl1-sc0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a subcoluna 0 da sublinha 1 da celula. Nao retorna nada.		
(defun desenha-celula-sl1-sc0 (moeda ids-fios-a-imprimir)
	(declare (ignore moeda))
	(if (second ids-fios-a-imprimir) (format T " | ")(format T "   ")))

;;; desenha-celula-sl1: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 1 da celula. Nao retorna nada.	
(defun desenha-celula-sl1 (moeda ids-fios-a-imprimir)
	(desenha-celula-sl1-sc0 moeda ids-fios-a-imprimir)
	(if (third ids-fios-a-imprimir) (format T "\\     ")(format T "      "))
	(if (fourth ids-fios-a-imprimir) (format T "/")(format T " ")))

;;; desenha-celula-sl2: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 2 da celula. Nao retorna nada.	
(defun desenha-celula-sl2 (moeda ids-fios-a-imprimir)
	(desenha-celula-sl1-sc0 moeda ids-fios-a-imprimir) ;; desenhar a subcoluna 0 da sl 2 e equivalente a desenhar a da sl 1
	(if (third ids-fios-a-imprimir)(format T " ~2,'0D    " (third ids-fios-a-imprimir))(format T "       ")))
	
;;; desenha-celula-sl3-sc0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a subcoluna 0 da sublinha 3 da celula. Nao retorna nada.
(defun desenha-celula-sl3-sc0 (moeda ids-fios-a-imprimir)
	(declare (ignore moeda))
	(if (second ids-fios-a-imprimir)(format T " ~2,'0D" (second ids-fios-a-imprimir))(format T "   ")))

;;; desenha-celula-sl3: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 3 da celula. Nao retorna nada.		
(defun desenha-celula-sl3 (moeda ids-fios-a-imprimir)
	(desenha-celula-sl3-sc0 moeda ids-fios-a-imprimir) 
	(cond ((and (third ids-fios-a-imprimir) (fourth ids-fios-a-imprimir)) (format T "   x   "))
		  ((third ids-fios-a-imprimir) (format T "   \\   "))
		  ((fourth ids-fios-a-imprimir) (format T "   /   "))
		  (T (format T "       "))))
		  
;;; desenha-celula-sl4: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 4 da celula. Nao retorna nada.			  
(defun desenha-celula-sl4 (moeda ids-fios-a-imprimir)
	(desenha-celula-sl1-sc0 moeda ids-fios-a-imprimir) ;; desenhar a subcoluna 0 da sl 4 e equivalente a desenhar a da sl 1
	(if (fourth ids-fios-a-imprimir) (format T " ~2,'0D    " (fourth ids-fios-a-imprimir)) (format T "       ")))

;;; desenha-celula-sl5: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 5 da celula. Nao retorna nada.	
(defun desenha-celula-sl5 (moeda ids-fios-a-imprimir)
	(desenha-celula-sl1-sc0 moeda ids-fios-a-imprimir) ;; desenhar a subcoluna 0 da sl 5 e equivalente a desenhar a da sl 1
	(if (fourth ids-fios-a-imprimir) (format T "/     ") (format T "      "))
	(if (third ids-fios-a-imprimir) (format T "\\") (format T " ")))