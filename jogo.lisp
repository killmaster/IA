(load "tabuleiro.lisp")

(defstruct jogo tabuleiro jogador pontos-jogador1 pontos-jogador2 historico-jogadas)

(defun cria-jogo (tabuleiro &optional (jogador 1) (pontos-jogador1 0) (pontos-jogador2 0) (historico-jogadas '()))
  (make-jogo :tabuleiro tabuleiro :jogador jogador :pontos-jogador1 pontos-jogador1 :pontos-jogador2 pontos-jogador2 :historico-jogadas historico-jogadas))

(defun copia-jogo (jogo)
  (cria-jogo (jogo-tabuleiro jogo) (jogo-jogador jogo) (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo) (mapcar #'copy-structure (jogo-historico-jogadas jogo))))

;;; Verificar se existe fio,
;;; Remover fio,
;;; Verificar se nao existem fios adjacentes na origem ou no destino,
;;; Se nao existirem remover moedas e adicionar valor das moedas a' pontuacao do jogador
;;; Caso existam fios adjacentes o jogador joga novamente
;;; Caso contrario passa-se ao turno do proximo jogador
(defun jogo-aplica-jogada! (jogo id)
  (let fio-tmp (tabuleiro-fio-com-id (jogo-tabuleiro jogo) id))
  (let acc 0)
  (cond ((not (eq fio-tmp NIL)) 
	 (tabuleiro-remove-fio-com-id! (jogo-tabuleiro jogo) id)
	 (cons fio-tmp (jogo-historico-jogadas jogo)) (reverse (jogo-historico-jogadas jogo))
	 ; se nao houver fios nos pontos origem ou destino do fio removido, remover moeda e atribuir pontos
	 (cond ((eq (tabuleiro-fio-posicao (jogo-tabuleiro jogo) (fio-origem fio-tmp)) NIL)
		(incf acc (car (tabuleiro-moeda-posicao jogo-tabuleiro (fio-origem fio-tmp))))
		(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-origem fio-tmp)))
	       ((eq (tabuleiro-fio-posicao (jogo-tabuleiro jogo) (fio-destino fio-tmp)) NIL)
		(incf acc (car (tabuleiro-moeda-posicao jogo-tabuleiro (fio-destino fio-tmp))))
		(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-destino fio-tmp))))
	 (cond ((not (eq acc 0))
		(cond ((eq (jogo-jogador jogo) 1) (setf (jogo-jogador jogo) 2))
		      ((eq (jogo-jogador jogo) 2) (setf (jogo-jogador jogo) 1)))
		(T (cond ((eq (jogo-jogador jogo) 1) (incf (jogo-pontos-jogador1 jogo) acc))
			 (T incf (jogo-pontos-jogador2 jogo) acc))))))))

(defun jogo-terminado-p (jogo)
  (cond ((eq (tabuleiro-fios (jogo-tabuleiro jogo)) '()) T)
	(T NIL)))