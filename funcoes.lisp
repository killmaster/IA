(load (compile-file "problema.lisp"))

(defun accoes (jogo)
  (let ((res))
    (dolist (el (tabuleiro-moedas (jogo-tabuleiro jogo)))
      (cons el res))))

(defun resultado (jogo id)
  (let ((res (copia-jogo jogo)))
    (jogo-aplica-jogada! res id)))

(defun teste-terminal-p (jogo prof)
  (declare (ignore prof))
  (jogo-terminado-p jogo))

(defun utilidade (jogo jogador)
  (let ((res))
  (cond ((= jogador 1)
	 (setf res (- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo))))
	(T (setf res (- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo)))))))
