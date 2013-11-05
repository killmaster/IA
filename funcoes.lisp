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
  (problema-funcao-avaliacao jogo jogador))
