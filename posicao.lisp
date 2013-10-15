(defstruct posicao linha coluna)

(defun cria-posicao (linha coluna)
  (make-posicao :linha linha :coluna coluna))

(defun posicoes-iguais-p (pos1 pos2)
  (cond ((and (= (posicao-linha pos1) (posicao-linha pos2)) 
	      (=(posicao-coluna pos1) (posicao-coluna pos2)))
	 T)
	 (T NIL)))
