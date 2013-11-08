(load (compile-file "fio.lisp"))

(defstruct tabuleiro linhas colunas fios moedas)

(defun cria-tabuleiro (linhas colunas &optional (fios '()) (moedas '()))
  (make-tabuleiro :linhas linhas :colunas colunas :fios fios :moedas moedas))

(defun copia-tabuleiro (tabuleiro)
  (cria-tabuleiro (tabuleiro-linhas tabuleiro) (tabuleiro-colunas tabuleiro) (mapcar #'copy-structure (tabuleiro-fios tabuleiro)) (mapcar #'copy-list (tabuleiro-moedas tabuleiro))))

(defun tabuleiro-fio-com-id (tabuleiro id)
  (let ((res))
  (dolist (el (tabuleiro-fios tabuleiro) res)
    (cond ((= id (fio-id res)) (setf res el)
	  )))))

(defun tabuleiro-fios-posicao (tabuleiro posicao)
  (let ((res)
	(tmp '()))
  (dolist (el (tabuleiro-fios tabuleiro) res)
    (cond ((or (posicoes-iguais-p posicao (fio-origem el)) 
	       (posicoes-iguais-p posicao (fio-destino el)))
	   (setf res (nconc res el)))))))

(defun tabuleiro-moeda-posicao (tabuleiro posicao)
  (let ((res))
  (dolist (el (tabuleiro-moedas tabuleiro) res)
    (cond ((posicoes-iguais-p posicao (cdr el)) (setf res (car el)))))))

(defun tabuleiro-total-moedas (tabuleiro)
  (let ((res))
  (dolist (el (tabuleiro-moedas tabuleiro) res)
    (incf res (car el)))))

(defun tabuleiro-adiciona-fio! (tabuleiro pos1 pos2)
  (let ((acc 1) (tmp))
  (setf tmp (cria-fio acc pos1 pos2))
  (setf (tabuleiro-fios tabuleiro) (nconc (tabuleiro-fios tabuleiro) (list tmp)))))

(defun tabuleiro-adiciona-moeda-posicao! (tabuleiro posicao valor)
 ; (let ((moedas (tabuleiro-moedas tabuleiro)))
  (cond ((or (not(tabuleiro-moeda-posicao tabuleiro posicao)) (eq(tabuleiro-moedas tabuleiro) NIL))
	 (setf (tabuleiro-moedas tabuleiro) (cons valor posicao)))
	(T (dolist (el moedas)
	      (cond ((posicoes-iguais-p (cdr el) posicao) (setf (car el) valor)))))))

(defun tabuleiro-remove-fio-com-id! (tabuleiro id)
  (setf (tabuleiro-fios tabuleiro) (remove (tabuleiro-fio-com-id tabuleiro id) (tabuleiro-fios tabuleiro))))

(defun tabuleiro-remove-moeda-posicao! (tabuleiro posicao)
  (setf (tabuleiro-moedas tabuleiro) (remove (tabuleiro-moeda-posicao tabuleiro posicao) (tabuleiro-moedas tabuleiro))))
