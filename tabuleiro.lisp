(load "fio.lisp")

(defstruct tabuleiro linhas colunas fios moedas)

(defun cria-tabuleiro (linhas colunas &optional (fios '()) (moedas '()))
  (make-tabuleiro :linhas linhas :colunas colunas :fios fios :moedas moedas))

(defun copia-tabuleiro (tabuleiro)
  (cria-tabuleiro tabuleiro-linhas tabuleiro-colunas (mapcar #'copy-structure tabuleiro-fios) (mapcar #'copy-structure tabuleiro-moedas)))

(defun tabuleiro-fio-com-id (tabuleiro id)
  (dolist (el tabuleiro-fios res)
    (cond ((= id (fio-id res)) (setf res el)
	  ))))

(defun tabuleiro-fio-posicao (tabuleiro posicao)
  (let tmp '())
  (dolist (el tabuleiro-fios res)
    (cond ((or (posicoes-iguais-p posicao (fio-origem el)) 
	       (posicoes-iguais-p posicao (fio-destino el))) 
	  (cons el tmp) (setf res tmp)
	  )))

(defun tabuleiro-moeda-posicao (tabuleiro posicao)
  (dolist (el tabuleiro-moedas res)
    (cond ((posicoes-iguais-p posicao (cdr el))) (setf res (car el)))))

(defun tabuleiro-total-moedas (tabuleiro)
  (dolist (el tabuleiro-moedas res)
    (incf res (car el))))

(defun tabuleiro-adiciona-fio! (tabuleiro pos1 pos2)
  (let acc 1)
  (setf tmp (cria-fio acc pos1 pos2))
  (cons tmp tabuleiro-fios))

(defun tabuleiro-adiciona-moeda-posicao! (tabuleiro posicao valor)
  (cond ((not(tabuleiro-moeda-posicao tabuleiro posicao))
	 (cons valor posicao))
	(dolist (el tabuleiro-moeadas)
	  (cond ((posicoes-iguais-p (cdr el) posicao) (setcar el valor))))))

(defun tabuleiro-remove-fio-com-id! (tabuleiro id)
  (remove (tabuleiro-fio-com-id tabuleiro id) tabuleiro-fios))

(defun tabuleiro-remove-moeda-posicao! (tabuleiro posicao)
  (remove (tabuleiro-moeda-posicao tabuleiro posicao) tabuleiro-moedas))