(load "fio.lisp")

(defstruct tabuleiro linhas colunas fios)

(defun cria-tabuleiro (linhas colunas)
  (make-tabuleiro :linhas linhas :colunas colunas :fios '()))

(defun tabuleiro-fio-com-id (tabuleiro id)
  (dolist (el tabuleiro-fios res)
    (cond ((= id (fio-id res)) (setf res el)
	  ))))

