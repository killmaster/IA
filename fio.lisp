(load "posicao.lisp")

(defstruct fio id origem destino)

(defun cria-fio (id origem destino)
  (make-fio :id id :origem origem :destino destino))