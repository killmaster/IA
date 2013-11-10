;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                       ;;;;;;
;;;;;;        Projecto de IA 2013/2014       ;;;;;;       
;;;;;;                                       ;;;;;;
;;;;;;                Grupo 45               ;;;;;;
;;;;;;                                       ;;;;;;
;;;;;;          Carlos Martins 57789         ;;;;;;
;;;;;;                                       ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ;;;
;;; Estrutura posicao ;;;
;;;                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct posicao linha coluna)

(defun cria-posicao (linha coluna)
  (make-posicao :linha linha :coluna coluna))

(defun posicoes-iguais-p (pos1 pos2)
  (cond ((and (= (posicao-linha pos1) (posicao-linha pos2)) 
        (=(posicao-coluna pos1) (posicao-coluna pos2)))
   T)
   (T NIL)))

;;;;;;;;;;;;;;;;;;;;;
;;;               ;;;
;;; Estrutura fio ;;;
;;;               ;;;
;;;;;;;;;;;;;;;;;;;;;
(defstruct fio id origem destino)

(defun cria-fio (id origem destino)
  (make-fio :id id :origem origem :destino destino))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;
;;; Estrutura tabuleiro ;;;
;;;                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tabuleiro linhas colunas fios moedas)

(defun cria-tabuleiro (linhas colunas &optional (fios '()) (moedas '()))
  (make-tabuleiro :linhas linhas :colunas colunas :fios fios :moedas moedas))

(defun copia-tabuleiro (tabuleiro)
  (cria-tabuleiro (tabuleiro-linhas tabuleiro) (tabuleiro-colunas tabuleiro) (mapcar #'copy-structure (tabuleiro-fios tabuleiro)) (mapcar #'copy-list (tabuleiro-moedas tabuleiro))))

(defun tabuleiro-fio-com-id (tabuleiro id)
  (let ((res))
  (dolist (el (tabuleiro-fios tabuleiro) res)
    (cond ((= id (fio-id el)) (setf res el))))))

(defun tabuleiro-fios-posicao (tabuleiro posicao)
  (let ((res))
  (dolist (el (tabuleiro-fios tabuleiro) res)
    (cond ((or (posicoes-iguais-p posicao (fio-origem el)) 
	       (posicoes-iguais-p posicao (fio-destino el)))
	   (setf res (nconc res (list el))))))))

(defun tabuleiro-moeda-posicao (tabuleiro posicao)
  (let ((res))
  (dolist (el (tabuleiro-moedas tabuleiro) res)
    (cond ((posicoes-iguais-p posicao (cdr el)) (setf res (car el)))))))

(defun tabuleiro-total-moedas (tabuleiro)
  (let ((res 0))
  (dolist (el (tabuleiro-moedas tabuleiro) res)
    (incf res (car el)))))

(defun tabuleiro-adiciona-fio! (tabuleiro pos1 pos2)
  (let ((acc 1) (tmp))
    (cond ((not (eq (tabuleiro-fios tabuleiro) nil)) (incf acc (fio-id (car(last (tabuleiro-fios tabuleiro)))))))
    (setf tmp (cria-fio acc pos1 pos2))
    (setf (tabuleiro-fios tabuleiro) (nconc (tabuleiro-fios tabuleiro) (list tmp)))))

(defun tabuleiro-adiciona-moeda-posicao! (tabuleiro posicao valor)
 ; (let ((moedas (tabuleiro-moedas tabuleiro)))
  (cond ((or (not(tabuleiro-moeda-posicao tabuleiro posicao)) (eq(tabuleiro-moedas tabuleiro) NIL))
	 (setf (tabuleiro-moedas tabuleiro) (nconc (tabuleiro-moedas tabuleiro) (list (cons valor posicao)))))
	(T (dolist (el (tabuleiro-moedas tabuleiro))
	      (cond ((posicoes-iguais-p (cdr el) posicao) (setf (car el) valor)))))))

(defun tabuleiro-remove-fio-com-id! (tabuleiro id)
  (setf (tabuleiro-fios tabuleiro) (remove (tabuleiro-fio-com-id tabuleiro id) (tabuleiro-fios tabuleiro))))

(defun tabuleiro-remove-moeda-posicao! (tabuleiro posicao)
  (let ((res))
  (dolist (el (tabuleiro-moedas tabuleiro) res)
    (cond ((not (posicoes-iguais-p (cdr el) posicao)) (setf res (nconc res (list el))))))
  (setf (tabuleiro-moedas tabuleiro) res)))
 ;; (setf (tabuleiro-moedas tabuleiro) (remove (cons (tabuleiro-moeda-posicao tabuleiro posicao) posicao) (tabuleiro-moedas tabuleiro))))

;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; Estrutura jogo ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defstruct jogo tabuleiro jogador pontos-jogador1 pontos-jogador2 historico-jogadas)

(defun cria-jogo (tabuleiro &optional (jogador 1) (pontos-jogador1 0) (pontos-jogador2 0) (historico-jogadas '()))
  (make-jogo :tabuleiro tabuleiro :jogador jogador :pontos-jogador1 pontos-jogador1 :pontos-jogador2 pontos-jogador2 :historico-jogadas historico-jogadas))

(defun copia-jogo (jogo)
  (cria-jogo (copia-tabuleiro (jogo-tabuleiro jogo)) (jogo-jogador jogo) (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo) (copy-list (jogo-historico-jogadas jogo))))

;;; Verificar se existe fio,
;;; Remover fio,
;;; Verificar se nao existem fios adjacentes na origem ou no destino,
;;; Se nao existirem remover moedas e adicionar valor das moedas a' pontuacao do jogador
;;; Caso existam fios adjacentes o jogador joga novamente
;;; Caso contrario passa-se ao turno do proximo jogador
(defun jogo-aplica-jogada! (jogo id)
  (let ((fio-tmp (tabuleiro-fio-com-id (jogo-tabuleiro jogo) id))
	(acc 0))
  (cond ((not (eq fio-tmp NIL))
	 (tabuleiro-remove-fio-com-id! (jogo-tabuleiro jogo) id)
	 (setf (jogo-historico-jogadas jogo) (nconc (jogo-historico-jogadas jogo) (list id)))
	; (setf (jogo-historico-jogadas jogo) (reverse (jogo-historico-jogadas jogo)))
	; se nao houver fios nos pontos origem ou destino do fio removido, remover moeda e atribuir pontos
	 (cond ((and (eq (tabuleiro-fios-posicao (jogo-tabuleiro jogo) (fio-origem fio-tmp)) NIL)
		     (eq (tabuleiro-fios-posicao (jogo-tabuleiro jogo) (fio-destino fio-tmp)) NIL))
		(incf acc (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) (fio-origem fio-tmp)))
		(incf acc (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) (fio-destino fio-tmp)))
		(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-origem fio-tmp))
		(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-destino fio-tmp))
		(cond ((eq (jogo-jogador jogo) 1) (incf (jogo-pontos-jogador1 jogo) acc))
		      ((eq (jogo-jogador jogo) 2) (incf (jogo-pontos-jogador2 jogo) acc))))
	       ((eq (tabuleiro-fios-posicao (jogo-tabuleiro jogo) (fio-origem fio-tmp)) NIL)
		(incf acc (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) (fio-origem fio-tmp)))
		(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-origem fio-tmp))
		(cond ((eq (jogo-jogador jogo) 1) (incf (jogo-pontos-jogador1 jogo) acc))
		      ((eq (jogo-jogador jogo) 2) (incf (jogo-pontos-jogador2 jogo) acc))))
	       ((eq (tabuleiro-fios-posicao (jogo-tabuleiro jogo) (fio-destino fio-tmp)) NIL)
		(incf acc (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) (fio-destino fio-tmp)))
		(tabuleiro-remove-moeda-posicao! (jogo-tabuleiro jogo) (fio-destino fio-tmp))
		(cond ((eq (jogo-jogador jogo) 1) (incf (jogo-pontos-jogador1 jogo) acc))
		      ((eq (jogo-jogador jogo) 2) (incf (jogo-pontos-jogador2 jogo) acc))))
	       (T (cond ((eq (jogo-jogador jogo) 1) (setf (jogo-jogador jogo) 2))
		      ((eq (jogo-jogador jogo) 2) (setf (jogo-jogador jogo) 1)))))))))
	 
(defun jogo-terminado-p (jogo)
  (cond ((eq (tabuleiro-fios (jogo-tabuleiro jogo)) '()) T)
  (T NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;; Estrutura problema ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problema
  estado-inicial
  jogador
  accoes
  resultado
  teste-corte-p
  funcao-avaliacao
  historico-accoes
  chaves-equivalencia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;
;;; Funcoes do problema ;;;
;;;                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun accoes (jogo)
  (let ((res))
    (dolist (el (tabuleiro-fios (jogo-tabuleiro jogo)) res)
      (setf res (nconc res (list (fio-id el)))))
    (setf res (reverse res))))

(defun resultado (jogo id)
  (let ((res (copia-jogo jogo)))
    (jogo-aplica-jogada! res id)
    res))
    

(defun teste-terminal-p (jogo prof)
  (declare (ignore prof))
  (jogo-terminado-p jogo))

(defun utilidade (jogo jogador)
  (let ((res))
  (cond ((= jogador 1)
	 (setf res (- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo))))
	(T (setf res (- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;
;;;       Minimax       ;;;
;;;                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun minimax (problema jogador)
  (min-value problema jogador))

(defun min-value (problema jogador)
  (if (teste-terminal-p (problema-estado-inicial problema) NIL)
      (utilidade (problema-estado-inicial problema) (problema-jogador problema))
    (let ((v #.MOST-NEGATIVE-FIXNUM))
      (dolist (a (accoes (problema-estado-inicial problema)) v)
	(setf (problema-estado-inicial problema) (resultado (problema-estado-inicial problema) a))
	(break)
	(setf v (min v (max-value problema jogador)))))))

(defun max-value (problema jogador)
  (if (teste-terminal-p (problema-estado-inicial problema) NIL)
      (utilidade (problema-estado-inicial problema) (problema-jogador problema))
    (let ((v #.MOST-POSITIVE-FIXNUM))
      (dolist (a (accoes (problema-estado-inicial problema)) v)
	(setf (problema-estado-inicial problema) (resultado (problema-estado-inicial problema) a))
	(setf v (max v (min-value problema jogador)))))))
|#
(defun minimax (problema jogador)
  (let ((bestValue) (val) (acc 0))
  (if (teste-terminal-p (problema-estado-inicial problema) NIL)
      (values (first (accoes (problema-estado-inicial problema))) (utilidade (problema-estado-inicial problema) jogador) acc)
    (if (eq jogador (problema-jogador problema))
	(progn (setf bestValue #.MOST-NEGATIVE-FIXNUM)
	       (dolist (el (accoes (problema-estado-inicial problema)) val)
		 (setf (problema-estado-inicial problema) (resultado (problema-estado-inicial problema) el))
		 (setf val (second (multiple-value-list (minimax problema jogador))))
		 (setf bestValue (max bestValue val))
		 (incf acc 1))
	       bestValue)
      (progn (setf bestValue #.MOST-POSITIVE-FIXNUM)
	     (dolist (el (accoes (problema-estado-inicial problema)) val)
	       (setf (problema-estado-inicial problema) (resultado (problema-estado-inicial problema) el))
	       (setf val (second (multiple-value-list (minimax problema jogador))))
	       (setf bestValue (min bestValue val))
	       (incf acc 1))
	      bestValue)))))

(load "interface-moedas.fas")
(load "exemplos.fas")
