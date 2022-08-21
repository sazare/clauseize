; ito-kqc.lisp for kqc from wff

;; cid counter
(defparameter *cn* 0)
(defun reset-cn () (setf *cn* 0))

;; convert kqc
(defun form-clause (vars lits)
  (cons (incf *cn*)(cons vars lits))
)
;
;(defun form-clauses (vars ws)
;  (loop for w in ws collect
;    (form-clause vars w)
;  )
;)
;
;(defun form-kqcn  (vars wffs)
;  (loop for w in wffs collect
;    (cons (incf *cn*) (cons vars (argsof w)))
;  )
;)
;
;(defun form-kqc (vars wff)
;  (cond 
;    ((is-literal wff) (form-clause vars (list wff)))
;    ((is-∨ wff) (form-clauses vars (argsof wff)))
;    ((is-∧ wff) (form-kqcn vars (argsof wff)))
;    (t (format t "form-kqc: fatal form ~a~%" wff))
;  )
;)
;
;
;(defun kqcfy (wffs) 
; "wffs is all wffs in a file"
;    (loop for wff in wffs collect
;      (let (cls)
;        (setq cls (clsfy wff))
;        (form-kqc *local-vars* cls)
;      )
;    )
;)
;
;;;;;;

(defun bind-flatten (vars adt)
  (let ((flas (flatten adt)))
    (loop for cls in flas collect 
      (form-clause (fit-vars vars cls) cls)
    )
  )
)

(defun flatten (adt)
  "convert tree ∧-∨ form to clause form"
  (cond 
    ((is-literal adt) (list (flatten1* (list adt))))
    ((is-∨ adt) (list (flatten1* (argsof adt))))
    ((is-∧ adt) (flatten2* (argsof adt)))
  )
)


(defun flatten1 (adt)
  (cond
    ((is-literal adt) (list adt))
    ((is-∨ adt) (flatten1* (argsof adt)))
    ((is-∧ adt) (format t "flattern1: form of wff at ~a~%" adt))
  )
)

(defun flatten1* (disj) 
  (loop for dis in disj append 
    (flatten1 dis)
  )
)

(defun flatten2* (disjs)
  (loop for disj in disjs collect
    (flatten1 disj)
  )
)

;;; clausefy

(defun clausefy (wff)
  (let (w upc)
    (reset-vars)
    (setq upc (upconj (extract-vars (rename-bv (conv-neginto (conv-imply wff))))))
    (setq w (bind-flatten *local-vars* upc))
    (values w *local-vars*)
  )
)

(defun fit-vars (vars cs)
  (loop for v in vars
    when (var-in-clause? v cs) collect v
  )
)

(defun var-in-clauseset? (v cs)
  (loop for lit in cs thereis (var-in-clause? v lit))
)

(defun var-in-clause? (v clause)
  (loop for lit in clause thereis (var-in-literal? v lit))
)

(defun var-in-literal? (v literal)
  (cond
    ((null (cddr literal)) nil)
    (t (member v (cddr literal)))
  )
)

    
