; ito-kqc.lisp for kqc from wff

;; cid counter
(defparameter *cn* 0)
(defun reset-cn () (setf *cn* 0))

;; convert kqc
(defun form-clause (vars lits)
  (cons (incf *cn*)(cons vars lits))
)

(defun form-clauses (vars ws)
  (loop for w in ws collect
    (form-clause vars w)
  )
)

(defun form-kqcn  (vars wffs)
  (loop for w in wffs collect
    (cons (incf *cn*) (cons vars (argsof w)))
  )
)

(defun form-kqc (vars wff)
  (cond 
    ((is-literal wff) (form-clause vars (list wff)))
    ((is-∧ wff) (form-kqcn vars (argsof wff)))
    ((is-∨ wff) (form-clauses vars (argsof wff)))
  )
)


(defun kqcfy (wffs) 
; wffs is all wffs in a file
    (loop for wff in wffs collect
      (let (cls)
        (setf cls (clsfy wff))
        (form-kqc *local-vars* cls)
      )
    )
)

