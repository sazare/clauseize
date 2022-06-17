;; pnf for quantifier


;;; any parameter list from binds
;(defun make-params (sym  binds)
;  (loop for b in binds collect
;    (cdr b)
;  )
;)

;; quantifier binding rename
(defun rename-bv-form (form &optional binds)
  (let (key)
    (setf key (assoc form binds))
    (cond 
      ((null key) form)
      (t (cdr key))
    )
  )
)

(defun rename-bv-args (args &optional binds)
  (loop for arg in args collect 
    (rename-bv-form arg binds)
  )  
)

(defun rename-bv-pred (atomic &optional binds)
  (cond 
    ((atom atomic) atomic)
    (t (cons (car atomic) (rename-bv-args (cdr atomic) binds)))
  )
)

(defun rename-bv (wff &optional binds)
  "rename all quantifier variable" 
  (let (newv)
    (cond
      ((is-¬ wff) (make-¬ (rename-bv (argof wff 1) binds)))
      ((is-∨ wff) (make-∨ (rename-bv (argof wff 1) binds) (rename-bv (argof wff 2) binds)))
      ((is-∧ wff) (make-∧ (rename-bv (argof wff 1) binds) (rename-bv (argof wff 2) binds)))
  
      ((is-∀ wff) (setq newv (newsym (bvarof wff)))
                  (make-∀ newv (rename-bv (argof wff 1) (cons (cons (bvarof wff) newv) binds))))
  
      ((is-∃ wff) (setq newv (newsym (bvarof wff)))
                  (rename-bv (argof wff 1) (cons (cons (bvarof wff) (skolemize newv binds)) binds)))

  
      (t (rename-bv-pred wff binds))
    )
  )
)



;(defun conv-negate  (wff)
;  "run under keeping a ¬"
;  (cond
;    ((is-¬ wff) (conv-neginto (argof wff 1)))
;    ((is-∨ wff) (make-∧ (conv-negate (argof wff 1))(conv-negate (argof wff 2))))
;    ((is-∧ wff) (make-∨ (conv-negate (argof wff 1))(conv-negate (argof wff 2))))
;    ((is-∀ wff) (make-∃ (bvarof wff) (conv-negate (argof wff 1))))
;    ((is-∃ wff) (make-∀ (bvarof wff) (conv-negate (argof wff 1))))
;    (t (make-¬ wff))
;  )
;)
;
;(defun conv-neginto (wff)
;  "precond: now op is neg, or, and, any, exist only"
;  (cond
;    ((is-¬ wff) (conv-negate (argof wff 1)))
;    ((is-∨ wff) (make-∨ (conv-neginto (argof wff 1))(conv-neginto (argof wff 2))))
;    ((is-∧ wff) (make-∧ (conv-neginto (argof wff 1))(conv-neginto (argof wff 2))))
;    ((is-∀ wff) (make-∀ (bvarof wff) (conv-neginto (argof wff 1))))
;    ((is-∃ wff) (make-∃ (bvarof wff) (conv-neginto (argof wff 1))))
;    (t wff) 
;  )
;)
;
