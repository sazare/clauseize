;; pnf for quantifier


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

(defun rename-bv* (wffs &optional binds)
  (loop for wff in wffs collect
    (rename-bv wff binds)
  )
)

(defun rename-bv (wff &optional binds)
  "rename all quantifier variable" 
  (let (newv)
    (cond
      ((is-¬ wff) (make-¬ (rename-bv (argof wff 1) binds)))
      ((is-∨ wff) (make-∨* (rename-bv* (argsof wff) binds)))
      ((is-∧ wff) (make-∧* (rename-bv* (argsof wff) binds)))
  
      ((is-∀ wff) (setq newv (newsym (bvarof wff)))
                  (make-∀ newv (rename-bv (argof wff 1) (cons (cons (bvarof wff) newv) binds))))
      ((is-∃ wff) (setq newv (newsym (bvarof wff)))
                  (rename-bv (argof wff 1) (cons (cons (bvarof wff) (skolemize newv binds)) binds)))

      (t (rename-bv-pred wff binds))
    )
  )
)

(defun remove-∀* (wffs)
  (loop for wff in wffs collect
    (remove-∀ wff)
  )
)

(defun remove-∀ (wff)
  "remove all ∀ quantifier. ∃ may be already removed." 
    (cond
      ((is-¬ wff) (make-¬ (remove-∀ (argof wff 1) )))
      ((is-∨ wff) (make-∨* (remove-∀* (argsof wff))))
      ((is-∧ wff) (make-∧* (remove-∀* (argsof wff))))
      ((is-∀ wff) (remove-∀ (argof wff 1)))
      ((is-∃ wff) (remove-∀ (argof wff 1)))
      (t wff)
    )
)

(defun literalize (wff)
  "¬ appear before atomic, no ∨∧∀∃, and
   P → (+ P), (P ...) → (+ P ...); (¬ P)→(+ P)、(¬ P ...) → (- P ...)"

    (cond
      ((is-¬ wff) 
         (cond 
           ((atom (argof wff 1))  (list '- (argof wff 1)))
            (t (cons '- (argof wff 1))))
      )
      ((atom wff) (list ' + wff))
      (t  (cons '+ wff))
    )
)
