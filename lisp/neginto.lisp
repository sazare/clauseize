;; pnf for neg

; objective: move ¬ to just before atom
; step 1: (¬ (op wff)) → (op' (neg wff)
; step 2: (¬ (¬ wff)) → wff
; step 3: test case (pred) 


(defun conv-negate* (wffs)
  (loop for wff in wffs collect
    (conv-negate wff)
  )
)

(defun conv-negate  (wff)
  "run under keeping a ¬"
  (cond
    ((is-¬ wff) (conv-neginto (argof wff 1)))
    ((is-∨ wff) (make-∧* (conv-negate* (argsof wff))))
    ((is-∧ wff) (make-∨* (conv-negate* (argsof wff))))
    ((is-∀ wff) (make-∃ (bvarof wff) (conv-negate (argof wff 1))))
    ((is-∃ wff) (make-∀ (bvarof wff) (conv-negate (argof wff 1))))
    (t (make-¬ wff))
  )
)

(defun conv-neginto* (args)
  (loop for arg in args collect 
    (conv-neginto arg)
  )
)

(defun conv-neginto (wff)
  "precond: now op is neg, or, and, any, exist only"
  (cond
    ((is-¬ wff) (conv-negate (argof wff 1)))
    ((is-∨ wff) (make-∨* (conv-neginto* (argsof wff))))
    ((is-∧ wff) (make-∧* (conv-neginto* (argsof wff))))
    ((is-∀ wff) (make-∀ (bvarof wff) (conv-neginto (argof wff 1))))
    ((is-∃ wff) (make-∃ (bvarof wff) (conv-neginto (argof wff 1))))
    (t wff) 
  )
)

