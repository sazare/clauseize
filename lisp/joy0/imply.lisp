;; pnf for ≡、⇒、⇦

(defun conv-imply (wff)
  (cond
    ((is-≡ wff) (make-∧ (make-∨ (make-¬ (conv-imply (argof wff 1))) (conv-imply (argof wff 2))) 
                        (make-∨ (conv-imply (argof wff 1)) (make-¬ (conv-imply (argof wff 2))))))
    ((is-⇒ wff) (make-∨ (make-¬ (conv-imply (argof wff 1))) (conv-imply (argof wff 2))))
    ((is-⇒ wff) (make-∨ (make-¬ (conv-imply (argof wff 1))) (conv-imply (argof wff 2))))
    ((is-¬ wff) (make-¬ (conv-imply (argof wff 1))))
    ((is-∀ wff) (make-∀ (bvarof wff) (conv-imply (argof wff 1))))
    ((is-∃ wff) (make-∃ (bvarof wff) (conv-imply (argof wff 1))))
    (t wff) ; just an atomic
  )
)



