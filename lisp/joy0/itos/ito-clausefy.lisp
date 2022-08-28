;; ito of clausefy

(myload "ito.lisp")
(asdf:load-system :clsfy)

(defito ito-clausefy()
  "ito of clausefy"
  (reset-cn)
  (intend-equal "r⇒(p⇒q)" '((1 () (- P)(+ Q)(- R))) (clausefy '(⇒ R (⇒ P Q))))
  (intend-equal "(p⇒q)⇒(r⇒s)" '((2 () (+ P)(- r)(+ s))(3 () (- q) (- r) (+ s))) (clausefy '(⇒ (⇒ P Q)(⇒ R S))))
)

; (∨ (¬ (⇒ p q)) (⇒ r s))
;; (∨ (¬ (∨ (¬ p) q))(∨ (¬ r) s))
;;; (∨ (∧ (¬ (¬ P))(¬ q)) (¬ r) s)
;;;; (∧ (∨ P (¬ r) s)(∨ (¬ q) (¬ r) s))



(ito-clausefy)
