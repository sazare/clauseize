; 1st step of cnf tran


(defun ∧ (a b)
  `(∧ ,a ,b)
)

(defun ∨ (a b)
  `(∨ ,a ,b)
)

(defun ¬ (a)
  `(¬ ,a)
)

(defmacro ≡ (a b)
  `(∧ (∨ (¬ ',a) ',b)(∨ ',a (¬ ',b)))
)

(defmacro ⇒ (a b)
  `(∨ (¬ ',a) ',b)
)


;; macro dont recursive applied?

(⇒ (⇒ p q) r)

;CL-USER(2): (⇒ (⇒ p q) r)
;
;(∨ (¬ (⇒ P Q)) R)
;CL-USER(3): (≡ (≡ p q) r)
;
;(∧ (∨ (¬ (≡ P Q)) R) (∨ (≡ P Q) (¬ R)))

;; alternative question
;;; how to write recursive in macro? can i?


