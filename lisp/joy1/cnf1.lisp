; 1st step of cnf tran

;;
(defun ∧ (a b)
  `(∧ ,a ,b)
)

(defun ∨ (a b)
  `(∨ ,a ,b)
)

(defun ¬ (a)
  `(¬ ,a)
)

;  (∀ x (Pp x)) ok not (∀ (x y) (pp xy))
(defun ∀ (vs e) `(∀ ,vs ,e))
;(defun ∀ (vs e) `(∀ ,vs ,e))


(defun ∃ (vs e) `(∃ ,vs ,e))
;(defun ∃ (vs e) `(∃ ,vs ,e))

(defmacro ≡ (a b)
  `(∧ (⇒ ,a ,b) (⇒ ,b ,a))
)

(defmacro ⇒ (a b)
  `(∨ (¬ ,a) ,b)
)


;; macro dont recursive applied?
;; while prop or pred are defined identical one, work.

;(⇒ (⇒ p q) r)
;
;(≡ (⇒ p q)(⇒ (pp a)(qq b)))
;
;(∀ x (pp x))
;
;(∀ (x y) (∧ (pp x)(qq y)))

