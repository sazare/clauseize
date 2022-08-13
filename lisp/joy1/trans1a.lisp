;; ⇒ を ¬∨に変換する
; 論理結合子をマクロや関数として定義し、evalすると変換されるようにはできないか。
; 変換に結合子と違う名前をつけるほうがよい。そうでないと書けない。か


(defmacro ≡ (a b)
  `(∧ (∨ (¬ ,a) ,b)(∨ ,a (¬ ,b)))
)

(defmacro ⇒ (a b)
  `(∨ (¬ ,a) ,b)
)

(defun ∧ (a b)
  `(∧ ,a ,b)
)

(defun ∨ (a b)
  `(∨ ,a ,b)
)

;; ¬¬a => aは、変換に違う名前をつけたほうがよい。
(defun ¬ (a)
  `(¬ ,a)
)

;; changed ∀∃ expression form
;; (∀ x (P x y)) => ((x1) (P x1 y))
;; (∀ x (∃ z (R x z))) => ((x1) (R x1 (z x1)))


;; (∨ A (∨ B C) (∧ D E))
;; (∧ A (∨ B C) (∧ D E))

