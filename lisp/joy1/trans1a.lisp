;; ⇒ を ¬∨に変換する
; 論理結合子をマクロや関数として定義し、evalすると変換されるようにはできないか。
; 変換に結合子と違う名前をつけるほうがよい。そうでないと書けない。か

(defmacro ≡ (a b)
  `(∧ (∨ (¬ ',a) ',b)(∨ ',a (¬ ',b)))
)

(defmacro ⇒ (a b)
  `(∨ (¬ ',a) ',b)
)

(defmacro ∧ (a b)
  `(∧ ',a ',b)
)

(defmacro ∨ (a b)
  `(∨ ',a ',b)
)

;; ¬¬a => aは、変換に違う名前をつけたほうがよい。
(defmacro  ¬ (a)
  `(- ',a)
)

(defun nop (a)
  (cond 
    ((or (atom a)(equal '∨ (car a))(equal '∧ (car a))(equal '¬ (car a))) a)
  )
)


(defparameter *conn* '(¬ ∨ ∧ ∀ ∃ ⇒ ≡))

(defun literal? (a)
  (cond
    ((atom a) t)
    ((member (car a) *conn*) nil)
    (t t)
  )
)

(defun ¬inner* (as)
  (loop for a in as collect
    (¬inner* a)
  )
)

;;; this is idiot
(defun ¬inner (a)
  (cond 
    ((literal? a) `(¬ ,a))
    ((eq '¬ (car a)) (¬inner* (cdr a)))
    (t (list(car a) (¬inner (cadr a))))
  )
)

    

;; changed ∀∃ expression form
;; (∀ x (P x y)) => ((x1) (P x1 y))
;; (∀ x (∃ z (R x z))) => ((x1) (R x1 (z x1)))


;; (∨ A (∨ B C) (∧ D E))
;; (∧ A (∨ B C) (∧ D E))

