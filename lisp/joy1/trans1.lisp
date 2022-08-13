;; ⇒ を ¬∨に変換する
; 論理結合子をマクロや関数として定義し、evalすると変換されるようにはできないか。
; defunしている∧∨¬はdefmacroにすると実行されてしまうので、このまま。



(defmacro ≡ (a b)
  `(∧ (∨ (¬ ,a) ,b)(∨ ,a (¬ ,b)))
)

(defmacro ⇒ (a b)
  `(∨ (¬ ,a) ,b)
)

(defun ∧ (a b)
  (list '∧ a b)
)

(defun ∨ (a b)
  (list '∨ a b)
)

;(defmacro ∨ (a b)
;  `(∨ ,a ,b)
;)

(defun ¬ (a)
  (list '¬ a)
)

;(defmacro ¬ (a) 
;  `(¬ ,a)
;)

;(defmacro ¬ (a)
;  `(cond 
;    ((listp ,a)
;      (cond 
;        ((eq '¬ (car ,a)) (car ,a))
;        (t ,a)
;      ))
;    (t ,a)
;  )
;)
;
