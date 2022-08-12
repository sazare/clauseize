;; ⇒ を ¬∨に変換する
; 論理結合子をマクロや関数として定義し、evalすると変換されるようにはできないか。


(defmacro ⇒ (a b)
  `(∨ (¬ ,a) ,b)
)

(defmacro ∨ (a b)
  `(∨ ,a ,b)
)

(defmacro ¬ (a)
  `(cond 
    ((listp ,a)
      (cond 
        ((eq '¬ (car ,a)) (car ,a))
        (t ,a)
      ))
    (t ,a)
  )
)

