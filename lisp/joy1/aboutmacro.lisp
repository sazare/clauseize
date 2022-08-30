;;; macro and function

(defmacro aa (x y) `(list ,x ,y))
(defun aaf (x y) `(list ,x ,y))


;;; eval
(defmacro ev (f a) `(,f ,a))

;(defmacro eev (e) `((car ,e)(cdr ,e)))
(defmacro eev (e) `(,(car e),(cdr e)))

;;; setq
(defmacro mysetq (v e) `(set ',v ,e))


;; logic
(defparameter *logop* "∨∧¬∀∃⇒≡")

(defun islogicop(c)
  (search (string c) *logop*)
)

(defmacro isatomic (e)
  `(isatomicj ',e)
)

(defun isatomicj (e)
  (or (atom e) (not (islogicop (car e))))
)
  
(defmacro trans1 (s) 
  `(trans1j ',s)
)

(defun trans1j (e) 
  (cond
    ((isatomicj e) e)
    (t (eval e))
  )
)

(defmacro ⇒ (a b)
  `(∨ (¬ ,a) ,b)
)

(defmacro ≡ (a b)
  `(∧ (⇒ ,a ,b) (⇒ ,a ,b))
)

(defun ¬j (e)
  e
)

(defmacro ¬ (a)
  `(¬j ,a)
)

(defmacro ∀ (v a)
  `(∀j ,v ,a)
)

(defmacro ∃ (v a)
  `(∃j ,v ,a)
)

;(defmacro ∨ (a b)
;  `(∨j ,a ,b)
;)
;
;(defmacro ∧ (a b)
;  `(∧j ,a ,b)
;)

