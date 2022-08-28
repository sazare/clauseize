; 1st step of cnf tran

;; def for symbols
;; for these defs, ',a needless. and the recursive application is effective.

(defvar x 'x)
(defvar y 'y)
(defvar a 'a)
(defvar b 'b)
(defvar c 'c)

(defvar p 'p)
(defvar q 'q)
(defvar r 'r)
(defvar s 's)


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


;(defmacro ≡ (a b)
;  `(∧ (∨ (¬ ,a) ,b)(∨ ,a (¬ ,b)))
;)

(defmacro ≡ (a b)
  `(∧ (⇒ ,a ,b) (⇒ ,b ,a))
)

(defmacro ⇒ (a b)
  `(∨ (¬ ,a) ,b)
)


;; macro dont recursive applied?
;; but don't use ' and symbol's value is itself, seems allright

(⇒ (⇒ p q) r)

;CL-USER(2): (⇒ (⇒ p q) r)
;
;(∨ (¬ (⇒ P Q)) R)
;CL-USER(3): (≡ (≡ p q) r)
;
;(∧ (∨ (¬ (≡ P Q)) R) (∨ (≡ P Q) (¬ R)))

;; alternative question
;;; how to write recursive in macro? can i?


