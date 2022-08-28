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


