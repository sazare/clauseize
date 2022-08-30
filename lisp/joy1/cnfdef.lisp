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

(defun pp (x) `(pp ,x))
(defun qq (x) `(qq ,x))
(defun rr (x) `(rr ,x))
(defun ss (x) `(ss ,x))
