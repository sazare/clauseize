; binding manipulations

;;; symbol generation
;;cheaplogic/lisp/rubbish/rubbish-base.lispから

;; basevar : var.1234.1222 => var
(defun basevar (v)
 (subseq (symbol-name v) 0 (position #\. (symbol-name v)))
)

;; newvar : v -> v.nnn
;; for ito, set *gensym-counter* to 0
(defparameter *varlist* ())

(defun reset-newsym ()
  (setq *varlist* ())
)

(defun newsym (v)
  (prog ( (ns (intern (string (gensym (format nil "~a." (basevar v)))))))
    (push ns *varlist*)
    (return ns)
  )
)


;; quantifier binding list
;; make
(defun put-sym-new (sym new binds)
  (cons (cons sym new) binds)
)

;; find
(defun get-sym-new (sym binds)
  (let ((entry (assoc sym binds)))
    (cond
      (entry (cdr entry))
      (t sym)
    )
  )
)

;; make a skolem function name

(defun skolem-name (sym)
  (intern (string (format nil "SKF-~a" sym)))
)

(defun skolem (fname &optional args)
  (cons (skolem-name fname) args)
)

;; make a parameter for ∃
;; binds : ((x . x.10)(y . y.11)(z . (skf-z.10 x.10 y.11)...)
(defun get-skolem-parameters (binds)
  (let (param oparam)
    (loop for b in binds
       when (and (atom (cdr b)) (not (member (car b) oparam) ))
       do  (push (cdr b) param)
           (push (car b) oparam)
    )
    (reverse param)
  )
)

(defun skolemize (sym binds)
  (skolem sym (get-skolem-parameters binds))
)

;; rename var to new in every pred(also not prop)

