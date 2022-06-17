; primitive functions over wff

;;; is functions

(defun is-≡ (wff)
  (and (listp wff) (equal '≡ (car wff)))
)

(defun is-⇒ (wff)
  (and (listp wff) (equal '⇒ (car wff)))
)

(defun is-⇦ (wff)
  (and (listp wff) (equal '⇦ (car wff)))
)

(defun is-¬ (wff)
  (and (listp wff) (equal '¬ (car wff)))
)

(defun is-∨ (wff)
  (and (listp wff) (equal '∨ (car wff)))
)

(defun is-∧ (wff)
  (and (listp wff) (equal '∧ (car wff)))
)

(defun is-∀ (wff)
  (and (listp wff) (listp (car wff)) (equal '∀ (caar wff)))
)

(defun is-∃ (wff)
  (and (listp wff) (listp (car wff)) (equal '∃ (caar wff)))
)

;; maparg
;(defun map-arg (wff fn)
; ; ¬、∀、∃ => (fn (argof wff 1)) => (argof wff 1)
;
; ; other(2 args) => (fn (argof wff 1)) => (argof wff 1) and (fn (argof wff 2)) => (argof wff 2)
; (format t "not yet implemented~%")
;)

;; ?ofwff

(defun opof (wff)
  "precondition: wff is ≡、⇒、⇦、¬、∨、∧"
  (and (listp wff)(car wff))
)

(defun quantof (wff) 
  "precondition: wff is-∀ or is-∃"
  (and (listp wff) (listp (car wff))  (caar wff))
)

;; (∀ x) is correct, (∀ x y) is not.
(defun bvarof (wff) 
  "precondition: wff is-∀ or is-∃"
  (and (listp wff) (listp (car wff))  (cadar wff))
)

;;; args of 
(defun argof (wff n)
  (nth n wff)
)


;;; make functions
(defun make-≡ (&rest wffs)
  (cons '≡ wffs)
)

(defun make-⇒ (&rest wffs)
  (cons '⇒ wffs)
)

(defun make-⇦ (&rest wffs)
  (cons '⇦ wffs)
)

(defun make-¬ (&rest wffs)
  (cons '¬ wffs)
)

(defun make-∨ (&rest wffs)
  (cons '∨ wffs)
)

(defun make-∧ (&rest wffs)
  (cons '∧ wffs)
)

(defun make-∀ (v wff)
  (list (list '∀ v) wff)
)

(defun make-∃ (v wff)
  (list (list '∃ v) wff)
)

