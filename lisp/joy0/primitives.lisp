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

;; logical

(defun is-atomic (w)
  (and (not (is-∨ w))(not (is-∧ w))(not (is-∀ w))(not (is-∃ w)))
)

(defun is-literal (w)
  (or (eq '- (car w))(eq '+ (car w)))
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

(defun argsof (wff)
  (cdr wff)
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

(defun remove-ops (wffs)
  (loop for w in wffs collect
    (argsof w)
  )
)

(defun make-∨* (wffs)
  (cons '∨ wffs)
)

(defun make-∧* (wffs)
  (cons '∧ wffs)
)

(defun allop? (op wffs)
  (loop for w in (argsof wffs) always (eq (opof w) op))
)

(defun shrink1 (op wffs)
  (cond
    ((allop? op wffs) (cons op (loop for w in wffs collect (argsof w))))
    (t wffs)
  )
)


(defun ∨in∨ (wff)
  (and (eq '∨ (opof wff)) (length (argsof wff)) (eq '∨ (opof (argof wff 1))))
)

(defun shrink∨ (wff)
 "∨∨ is ∨"
  (cond
    ((∨in∨ wff) (argof wff 1))
    (t wff)
  )
)

