; primitive functions over wff

;;; is functions

(defun is≡ (wff)
  (and (listp wff) (eq '≡ (car wff)))
)

(defun is⇒ (wff)
  (and (listp wff) (eq '⇒ (car wff)))
)

(defun is⇦ (wff)
  (and (listp wff) (eq '⇦ (car wff)))
)

(defun is¬ (wff)
  (and (listp wff) (eq '¬ (car wff)))
)

(defun is∨ (wff)
  (and (listp wff) (eq '∨ (car wff)))
)

(defun is∧ (wff)
  (and (listp wff) (eq '∧ (car wff)))
)

(defun is∀ (wff)
  (and (listp wff) (eq 3 (length wff)) (eq '∀ (car wff)))
)

(defun is∃ (wff)
  (and (listp wff) (eq 3 (length wff)) (eq '∃ (car wff)))
)

;; logical
;; after ⇒ conversion, before inner¬
(defun isatomic (w)
  (and (not (is∨ w))(not (is∧ w))(not (is∀ w))(not (is∃ w)))
)

;; after inner¬
(defun isliteral (w)
  (or (eq '- (car w))(eq '+ (car w)))
)


;; ?ofwff

(defun opof (wff)
  "precondition: wff is ≡、⇒、⇦、¬、∨、∧, ∀, ∃"
  (and (listp wff)(car wff))
)

;;; for quantified wff 
;; (∀ x α) or (∀ (x y) α) are correct, (∀ β) or (∀ x y β)  is not.
(defun varsof (wff) 
  "precondition: wff is∀ or is∃. after is∀ or is∃"
  (cadr wff)
)

(defun qargof (wff)
  "precondition: wff is∀ or is∃"
  (nth 2 wff)
)


;;;; not quantified form of wff
;;; args of 
(defun argof (wff n)
  (nth n wff)
)

(defun argsof (wff)
  (cdr wff)
)

;;; make functions
(defun make≡ (&rest wffs)
  (cons '≡ wffs)
)

(defun make⇒ (&rest wffs)
  (cons '⇒ wffs)
)

(defun make⇦ (&rest wffs)
  (cons '⇦ wffs)
)

(defun make¬ (&rest wffs)
  (list '¬ wffs)
)

(defun make∨ (&rest wffs)
  (cons '∨ wffs)
)

(defun make∧ (&rest wffs)
  (cons '∧ wffs)
)

(defun make∀ (v wff)
  (list '∀ v  wff)
)

(defun make∃ (v wff)
  (list '∃ v  wff)
)


(defun make∨* (wffs)
  (cons '∨ wffs)
)

(defun make∧* (wffs)
  (cons '∧ wffs)
)


