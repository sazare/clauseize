;; pnfy convert wff to clause

(defparameter *local-vars* nil)

(defun reset-vars () 
  (setf *local-vars* nil)
)


(defun extract-vars* (wffs)
  (loop for wff in wffs collect
    (extract-vars wff)
  )
)


(defun extract-vars (wff)
  (cond
    ((is-¬ wff) (make-¬ (extract-vars (argof wff 1))))
    ((is-∨ wff) (make-∨* (extract-vars* (argsof wff))))
    ((is-∧ wff) (make-∧* (extract-vars* (argsof wff))))

    ((is-∀ wff) (pushnew (bvarof wff) *local-vars*) (extract-vars (argof wff 1)))
    ((is-∃ wff) (extract-vars (argof wff 1))) 
    (t wff)
  )
)


(defun clsfy (wff)
  (let (w)
    (reset-vars)
    (setq w (upconj (extract-vars (rename-bv (conv-neginto (conv-imply wff))))))
    (values w *local-vars*)
  )
)
