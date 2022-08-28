
(defun mm1 (x y) `(∨ ,x ,y))


;;; something like this, but not this

(defun nop (w) 
  (let 
    ((¬ (lambda (w1) (cdr w1)))
     (∨ (lambda (&rest ws) `(∨ ,@ws)))
     (∧ (lambda (&rest ws) `(∧ ,@ws)))
     (∀ (lambda (vs w1) `(∀ ,vs ,w1)))
     (∃ (lambda (vs w1) `(∃ ,vs ,w1)))
    )
  (eval w)
  )
)

(defun ¬ (w) 
  (let 
    ((¬ (lambda (w1) w1))
     (∨ (lambda (&rest ws) `(∧ ,@ws)))
     (∧ (lambda (&rest ws) `(∨ ,@ws)))
     (∀ (lambda (vs w1) `(∃ ,vs ,w1)))
     (∃ (lambda (vs w1) `(∀ ,vs ,w1)))
    )
  (eval w)
  )
)


(defun ∧ (&rest ws) 
  `(∧ ,@ws)
)

(defun ∨ (&rest ws)
  `(∨ ,@ws)
)

(defun ∀ (vs w)
  `(∀ ,ws ,w)
)

(defun ∃ (vs w)
  `(∃ ,vs w)
)


