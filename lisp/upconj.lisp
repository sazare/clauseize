; move up conj to top

(defun upconj (w)
  (cond
    ((is-atomic w) (literalize w))
    ((is-¬ w) (literalize w))
    ((is-∧ w) (conspread (upconj (argof w 1)) (upconj (argof w 2))))
    ((is-∨ w) (disspread (upconj (argof w 1)) (upconj (argof w 2))))
    (t w)
  )
)

(defun conspread (w1 w2)
  (cond 
    ((and (is-literal w1)(is-literal w2)) (make-∧* (list w1 w2)))
    ((and (is-∧ w1)(is-literal w2)) (make-∧* (append (argsof w1) (cons w2 nil))))
    ((and (is-literal w1)(is-∧ w2)) (make-∧* (cons w1 (argsof w2))))
    ((and (is-∧ w1)(is-∧ w2)) (make-∧* (append (argsof w1) (argsof w2))))

    ((and (is-∨ w1)(is-literal w2)) (make-∧* (list w1 w2)))
    ((and (is-literal w1)(is-∨ w2)) (make-∧* (list w1 w2)))
    ((and (is-∨ w1)(is-∨ w2)) (make-∧* (list w1 w2)))

    (t (make-∧* (list w1 w2)))
  )
)

(defun combi∨0r (ws w)
  (loop for w1 in ws collect
    (cond 
      ((is-literal w1) (make-∨* (list w1 w)))
      (t (make-∨* (append (argsof w1) (list w))))
    )
  )
)

(defun combi∨0l (w ws)
  (loop for w1 in ws collect
    (cond 
      ((is-literal w1) (make-∨* (list w w1)))
      (t  (make-∨* (cons w (argsof w1))))
    )
  )
)

(defun combi∨ (ws1 ws2)
  (loop for w1 in ws1 append
    (loop for w2 in ws2 collect
      (make-∨* (list w1 w2))
    )
  )
)

(defun disspread (w1 w2)
  (cond 
    ((and (is-literal w1)(is-literal w2)) (make-∨* (list w1 w2)))

    ((and (is-∨ w1)(is-literal w2)) (make-∨* (append (argsof w1) (list w2))))
    ((and (is-literal w1)(is-∨ w2)) (make-∨* (cons w1 (argsof w2))))
    ((and (is-∨ w1)(is-∨ w2)) (make-∨* (append (argsof w1)(argsof w2))))

    ((and (is-∧ w1)(is-literal w2)) (make-∧* (combi∨0r (argsof w1) w2)))
    ((and (is-literal w1)(is-∧ w2)) (make-∧* (combi∨0l w1 (argsof w2))))
    ((and (is-∧ w1)(is-∧ w2)) (make-∧* (combi∨ (argsof w1)(argsof  w2))))

    (t (make-∨* (list w1 w2)))
  )
)
