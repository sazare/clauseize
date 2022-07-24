; move up conj to top

(defun upconj-before* (ws)
  (loop for w in ws collect
    (upconj-before w)
  )
)

(defun upconj-before (w)
  (cond
    ((is-atomic w) (literalize w))
    ((is-¬ w)      (literalize w))
    ((is-∧ w)      (make-∧* (upconj-before* (argsof w))))
    ((is-∨ w)      (make-∨* (upconj-before* (argsof w))))
    (t (format t "~a is not a wff in upconj-before~%" w))
  )
)
(defun upconj (w)
  (upconj-after (upconj-before w))
)

(defun upconj* (ws)
  (loop for w in ws collect 
    (upconj w)
  )
)

(defun upconj-after* (ws)
  (loop for w in ws collect
    (upconj-after w)
  )
)

(defun upconj-after (befored)
  (let (children) 
    (cond 
      ((is-literal befored) befored)
      (t
        (let ()
          (setq children (upconj-after* (argsof befored)))
          (cond
            (t  
              (let (conjsd disjsd litsd)
                (multiple-value-setq (conjsd disjsd litsd) (div2∧∨ children))
                (cond 
                  ((and (null conjsd)(null disjsd)) befored)
                  ((is-∧ befored) (conjspread conjsd disjsd litsd))
                  ((is-∨ befored) (disjspread conjsd disjsd litsd))
                  (t (format t "~a is not a wff in upconj~%" befored) )
                )
              )
            )
          )
        )
      )
    )
  )
)


(defun div2∧∨ (ws)
  (let (conjs disjs lits)
    (loop for w in ws do
      (cond 
        ((is-∧ w) (push w conjs))
        ((is-∨ w) (push w disjs))
        ((is-literal w) (push w lits))
        (t (format t "~a is not wff in div2∧∨~%" w))
      )
    )
    (values (reverse conjs)(reverse disjs)(reverse lits))
  )
)

(defun cutbranch (ws)
  (loop for w in ws append
    (argsof w)
  )
)

(defun flattern-∧ (as)
  (loop for a in as append
    (argsof a)
  )
)

(defun conjspread (conjsd disjsd litsd)
  (cond
    ((and (null conjsd)(null disjsd)(null litsd)) nil)
    ((and (null disjsd)(null litsd))  (make-∧* conjsd))
    ((and (null conjsd)(null disjsd)) (make-∧* litsd))
;    ((and (null conjsd)(null disjsd)) (make-∧* (make-each-∨* litsd)))
    ((and (null conjsd)(null litsd))  (make-∧* disjsd))
    ((null conjsd) (make-∧* (append disjsd (make-each-∨* litsd))))
    ((null disjsd) (make-∧* (append (flattern-∧ conjsd) litsd)))
;    ((null disjsd) (make-∧* (append (flattern-∧ conjsd) (make-each-∨* litsd))))
    ((null litsd)  (make-∧* (append (flattern-∧ conjsd) disjsd)))
    (t (make-∧* (append conjsd disjsd litsd)))
;    (t (make-∧* (append conjsd disjsd (make-each-∨* litsd))))
  )
)

(defun cross (ls1 ls2)
  (loop for al1 in ls1 append
    (loop for al2 in ls2 collect
      (append al1 (list al2))
    )
  )
)

(defun fatten (cs)
  (loop for c in cs collect
    (list c)
  )
)

(defun prod-conj (css)
  (if (eq (length css) 1) 
    (fatten (argsof (car css)))
    (let ((ps (fatten (argsof (car css)))))
        (loop for c in (cdr css) collect
          (setf ps (cross ps (argsof c)))
        )
      ps
    )
  )
)

(defun add-disj (css ds)
  "never ds be null"
  (loop for cs in css collect
    (make-∨* (append cs ds))
  )
)

(defun make-each-∨* (dll)
  (loop for dl in dll collect 
    (make-∨* (list dl)) ; dl is a wff
  )
)

(defun put-each-∨* (dll)
  (loop for dl in dll collect 
    (make-∨* dl) ; dl is a list of wff
  )
)

(defun make-each-∧* (dll)
  (loop for dl in dll collect 
    (make-∧* (list dl)) ; dl is a list of wff
  )
)

(defun put-each-∧* (dll)
  (loop for dl in dll collect 
    (make-∧* dl) ; dl is a list of wff
  )
)

(defun demorgan (conjsed disjsd) 
  (cond
    ((null disjsd) (put-each-∨* (prod-conj conjsed)))
;    ((null disjsd) (make-each-∨* (prod-conj conjsed)))
    (t (add-disj (prod-conj conjsed) disjsd))
  )
)

(defun disjspread (conjsd disjsd litsd)
  (cond
    ((and (null conjsd)(null disjsd)(null litsd)) nil)
    ((and (null disjsd)(null litsd))  (make-∧* (demorgan conjsd nil)))
    ((and (null conjsd)(null disjsd)) (make-∨* litsd))
    ((and (null conjsd)(null litsd))  (make-∨* disjsd))
    ((null conjsd) (make-∨* (append disjsd litsd)))
    ((null disjsd) (make-∧* (demorgan conjsd litsd)))
    ((null litsd)  (make-∧* (demorgan conjsd disjsd)))
    (t (make-∧* (demorgan conjsd (append disjsd litsd))))
  )
)



