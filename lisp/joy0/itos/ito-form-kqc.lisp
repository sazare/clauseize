;; ito of form-kqc
;; format upjoin to clauses

(myload "ito.lisp")
(asdf:load-system :clsfy)

(defparameter cff nil)

(defito ito-form-kqc()
  "convert the wff of upjoin to kqc form"

;(defparameter *cn* 0)
;  (intend-equal "a literal is a unit clause prop" '(1 () (+ P)) (form-kqc () (clsfy 'P)))
;
;(defparameter *cn* 0)
;  (intend-equal "a literal is a unit clause" '(1 ()  (+ P a b)) (form-kqc () (clsfy '(P a b))))
;
;(defparameter *cn* 0)
;  (intend-equal "∨ clause is a clause prop" '(1 () (+ P)(- R)(+ Q)) (form-kqc () (clsfy '(∨ P (∨ (¬ R) Q)))))
;
;(defparameter *cn* 0)
;  (intend-equal "∨ clause is a clause" '(1 () (+ P a)(- R b)(+ Q c)) (form-kqc  () (clsfy '(∨ (∨ (P a)(¬(R b)))(Q c)))))
;
;(defparameter *cn* 0)
;  (intend-equal "∧ clause is clauses set  prop" '((1 () (+ P)(- R))(2 () (+ Q) (+ S))) (form-kqc *local-vars* (clsfy '(∧ (∨ P (¬ R))(∨ Q S)))))
;
;(defparameter *cn* 0)
;  (intend-equal "∧ clause is clauses set" '((1 () (+ P a)(- R b)) (2 () (+ Q c)(- S d))) (form-kqc  *local-vars* (clsfy '(∧ (∨ (P a) (¬ (R b))) (∨ (Q c) (¬ (S d)))))))
;
;  (reset-vars)
;  (setq *gensym-counter* 1)
;(defparameter *cn* 0)
;  (setq cff (clsfy '(∧ ((∀ x)(∨ (P x) (¬ (R x)))) ((∀ x)(∨ (Q x) (¬ (S x)))))))
;  (intend-equal "∧ clause is clauses set" '((1 (x.2 x.1) (+ P x.1)(- R x.1)) (2 (x.2 x.1) (+ Q x.2)(- S x.2))) (form-kqc *local-vars* cff))
)

(defito ito-flatten ()
  "ito for flattern"
  (intend-equal "flatten literal" '(((+ P))) (flatten '(+ P)))
  (intend-equal "flatten ∨"       '(((- P))) (flatten '(∨(- P))))
  (intend-equal "flatten ∨ab"     '(((- P a))) (flatten '(∨(- P a))))
  (intend-equal "flatten ∨ab"     '(((- P a)(+ Q a))) (flatten '(∧(∨(- P a)(+ Q a)))))

  (reset-cn)
  (intend-equal "clausefy"     '((1 nil (- P a)(+ Q a))(2 nil (+ S a b c))) (clausefy '(∧(∨(¬ (P a))(Q a))(∨ (S a b c)))))
  (reset-cn)
  (intend-equal "clausefy"     '((1 (a.1)(- P a.1)(+ Q a.1))(2 (a.1) (+ S a.1 b c))) (clausefy '((∀ a)(∧(∨(¬ (P a))(Q a))(∨ (S a b c))))))
)

 


(defun ito-form-kqc-all ()
  (ito-form-kqc)
  (ito-flatten)
)

(ito-form-kqc-all)

