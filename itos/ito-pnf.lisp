;; ito of conversion to prefix normal form

(myload "ito.lisp")
(asdf:load-system :clsfy)

;; step 1
(defito ito-⇒ ()
  "convert imply"

; its ⇒ wff
  (intend-equal "convert prop ⇒" '(∨ (¬ P) Q)          (conv-imply '(⇒ P Q)))
  (intend-equal "convert pred ⇒" '(∨ (¬ (P a)) (Q a))  (conv-imply '(⇒ (P a)(Q a))))

;; ⇒ in ⇒
  (intend-equal "convert prop ⇒ in ⇒" '(∨ (¬ P) (∨ (¬ Q) R) )                           (conv-imply '(⇒ P (⇒ Q R))))
  (intend-equal "convert pred ⇒ in ⇒" '(∨ (¬ (∨ (¬ (P a)) (Q a))) (∨ (¬ (R a)) (Q b)))  (conv-imply '(⇒ (⇒ (P a)(Q a)) (⇒ (R a)(Q b)))))

  ;¬ and ⇒
  (intend-equal "convert prop ⇒ in ⇒" '(∨ (¬ (¬ P)) (∨ (¬ Q) R))                            (conv-imply '(⇒ (¬ P) (⇒ Q R))))
  (intend-equal "convert pred ⇒ in ⇒" '(∨ (¬ (¬ (∨ (¬ (P a)) (Q a)))) (∨ (¬ (R a)) (Q b)))  (conv-imply '(⇒ (¬ (⇒ (P a)(Q a))) (⇒ (R a)(Q b)))))
)


(defito ito-≡ ()
  "convert equivalent"
; its ≡ wff
  (intend-equal "convert ≡" '(∧  (∨ (¬ P) Q) (∨ P (¬ Q)))                  (conv-imply '(≡ P Q)))
  (intend-equal "convert ≡" '(∧  (∨ (¬ (P a)) (Q a)) (∨ (P a) (¬ (Q a)) )) (conv-imply '(≡ (P a)(Q a))))

;; ≡ inside of a wff
  (intend-equal "convert ⇒in≡≡" '(∧ (∨ (¬ (∨ (¬ P) (¬ R)))(∨ S (¬ Q))) (∨ (∨ (¬ P) (¬ R))(¬ (∨ S (¬ Q)))))
                                 (conv-imply '(≡ (⇒ P (¬ R))(∨ S (¬ Q)))))

)

(defito ito-mix ()
  "convert mixform"
  (intend-equal "convert ⇒in≡" '(∧ (∨ (¬ (¬ (∨ (¬ P) R))) (¬ Q)) (∨ (¬ (∨ (¬ P) R)) (¬ (¬ Q)))) 
                                (conv-imply '(≡ (¬ (⇒ P R)) (¬ Q))))

  (intend-equal "convert ⇒⇒in≡" '(∧ (∨ (¬ (¬ (∨ (¬ (P a)) (R b)))) (∨ (¬ (Q a)) (¬ (S a))))
                                (∨ (¬ (∨ (¬ (P a)) (R b))) (¬ (∨ (¬ (Q a)) (¬ (S a)))) ))
                                (conv-imply '(≡ (¬ (⇒ (P a) (R b))) (⇒ (Q a) (¬ (S a))))))

)

;; step2 
(defito ito-neginto ()
  "not neg case. in neginto"
  (intend-equal "top is ∨" '(∨ (¬ (P x))(Q x)) (conv-neginto '(∨ (¬ (P x))(Q x))))
  (intend-equal "top is ∧" '(∧ (¬ (P x))(Q x)) (conv-neginto '(∧ (¬ (P x))(Q x))))
  (intend-equal "top is ∀" '((∀ x)(¬ (P x))) (conv-neginto '((∀ x)(¬ (P x)))))
  (intend-equal "top is ∃" '((∃ x)(¬ (P x))) (conv-neginto '((∃ x)(¬ (P x)))))
)

(defito ito-neginto1 ()
  "neg case in neginto"
; case (¬ (op d)) を (op' (¬ d))にする
; op   ∧ ∨ ∀ ∃
; op'  ∨ ∧ ∃ ∀

  (intend-equal "du Morgan ¬(A∨B) => ¬A∧¬B"  '(∧ (¬ A)(¬ B)) (conv-neginto '(¬ (∨ A B))))
  (intend-equal "du Morgan ¬(A∧B) => ¬A∨¬B"  '(∨ (¬ A)(¬ B)) (conv-neginto '(¬ (∧ A B))))

  (intend-equal "du Morgan ¬(Ax∨By) => ¬Ax∧¬By"  '(∧ (¬ (A x))(¬ (B y))) (conv-neginto '(¬ (∨ (A x) (B y)))))
  (intend-equal "du Morgan ¬(Ax∧By) => ¬Ax∨¬By"  '(∨ (¬ (A x))(¬ (B y))) (conv-neginto '(¬ (∧ (A x) (B y)))))

  (intend-equal "du Morgan ¬A => ¬A"  '(¬ A) (conv-neginto '(¬ A)))
  (intend-equal "du Morgan ¬¬A => A"  'A (conv-neginto '(¬ (¬ A))))
  (intend-equal "du Morgan ¬¬¬A => ¬A"  '(¬ A) (conv-neginto '(¬ (¬ (¬ A)))))
  (intend-equal "du Morgan ¬¬¬¬AA => A"  'A (conv-neginto '(¬ (¬ (¬ (¬ A))))))

; pred

  (intend-equal "du Morgan pred ¬(Ax∨Bx) => ¬Ax∧¬Bx"  '(∧ (¬ (A x))(¬ (B x))) (conv-neginto '(¬ (∨ (A x) (B x)))))
  (intend-equal "du Morgan pred ¬(Ax∧Bx) => ¬Ax∨¬Bx"  '(∨ (¬ (A x))(¬ (B x))) (conv-neginto '(¬ (∧ (A x) (B x)))))

  (intend-equal "du Morgan pred ¬A => ¬A"  '(¬ (A x y)) (conv-neginto '(¬ (A x y))))
  (intend-equal "du Morgan pred ¬¬A => A"  '(A x y) (conv-neginto '(¬ (¬ (A x y)))))
  (intend-equal "du Morgan pred ¬¬¬A => ¬A"  '(¬ (A x y)) (conv-neginto '(¬ (¬ (¬ (A x y))))))
  (intend-equal "du Morgan pred ¬¬¬¬AA => A"  '(A x y) (conv-neginto '(¬ (¬ (¬ (¬ (A x y)))))))

;auanti

  (intend-equal "du Morgan ¬(∀xAx) => ∃x¬Ax"  '((∃ x) (¬ (A x))) (conv-neginto '(¬ ((∀ x) (A x)))))
  (intend-equal "du Morgan ¬(∀xAx) => ∃x¬Ax"  '((∃ x) (¬ (A x))) (conv-neginto '(¬ ((∀ x) (A x)))))

  (intend-equal "du Morgan ¬(∃xAx) => ¬∀xAx"  '((∀ x) (¬ (A x))) (conv-neginto '(¬ ((∃ x) (A x) ))))
  (intend-equal "du Morgan ¬(∃xAx) => ¬∀xAx"  '((∀ x) (¬ (A x))) (conv-neginto '(¬ ((∃ x) (A x) ))))

; ¬and¬not nexted
  (intend-equal "du Morgan complex 1"
                           '((∀ x)(∨(∨ (P x)(Q x))(∧ (¬(R x))((∀ y)(¬(S y))))))
                           (conv-neginto '((∀ x)(¬ (∧(∧ (¬ (P x))(¬ (Q x)))(∨ (R x)(¬(¬((∃ y)(S y))))))))))
  
)


(defito ito-quantifier ()
  "quantifier"
;; step3

  (intend-equal "no quantifire identical" 
           '(v (¬ (P a)) (∨ (∧ (Q b c)(¬ (R a c))) (P b))) 
            (rename-bv '(v (¬ (P a)) (∨ (∧ (Q b c)(¬ (R a c))) (P b))) ))


;; variable disjoint
  (setq *gensym-counter* 1)
  (intend-equal "all vars are changed" 
                          '((∀ x.1)(∨ (P x.1)((∀ y.2)(Q y.2))))
                          (rename-bv '((∀ x)(∨ (P x)((∀ y)(Q y))))))

;; skolemize
  (setq *gensym-counter* 1)
  (intend-equal "all vars are changed" 
                          '((∀ x.1)(∨ (P x.1)((∀ y.2)(Q y.2))))
                          (rename-bv '((∀ x)(∨ (P x)((∀ y)(Q y))))))

;; move quantifiers to prefix
;; final step
;; make (¬ P) to (-P) or
;; make (¬ (P x)) to (-P x)

)

(defun ito-pnf ()
 ; step1
  (ito-⇒)
  (ito-≡)
  (ito-mix)
 ; step2
  (ito-neginto)
  (ito-neginto1)

 ; step3
  (ito-quantifier)
)

(ito-pnf)


