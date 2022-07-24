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

  (intend-equal "top is ∨" '(∨ (¬ (P x))(R x)(Q x)) (conv-neginto '(∨ (¬ (P x))(R x)(Q x))))
  (intend-equal "top is ∧" '(∧ (¬ (P x))(R x)(Q x)) (conv-neginto '(∧ (¬ (P x))(R x)(Q x))))

  (intend-equal "top is ∀" '((∀ x)(∧ (Q x)(S x)(¬ (P x)))) (conv-neginto '((∀ x)(∧ (Q x)(S x) (¬ (P x))))))
  (intend-equal "top is ∃" '((∃ x)(∨ (Q x)(S x)(¬ (P x)))) (conv-neginto '((∃ x)(∨ (Q x)(S x) (¬ (P x))))))
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
  (intend-equal "all vars are changed" '((∀ x.1)(∨ (P x.1)((∀ y.2)(Q y.2)))) (rename-bv '((∀ x)(∨ (P x)((∀ y)(Q y))))))

;; skolemize
  (setq *gensym-counter* 1)
  (intend-equal "all vars are changed" '((∀ x.1)(∨ (P x.1)((∀ y.2)(Q y.2)))) (rename-bv '((∀ x)(∨ (P x)((∀ y)(Q y))))))

)

(defito ito-removeq()
  "ito for removeq"

;; **** at this point, removed ∃, but wff form keeped

;; move quantifiers to prefix
;; 1. remove ∀
;;;   and add vars to clause at last

  (setq *gensym-counter* 1)
  (reset-newsym)
  (intend-equal "remove ∀" '(∨ (P x)(Q y)) (remove-∀ '((∀ x)(∨ (P x)((∀ y)(Q y))))))
  (intend-equal "remove ∀" '(∨ (P x y)(∧ (Q x y)(¬ (P x z)))) (remove-∀ '((∀ x)(∨ ((∀ y)(P x y))(∧ ((∀ y)(Q x y))((∀ z)(¬ (P x z))))))))
)

(defito ito-literalize()
  "ito for literalize"

;; literalize
;; 3 literallize
;; make (¬ P) to (- P) and P to (+ P)
;; make (¬ (P x)) to (- P x) and (P x) to (+ P x)

  (intend-equal "¬ to - prop" '(- P) (literalize '(¬ P)))
  (intend-equal "none to + prop" '(+ P) (literalize 'P))

  (intend-equal "¬ to -" '(- P a b) (literalize '(¬ (P a b))))
  (intend-equal "none to +" '(+ P a b)(literalize '(P a b)))
)

(defito ito-div2∧∨ ()
  "ito for div2∧∨"
  (intend-equal "1and 2or" '(((∧ P Q)) ((∨ A)(∨ B)) nil) (multiple-value-list (div2∧∨ '((∧ P Q)(∨ A)(∨ B)))))
  (intend-equal "2and 2or" '(((∧ R U)(∧ P Q)) ((∨ A)(∨ B)) nil) (multiple-value-list (div2∧∨ '((∧ R U)(∨ A) (∧ P Q)(∨ B)))))
  (intend-equal "lit " '(() ((∨ (R U))(∨ A)) nil) (multiple-value-list (div2∧∨ '((∨ (R U)) (∨ A)))))
  (intend-equal "3types" '(((∧ (R n)(S n)))((∨ (B y)(A x))(∨ (N r))) nil) (multiple-value-list (div2∧∨ '((∧ (R n)(S n))(∨ (B y)(A x))(∨ (N r))))))

  (intend-equal "no and" '(() ((∨ A)(∨ B)(∨ C))nil) (multiple-value-list (div2∧∨ '((∨ A) (∨ B) (∨ C)))))
  (intend-equal "no disj" '(((∧ R U)(∧ A B)(∧ P Q)(∧ AA BB)) ()nil) (multiple-value-list (div2∧∨ '((∧ R U)(∧ A B) (∧ P Q)(∧ AA BB)))))
)


(defito ito-prod()
  "ito for prodconj "
  (intend-equal "prod 3 conj"   '(((+ P)(- Q)(+ R))) (prod-conj '(((+ P)(- Q)(+ R)))))
  (intend-equal "prod 1x3 conj" '(((+ S)(+ P)) (∨ (+ S)(+ Q)) (∨ (+ S)(+ R))) (prod-conj '(((+ S))((+ P)(- Q)(+ R)))))
  (intend-equal "prod 2x3 conj" '(((+ S)(+ P)) (∨ (+ S)(+ Q)) (∨ (+ S)(+ R))(∨ (+ V)(+ P)) (∨ (+ V)(+ Q)) (∨ (+ V)(+ R)))
               (prod-conj '(((+ S)(- V))((+ P)(- Q)(+ R)))))

  (intend-equal "prod 2x3x2 conj" '( 
    (((+ P1)(- P3)(+ P6))((+ P1)(- P3)(- P7)))
    (((+ P1)(+ P4)(+ P6))((+ P1)(+ P4)(- P7)))
    (((+ P1)(- P5)(+ P6))((- P1)(- P5)(- P7)))
    (((- P2)(- P3)(+ P6))((- P2)(- P3)(- P7)))
    (((- P2)(+ P4)(+ P6))((- P2)(+ P4)(- P7)))
    (((- P2)(- P5)(+ P6))((- P2)(- P5)(- P7))))
    (prod-conj '(((+ P1)(- P2))((- P3)(+ P4)(- P5))((+ P6)(- P7)))))

)

(defito ito-upconj()
  "ito for upconj"
;; 2. ∧∨ 2 layers
;;  → a set of clauses; a list of clauses
; unit clauses
  (intend-equal "just literal pos prop" '(+ P) (upconj 'P))
  (intend-equal "just literal neg prop" '(- P) (upconj '(¬ P)))

  (intend-equal "just literal pos" '(+ P a) (upconj '(P a)))
  (intend-equal "just literal neg" '(- P a) (upconj '(¬ (P a))))

  (intend-equal "(∧ a b)" '(∧ (+ a)(+ b)) (upconj '(∧ a b)))
  (intend-equal "(∧ a -b)" '(∧ (+ a)(- b)) (upconj '(∧ a (¬ b))))
  (intend-equal "(∧(∧ a b))" '(∧ (∧ (+ a)(+ b))) (upconj '(∧ (∧ a b))))
  (intend-equal "(∧(∨ a b))" '(∧ (∨ (+ a)(+ b))) (upconj '(∧ (∨ a b))))
  (intend-equal "(∧(∨ a b) c d)" '(∧ (∨ (+ a)(+ b))(+ C)(+ D)) (upconj '(∧ (∨ a b) c d)))
  (intend-equal "(∧(∨ a b)(∨ c d))" '(∧ (∨ (+ a)(+ b))(∨ (+ C)(+ D))) (upconj '(∧ (∨ a b)(∨ c d))))
  (intend-equal "(∧(∨ a b)(∨ c d) e f)" '(∧ (∨ (+ a)(+ b))(∨ (+ C)(+ D))(+ e)(+ f)) (upconj '(∧ (∨ a b)(∨ c d) e f)))

  (intend-equal "(∨ a b)" '(∨ (+ a)(+ b)) (upconj '(∨ a b)))
  (intend-equal "(∨ (∧ a b))" '(∧ (+ a)(+ b)) (upconj '(∨ (∧ a b))))
  (intend-equal "(∨ (∧ a -b))" '(∧ (+ a)(- b)) (upconj '(∨ (∧ a (¬ b)))))
  (intend-equal "(∨ (∨ a b))" '(∨ (+ a)(+ b)) (upconj '(∨ (∨ a b))))
  (intend-equal "(∨ (∨ a) c d)" '(∨ (+ a)(+ b)(+ c)(+ d)) (upconj '(∨ (∨ a b) c d)))
  (intend-equal "(∨ (∧ a b) c d)" '(∧ (∨ (+ a)(+ c)(+ d))(∨ (+ b)(+ c)(+ d))) (upconj '(∨ (∧ a b) c d)))
  (intend-equal "(∨ (∧ a b)(∨ c d))" '(∧ (∨ (+ a)(+ c)(+ d))(∨ (+ b)(+ c)(+ d))) (upconj '(∨ (∧ a b)(∨ c d))))
  (intend-equal "(∨ (∨ a b)(∨ c d))" '(∨ (+ a)(+ b)(+ c)(+ d)) (upconj '(∨ (∨ a b)(∨ c d))))
  (intend-equal "(∨ (∧ a b)(∨ c d) e f)" '(∧ (∨ (+ a)(+ c)(+ d)(+ e)(+ f))(∨ (+ b)(+ c)(+ d)(+ e)(+ f))) (upconj '(∨ (∧ a b)(∨ c d) e f)))
  (intend-equal "(∨ (∧ a b)(∧ c d))" '(∧ (∨ (+ a)(+ c))(∨ (+ a)(+ d))(∨ (+ b)(+ c))(∨ (+ b)(+ d))) (upconj '(∨ (∧ a b)(∧ c d))))
  (intend-equal "(∨ (∧ a b)(∧ c d)(∨ e f) g h)" '(∧ (∨ (+ a)(+ c)(+ e)(+ f)(+ g)(+ h))(∨ (+ a)(+ d)(+ e)(+ f)(+ g)(+ h))(∨ (+ b)(+ c)(+ e)(+ f)(+ g)(+ h))(∨ (+ b)(+ d)(+ e)(+ f)(+ g)(+ h))) (upconj '(∨ (∧ a b)(∧ c d)(∨ e f) g h)))

  (intend-equal "(a ∧ b) ∨ (c ∧ d) "  '(∧ (∨ (+ P a)(- R a)) (∨ (+ P a)(+ Q a)) (∨ (+ S a)(- R a)) (∨ (+ S a)(+ Q a)))  
                 (upconj '(∨ (∧ (P a) (S a)) (∧ (¬ (R a))(Q a)))))

  (intend-equal "a ∧ (b ∧ c)  prop" '(∧(- R)(+ Q)(+ P)) (upconj '(∧ P (∧ (¬ R) Q))))
  (intend-equal "a ∧ (b ∧ R) " '(∧ (- R a)(+ Q a)(+ P a)) (upconj '(∧ (P a) (∧ (¬ (R a))( Q a)))))

  (intend-equal "((a ∧ b) ∧ c)  prop" '(∧ (- R)(+ Q)(+ P)) (upconj '(∧ (∧ P (¬ R)) Q)))
  (intend-equal "((a ∧ b) ∧ R) " '(∧ (- R a)(+ Q a)(+ P a)) (upconj '(∧ (∧ (P a) (¬ (R a)))(Q a))))

  (intend-equal "(a ∨ b) ∧ c  prop" '(∧ (- R)(+ Q)(+ P)) (upconj '(∧ (∨ P (¬ R)) Q)))
  (intend-equal "(a ∨ b) ∧ c "  '(∧ (∨ (- R a)(+ Q a)(+ P a))) (upconj '(∧ (∨ (P a) (¬ (R a)))(Q a))))

  (intend-equal "((a ∨ b) ∧ c) prop" '(∧ (∨ (+ P) (- R)) (+ Q)) (upconj '(∧ (∨ P (¬ R)) Q)))
  (intend-equal "((a ∨ b) ∧ R) pred"  '(∧ (∨ (- R a) (+ Q a)(+ P a))) (upconj '(∧ (∨ (P a) (¬ (R a)))(Q a))))

  (intend-equal "a ∨ (b ∧ c)  prop" '(∧ (- R) (+ Q)(+ P)) (upconj '(∨ P (∧ (¬ R) Q))))
  (intend-equal "a ∨ (b ∧ R) "  '(∧ (- R a)(+ P a)(+ Q a)) (upconj '(∨ (P a) (∧ (¬ (R a))(Q a)))))

;  (intend-equal "∧∨∧∨ deep" '(∧(∨ (+ a)(+ c))(∨ (+ a)(+ d))(∨ (- b)(+ c))(∨ (- b)(+ d))(∨ (+ e)(+ f) (+ h))(∨ (+ g)(+ h))) (upconj '(∧ (∨ (∧ a (¬ b))(∧ c d)) (∨ (∧(∨ e f) g) h))))

;  (intend-equal "full pattern" '(∧ (∨ (+ P) (+ R) (+ A) (+ B) (+ C) (+ D) (+ E) (+ F)) (∨ (+ P) (+ S) (+ A) (+ B) (+ C) (+ D) (+ E) (+ F)) (∨ (+ Q) (+ R) (+ A) (+ B) (+ C) (+ D) (+ E) (+ F)) (∨ (+ Q) (+ S) (+ A) (+ B) (+ C) (+ D) (+ E) (+ F))) (upconj '(∨ (∧ P Q)(∧ R S)(∨ A B)(∨ C D) E F)))

  (intend-equal "a ∨ (b ∨ R) " '(∨ (+ P a)(- R a)(+ Q a)) (upconj '(∨ (P a) (¬ (R a))(Q a))))
  (intend-equal "a ∨ b" '(∨ (+ P a)(- R a)) (upconj '(∨ (P a) (¬ (R a))))) 
  (intend-equal "a ∨ (b ∨ R) " '(∨(+ P a)(- R a)(+ Q a)) (upconj '(∨ (P a) (∨ (¬ (R a))( Q a)))))
  (intend-equal "(a ∨ b) ∨ R " '(∨(+ P a)(- R a)(+ Q a)) (upconj '(∨ (∨ (P a) (¬ (R a)))(Q a))))
  (intend-equal "(a ∨ b) ∧ (c ∨ d) " '(∧ (∨ (- P a)(+ S b))(∨(- R a)(+ Q a))) (upconj '(∧ (∨ (¬ (P a)) (S b)) (∨ (¬ (R a)) (Q a)))))
  (intend-equal "(a ∨ b) ∨ (c ∨ d) " '(∨ (- P a)(+ S b)(- R a)(+ Q a)) (upconj '(∨ (∨ (¬ (P a)) (S b)) (∨ (¬ (R a)) (Q a)))))
)

(defito ito-clsfy ()
  "ito for clsfy"
  (intend-equal "P  prop" '(+ P) (clsfy 'P))
  (intend-equal "-P prop" '(- P) (clsfy '(¬ P)))
  (intend-equal "(P a)" '(+ P a) (clsfy '(P a)))
  (intend-equal "(¬ (P a))" '(- P a) (clsfy '(¬ (P a))))

  (intend-equal "double neg (¬(¬ P )) prop" '(+ P a) (clsfy '(¬ (¬ (P a)))))
  (intend-equal "double neg (¬(¬ (P a))) " '(+ P a) (clsfy '(¬ (¬ (P a)))))
  (intend-equal "3 neg (¬(¬(¬ P ))) prop" '(- P a) (clsfy '(¬(¬ (¬ (P a))))))
  (intend-equal "3 neg (¬(¬(¬ (P a)))) " '(- P a) (clsfy '(¬(¬ (¬ (P a))))))

  (intend-equal "imply prop" '(∨ (- P) (+ Q)) (clsfy '(⇒ P Q)))
  (intend-equal "imply" '(∨ (- P a) (+ Q a)) (clsfy '(⇒ (P a)(Q a))))

  (intend-equal "equiv prop" '(∧ (∨ (- P) (+ Q)) (∨ (+ P)(- Q))) (clsfy '(≡ P Q)))
  (intend-equal "equiv" '(∧(∨ (- P a) (+ Q a))(∨ (+ P a)(- Q a))) (clsfy '(≡ (P a)(Q a))))

  (intend-equal "equiv prop" '(∧ (∨ (- P) (- Q)) (∨ (+ P)(+ Q))) (clsfy '(≡ P (¬ Q))))
  (intend-equal "equiv and ¬" '(∧(∨ (+ P a) (+ Q a))(∨ (- P a)(- Q a))) (clsfy '(≡ (¬(P a))(Q a))))

;; quantifiers are removed
;; vars came from all ∀ quantifiers
;; finally 3 types of clauses
;;; 1. only unit clause L
;;; 2. only clause      (∨ L1 L2 L3 ...)
;;; 3. a set of clauses (∧ (∨ L...)(∨ L...)...) ? (∧ L1 (∨ L2 L3 L4)...)??

  (setq *gensym-counter* 1)
  (intend-equal "∀" '(∨ (- P x.1 a) (+ Q a x.1)) (clsfy '((∀ x)(⇒ (P x a)(Q a x)))))
  (intend-equal "get vars" '(x.1) *local-vars*)

  (setq *gensym-counter* 1)
  (intend-equal "∀∃" '(∨ (- P x.1 (skf-z.2 x.1)) (+ Q (skf-z.2 x.1) x.1)) (clsfy '((∀ x)((∃ z)(⇒ (P x z)(Q z x))))))
  (intend-equal "get vars" '(x.1) *local-vars*)

  (setq *gensym-counter* 1)
  (intend-equal "∀in∀and∃" '(∨ (- P x.1 (skf-z.2 x.1)) (+ Q n.3 x.1)) (clsfy '((∀ x)((∃ z)(⇒ (P x z)((∀ n)(Q n x)))))))
  (intend-equal "get vars" '(n.3 x.1) *local-vars*)

  (setq *gensym-counter* 1)
  (intend-equal "∀ ∨∨" '(∨ (- P x.1 a) (+ Q x.1) (- S x.1) (+ R x.1)) (clsfy '((∀ x)(∨ (¬(P x a))(∨ (∨ (Q x) (¬ (S x)))(R x))))))
  (intend-equal "get vars" '(x.1) *local-vars*)
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
  (ito-removeq)
  (ito-literalize)
  (ito-div2∧∨)
  (ito-upconj)
;  (ito-clsfy) 
)

(ito-pnf)


