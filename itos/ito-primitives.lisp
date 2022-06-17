;; ito of primitives

(myload "ito.lisp")
(asdf:load-system :pnfy)

(defito ito-primitives ()
  "ito of primitives"

; is-?


; is?
  (intend-T "is  prop ∧" (is-∧ '(∧ P Q)))
  (intend-T "is  pred ∧" (is-∧ '(∧ (P x) (Q y))))
  (intend-F "is not  pred ∧" (is-∧ '(X (P x) (Q y))))
  (intend-F "is not  pred ∧" (is-∧ '(X P Q)))

  (intend-T "is  prop ∨" (is-∨ '(∨ P Q)))
  (intend-T "is  pred ∨" (is-∨ '(∨ (P x)(Q y))))

  (intend-T "is  prop ≡" (is-≡ '(≡ P Q)))
  (intend-T "is  pred ≡" (is-≡ '(≡ (P x)(Q y))))

  (intend-T "is  prop ⇒" (is-⇒ '(⇒ P Q)))
  (intend-T "is  pred ⇒" (is-⇒ '(⇒ (P x)(Q y))))

  (intend-T "is  prop ⇦" (is-⇦ '(⇦ P Q)))
  (intend-T "is  pred ⇦" (is-⇦ '(⇦ (P x)(Q y))))

  (intend-T "is ∀x"      (is-∀ '((∀ x) (∨ (P x)(Q y)))))
  (intend-F "is not ∀x"  (is-∀ '((∃ x) (∨ (P x)(Q y)))))
  (intend-F "is not ∀x"  (is-∀ '(¬ (P x))))

  (intend-T "is ∃x"     (is-∃ '((∃ x) (∨ (P x)(Q y)))))
  (intend-F "is not ∃x" (is-∃ '((∀ x) (∧ (P x)(Q y)))))
  (intend-F "is not ∀x" (is-∃ '(¬ (P x))))

; of?
  (intend-T "opof prop ∧" (opof '(∧ P Q)))
  (intend-T "opof pred ∧" (opof '(∧ (P x)(Q y))))
  (intend-T "opof prop ∨" (opof '(∨ P Q)))
  (intend-T "opof pred ∨" (opof '(∨ (P x)(Q y))))
  (intend-T "opof prop ≡" (opof '(≡ P Q)))
  (intend-T "opof pred ≡" (opof '(≡ (P x)(Q y))))
  (intend-T "opof prop ⇒" (opof '(⇒ P Q)))
  (intend-T "opof pred ⇒" (opof '(⇒ (P x)(Q y))))
  (intend-T "opof prop ⇦" (opof '(⇦ P Q)))
  (intend-T "opof pred ⇦" (opof '(⇦ (P x)(Q y))))

  (intend-equal "opof pred ∀x" '∀ (quantof '((∀ x) (P x)(Q y))))
  (intend-equal "opof pred ∀x" 'x (bvarof '((∀ x) (P x)(Q y))))
  (intend-equal "opof pred ∃x" '∃ (quantof '((∃ x) (P x)(Q y))))
  (intend-equal "opof pred ∃x" 'x (bvarof '((∃ x) (P x)(Q y))))

  (intend-equal "argof ∀" '(∨ (P x)(Q y)) (argof '((∀ x) (∨ (P x)(Q y))) 1))
  (intend-equal "argof 1 of 2arg" '(P x) (argof '(∧ (P x)(Q y)) 1))
  (intend-equal "argof 2 of 2arg" '(Q y) (argof '(∧ (P x)(Q y)) 2))
  (intend-equal "argof 1arg as ¬" '(P x) (argof '(¬ (P x)) 1))

; make-?
  (intend-equal "make prop ≡" '(≡  P Q)  (make-≡ 'P 'Q))
  (intend-equal "make prop ⇒" '(⇒  P Q)  (make-⇒ 'P 'Q))
  (intend-equal "make prop ⇦" '(⇦  P Q)  (make-⇦ 'P 'Q))
  (intend-equal "make prop ¬" '(¬  P)  (make-¬ 'P))

  (intend-equal "make pred ≡" '(≡  (P a) (Q a))  (make-≡ '(P a) '(Q a)))
  (intend-equal "make pred ⇒" '(⇒  (P a) (Q a))  (make-⇒ '(P a) '(Q a)))
  (intend-equal "make pred ⇦" '(⇦  (P a) (Q a))  (make-⇦ '(P a) '(Q a)))

  (intend-equal "make pred ¬" '(¬  (P a) )  (make-¬ '(P a)))

  (intend-equal "make any" '((∀ x) (P x y))  (make-∀ 'x '(P x y)))
  (intend-equal "make exit" '((∃ x) (P x y)) (make-∃ 'x '(P x y)))

)


(ito-primitives)

