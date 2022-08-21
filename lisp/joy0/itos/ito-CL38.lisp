;; ito of chang and lee p38


(myload "ito.lisp")
(asdf:load-system :clsfy)

(defito ito-cl1 ()
  "1st sample in Chang&Lee"
;(CL1 (⇒ ((∀ x)(P x)) ((∃ x)(Q x))))
;(CA1 ((∃ x)(∨ (¬ P x) (Q x))))
  (intend-equal "chang&lee sample1" '((∃ x)(∨ (¬ P x) (Q x))) (pnfy '(⇒ ((∀ x)(P x)) ((∃ x)(Q x)))))

;(CL2 ((∀ x)((∀ y)((∃ z)(⇒ (∧ (P x z) (P y z)) ((∃ u)(Q x y u)))))))
;(CA2 ((∀ x)((∀ y)((∃ z)(∃ u) (∨ (¬ (P x z)) (∨ (¬ (P y z)) (Q x y u)))))))
  (intend-equal "chang&lee sample2" '((∀ x)((∀ y)((∃ z)(⇒ (∧ (P x z) (P y z)) ((∃ u)(Q x y u))))))
    (pnfy '((∀ x)((∀ y)((∃ z)(∃ u) (∨ (¬ (P x z)) (∨ (¬ (P y z)) (Q x y u))))))))
)


(ito-cl1)

