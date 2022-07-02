;; ito of primitives for binding

(myload "ito.lisp")
(asdf:load-system :clsfy)

(defito ito-newsym ()
  "newsym work"
  (setq *gensym-counter* 1)
  (intend-equal "new var " 'x.1 (newsym 'x))
  (intend-equal "new var " 'x.2 (newsym 'x))
  (intend-equal "new var " 'x.3 (newsym 'x.2))
)

(defito ito-binds ()
  "binding operations"

;; make
  (intend-equal "put a new " '((x . x.1)) (put-sym-new 'x 'x.1 ()))
  (intend-equal "put a new on same sym" '((x . x.2)(x . x.1)) (put-sym-new 'x 'x.2 '((x . x.1))))
  (intend-equal "put a new on diff sym" '((y . y.1)(x . x.1))  (put-sym-new 'y 'y.1 '((x . x.1))))

;; make a parameter list from binding
  (intend-equal "all diff args" '(x.1 y.2 z.3) (get-skolem-parameters '((x . x.1)(y . y.2)(z . z.3))))
  (intend-equal "exist var not" '(x.1 z.3) (get-skolem-parameters '((x . x.1)(y . (f-y.2 x.1))(z . z.3))))
  (intend-equal "exist var not" '(x.1 z.3) (get-skolem-parameters '((x . x.1)(z . z.3)(y . (f-y.2 x.1))(z . z.4))))
  (intend-equal "skip same var" '(x.1 y.2) (get-skolem-parameters '((x . x.1)(y . y.2)(x . x.3))))
  (intend-equal "skip same var" '(x.1 y.2) (get-skolem-parameters '((x . x.1)(y . y.2)(x . x.3)(y . y.4))))

;; make binding.
; ((∀ x) wff) => (x . new)
; ((∃ x) wff) ~> (x . (new ...)) 


;; skolem
  (setq *gensym-counter* 1)
  (intend-equal "skolemize with 0 args"  '(skf-abc.1)  (skolemize 'abc.1 ()))
  (setq *gensym-counter* 12)
  (intend-equal "skolemize with 2 args"  '(skf-abc.12 x.10 y.11) (skolemize 'abc.12 '((x . x.10)(z . (skf-x.5 x.10))(y . y.11))))

;; skolemize
;; on ∃ duplicate ∀
; ((∀ x)(∀ y)(∃ z)) => ((z . (skf-z.3))(y . y.2)(x . x.1))
;  => ((z . (skf-z.3 y.2 x.1))(y . y.2)(x . x.1))
;  in wff, z => (skf-z.3 y.2 x.1)
;          y => y.2
;          x => x.1


  (setq *gensym-counter* 1)
  (intend-equal "same vars in order"
        '((∀ x.1)((∀ y.2)(∨((∀ x.3)(P x.3 y.2 (skf-z.4 x.3 y.2)))(R x.1 y.2))))
         (rename-bv '((∀ x)((∀ y)(∨ ((∀ x)((∃ z)(P x y z))) (R x y))))))


  (setq *gensym-counter* 1)
  (intend-equal "no skolem with 0 args"  '(P x)  (rename-bv '(P x) ()))

  (setq *gensym-counter* 1)
  (intend-equal "a skolem with 0 args"  '(P (skf-x.1))  (rename-bv '((∃ x)(P x))) )

  (setq *gensym-counter* 1)
  (intend-equal "a skolem with 2 args"  '((∀ x.1)(P x.1 (skf-y.2 x.1) (skf-z.3 x.1)))  (rename-bv '((∀ x)((∃ y)((∃ z)(P x y z))))))

;; subst using binding



)


(defun ito-allbinding ()
  (ito-newsym)
  (ito-binds)
)

(ito-allbinding)

