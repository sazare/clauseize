;; ito of combinational

(myload "ito.lisp")
(asdf:load-system :clsfy)

(defito ito-combination ()
  "ito of list combination"

  (intend-equal "(abc)x(de)" '((a d)(a e)(b d)(b e)(c d)(c e)) (nross '((a b c) (d e))))
  (intend-equal "(ab)x(c)" '((a c)(b c)) (nross '((a b)(c))))
  (intend-equal "(ab)x(c)x(e)" '((a c e)(b c e)) (nross '((a b)(c)(e))))

)

;; (intend-equal "list of atomic" '(((P X) (Q X)) ((¬ (R X)) (Q X))) (nross '(((P x)(¬ (R x)))((Q x)))))


(ito-combination)

