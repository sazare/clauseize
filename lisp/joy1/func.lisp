; a framework framework

(defun f (w) 
  (let ( 
    (¬ `(cdr ,w))
    (∨ `(∧ (¬* (cdr ,w))))
    (∧ `(∨ (¬* (cdr ,w))))
    )
    (eval w)
  )
)



;; example
; (¬ (P a)) → (¬ (P a))
; (P x) → (P x)
; (¬ (∧ P Q)) → (∨ (¬ P)(¬ Q))
; (∨ (¬ (∨ (P a)(¬ (Q b))(¬ (R c))))) → 
; (∀ x (P x)) → (∀ x (P x))
; (¬ (∀ x (P x))) → (∃ x (¬ P x))
; (∀ x (¬ (P x))) → (∀ x (P x))
; E -> E when no ¬ in E 


;?
(defun ff (x) 
  (let (
        (a (lambda (x) (car x)))
        (d (lambda (x) (cdr x)))
        (c (lambda (x y)(cons x y))))
    (apply (car x)(cdr x))
)

;;; abstruct abstrunction

;; (f x)
(defun fx (y def)
  `(let (,def ) 
     (eval ,y)
   )
)

(defmacro mx (e def)
  `(let (,@def) 
     ,e
   )
)


(fx '(f a b) '((f (lambda (x y) (+ x y)))(a 2)(b 3))) 
(mx '(f a b) '((f (lambda (x y) (+ x y)))(a 2)(b 3)))


