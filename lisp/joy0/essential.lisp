; essentials for pnfy
; functions in this file may tested by my eyes.

(defun readafile0 (fname)
 (with-open-file (in fname)
  (read in)
 )
)

(defun readafile (fname)
 (with-open-file (ins fname)
   (let (data)
     (loop until (eq :eof (setf data (read ins nil :eof)))
       collect
          data
       )
     )
 )
)


;;; writeafile
; from rubbish-kqcio.lisp
(defun writeafile (fname objects)
  (with-open-file (out fname
      :direction :output
      :if-exists :supersede)

    (print objects out)
  )
)

; from rubbish-wff.lisp
(defun write-kqc (fname note kqc)
  (with-open-file (out fname
      :direction :output
      :if-exists :supersede)
    (format out ";~a @ ~a~%" fname note)
    (loop for sexp in kqc do
      (format out "~a~%" sexp)
    )
  )
)
;;

(defun wff2kqc (wffname note kqcname)
  (let (wffs)
    (setq wffs (readafile wffname))
    (with-open-file (out kqcname
                      :direction :output
                      :if-exists :supersede)
      (format out ";~a @ ~a~%" wffname note)
  
      (loop for wff in wffs collect
        (let ((kqc (clausefy wff)))
          (format out ";~a~%" wff)
          (loop for sexp in kqc do
            (format out "~a~%~%" sexp)
            (format t "~a~%" sexp)
          )
        )
      )
      T
    )
  ) 
)

; initial setup for this, 
; :wname : cname -> wffname
; :wff : wffname -> wff
; after make clause, cid.wff points to original wff
; cname comes from pnfy

(defun setwff (name wff)
  (setf (get name :wff) wff)
)

(defun wffof (name)
  (get name :wff)
)

