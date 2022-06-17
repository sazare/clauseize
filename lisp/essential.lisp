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


