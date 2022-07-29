; run clausefy on wffs/t?.wff

(asdf:load-system :clsfy)

(defparameter w0 (readafile "../wffs/t0.wff"))
(defparameter c0 (clausefy w0))

(defparameter w1 (readafile "../wffs/t1.wff"))
(defparameter c1 (clausefy w1))

(defparameter w2 (readafile "../wffs/t2.wff"))
(defparameter c2 (clausefy w2))

(defparameter w3 (readafile "../wffs/t3.wff"))
(defparameter c3 (clausefy w3))

(defparameter w4 (readafile "../wffs/t4.wff"))
(defparameter c4 (clausefy w4))

(defparameter w5 (readafile "../wffs/t5.wff"))
(defparameter c5 (clausefy w5))

(defparameter w6 (readafile "../wffs/t6.wff"))
(defparameter c6 (clausefy w6))

(defparameter w7 (readafile "../wffs/t7.wff"))
(defparameter c7 (clausefy w7))

(defparameter w8 (readafile "../wffs/t8.wff"))
(defparameter c8 (clausefy w8))

(defparameter w9 (readafile "../wffs/t9.wff"))
(defparameter c9 (clausefy w9))

(defparameter w10 (readafile "../wffs/s0.wff"))
(defparameter yy (loop for w in w10 append (clausefy w) ))
(write-kqc  "a.kqc" "for s0.wff" yy)


