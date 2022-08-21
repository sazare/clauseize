; pnfy system definition
(defsystem :clsfy
  :description "clsfy: a tool for converting wff to clause"
  :version "0.0.1"
  :author "Shinichi OMURA(song.of.sand@gmail.com)"
  :licence "MIT licence"
  :serial t
  :components
    (
      (:file "package")
      (:file "primitives"  :depends-on ("package"))
      (:file "binding"     :depends-on ("package"))
      (:file "essential"   :depends-on ("package"))
      (:file "disjoint-bv" :depends-on ("package"))
      (:file "imply"       :depends-on ("package"))
      (:file "neginto"     :depends-on ("package"))
      (:file "upconj"      :depends-on ("package"))
      (:file "clsfy"       :depends-on ("package"))
      (:file "form-kqc"    :depends-on ("package"))
    )
)

