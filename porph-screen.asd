;;;; porph-screen.asd

(asdf:defsystem :porph-screen
  :serial t
  :description "Describe porph-screen here"
  :author "Matthew Henderson"
  :license "GPL"
  :depends-on (:cl-csv
               :parse-number
               :cl-who
               :hunchentoot
               :parenscript
               :cl-ppcre
               :clsql
               :local-time)
  :components ((:file "package")
               (:file "spectra-objects")
               (:file "process-upload")
               (:file "snip")
               (:file "interpret")
               (:file "data-base")
               (:file "plot")
               (:file "web-app")
               (:file "comparision")))
