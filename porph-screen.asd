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
                 :local-time
		 :eazy-gnuplot)
    :components ((:file "package")
                 (:file "spectra-objects")
                 (:file "instances")
                 (:file "process-upload")
                 (:file "snip")
                 (:file "interpret")
                 (:file "db")
                 (:file "plot")
                 (:file "web-app")
                 (:file "comparison")))
