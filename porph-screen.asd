;;;; porph-screen.asd

(asdf:defsystem #:porph-screen
  :serial t
  :description "Describe porph-screen here"
  :author "Matthew Henderson"
  :license "GPL"
  :depends-on (#:cl-csv
               #:parse-number)
  :components ((:file "package")
               (:file "find-points")
               (:file "net-abs")
               (:file "plot")
               (:file "porph-screen")))
