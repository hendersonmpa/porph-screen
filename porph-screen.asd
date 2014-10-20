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
               :cl-ppcre)
  :components ((:file "package")
;               (:file "find-points")
               (:file "snip")
;               (:file "net-abs")
;               (:file "plot")
               (:file "porph-screen")
;               (:file "web-app")
               ))
