;;;; package.lisp
(defpackage :porph-screen
  (:use :cl
        :cl-csv
        :parse-number
        :cl-who
        :hunchentoot))

;; (rename-package "CL-PPCRE" "CL-PPCRE" '("RE"))
; (rename-package "HUNCHENTOOT" "HUNCHENTOOT" '("HUNCH"))
(rename-package "CL-WHO" "CL-WHO" '("WHO"))
