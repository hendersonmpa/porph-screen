;;;; web-app.lisp
(in-package :porph-screen)

;; Web server - Hunchentoot
(defun start-server (port)
  (start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun publish-static-content ()
  ;create-folder-dispatcher-and-handler
  (pushnew (create-folder-dispatcher-and-handler
            "/static/" "~/lisp/site/porph-screen/static/")
           *dispatch-table* :test #'equal)
  (pushnew (create-folder-dispatcher-and-handler
            "/data/"  "~/lisp/site/porph-screen/data/")
           *dispatch-table* :test #'equal))
;; Create our pages
(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(who:with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:meta :name "description" :content"")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/static/normalize.css")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/static/main.css")
            ;; (:link :type "text/css"
            ;;        :rel "stylesheet"
            ;;        :href "http://fonts.googleapis.com/css?family=Ubuntu")
            ;; (:link :type "text/css"
            ;;        :rel "stylesheet"
            ;;        :href "http://fonts.googleapis.com/css?family=Droid+Sans")
            ,(when script
                   `(:script :type "text/javascript"
                             (who:str ,script))))
           (:body
            (:div :id "header" ; Retro games header
                  (:img :src "/static/cary-60.jpg"
                        :alt "Cary-60"
                        :class "logo")
                  (:span :class "strapline"
                         "Division of Biochemistry The Ottawa Hospital"))
            ,@body))))

(define-easy-handler (select :uri "/select") ()
  (standard-page (:title "File Upload for Cary-60")
    (:h1 "File Upload for Porphyrin Screen Data")
    (:div :id "chart" ; Used for CSS styling of the links.
          (:p "Select the .csv file from the run data")
          (:form :method :post :enctype "multipart/form-data"
                 :action "upload" ;; What happens when you submit
                 (:p "Please choose a file: "
                     (:input :type :file
                             :name "spectra-csv"))
                 (:p (:input :type :submit :value "Submit file"))))))


;; (hunchentoot:define-easy-handler (upload :uri "/upload") (uploaded-file)
;;   (rename-file (car uploaded-file)
;;                (concatenate 'string "/tmp/" (cadr uploaded-file)))
;;   "SUCCESS")

(define-easy-handler (upload :uri "/upload") (spectra-csv)
  (rename-file (car spectra-csv)
               (concatenate 'string "/Users/matthew/lisp/site/porph-screen/data/" (cadr spectra-csv)))
  (standard-page (:title "Upload Complete")
    (:h1 "Success!")
    (:p  "The file: " (who:str spectra-csv) " has been uploaded")
    (:p "Review absorbance spectra and baseline correction "
        (:a :href "plots" "here"))))

;; (hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
;;   (setf (hunchentoot:content-type*) "text/plain")
;;   (format nil "Hey~@[ ~A~]!" name))

;; (defvar *test-directory* #p"/User/matthew/lisp/site/porph-screen/data/")
;; (defvar *test-files* nil)

(hunchentoot:define-easy-handler (plot :uri "/plot") ()
  (let* ((spectra (car *spectra*))
        (id (spectra-id spectra)))
    (plot-data spectra)
    (standard-page (:title "Absorbance Spectra")
      (:h1 "Porphyrin Screen Absorbance Spectra")
      (:p  "Some information about the plot")
      (:img :src  (concatenate 'string  "/data/" id ".png")
            :alt "plot here"))))

(hunchentoot:define-easy-handler (plots :uri "/plots") ()
  (standard-page (:title "Absorbance Spectra")
    (:h1 "Porphyrin Screen Absorbance Spectra")
    (:ol
     (dolist (spectra *spectra*)
       (let* ((id (spectra-id spectra))
              (triangle (spectra-triangle spectra))
              (base1 (triangle-base1 triangle))
              (peak (triangle-peak triangle))
              (base2 (triangle-base2 triangle)))
         (plot-data spectra)
         (cl-who:htm
          (:li (str (concatenate 'string "Sample ID: " id))
               (:table (:tr
                        (:th "Sample ID" )
                        (:th "Base 1 (nm)")
                        (:th "Peak (nm)")
                        (:th "Base 2 (nm)")
                        (:th "Net Absorbance"))
                       (:tr
                        (:td (str (spectra-id spectra)))
                        (:td (str (point-x base1)))
                        (:td (str (point-x peak)))
                        (:td (str (point-x base2)))
                        (:td (str (spectra-net-abs spectra)))))
               (:img :src  (concatenate 'string  "/data/" id ".png")
                     :alt "plot here"))))))))



  ;;(publish-static-content)
  ;;(start-server 8085)
