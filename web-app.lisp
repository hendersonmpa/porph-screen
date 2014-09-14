;;;; web-app.lisp
(in-package :porph-screen)

;; Web server - Hunchentoot

(setf hunchentoot:*catch-errors-p* nil) ; T for production
(setf hunchentoot:*show-lisp-errors-p* t)
(setf hunchentoot:*show-lisp-backtraces-p* t)

(defparameter *http-port* 4242)

(defvar *my-acceptor* nil)
;; (defun start-server (port)
;;   (start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun start-server ()
  (unless *my-acceptor*
    (pushnew (hunchentoot:create-folder-dispatcher-and-handler
              "/static/" "~/lisp/site/porph-screen/static/")
             hunchentoot:*dispatch-table* :test #'equal)
    (pushnew (hunchentoot:create-folder-dispatcher-and-handler
              "/data/" "~/lisp/site/porph-screen/data/")
             hunchentoot:*dispatch-table* :test #'equal)
    (setf *my-acceptor*
          (hunchentoot:start (make-instance
                              'hunchentoot:easy-acceptor
                              :port *http-port*)))))
(defun stop-server ()
  (when *my-acceptor*
    (hunchentoot:stop *my-acceptor*)
    (setf hunchentoot:*dispatch-table*
          (last hunchentoot:*dispatch-table*))
    (setf *my-acceptor* nil)))

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
                         "Division of Biochemistry: HPLC"))
            ,@body
            (:footer :role "contentinfo"
             (:img :src "/static/lisplogo_warning2_128.png"
                   :alt "Lisp Alien"
                   :class "stamp")
             (:p (:small "Created by Matthew P.A. Henderson"
                         (:a :href "mailto:mathenderson@toh.on.ca"
                             "mathenderson@toh.on.ca"))))))))



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
  (let ((file-name (concatenate 'string *data-repository* (cadr spectra-csv))))
    (rename-file (car spectra-csv) file-name)
    (setf *spectra* (complete-spectra *test-file*))
    (standard-page (:title "Upload Complete")
      (:h1 "Success!")
      (:p  "The file has been uploaded to the server")
      (:p "File location: " (str file-name) )
      (:h2 "Review absorbance spectra and baseline correction "
          (:a :href "plots" "here")))))

;; (hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
;;   (setf (hunchentoot:content-type*) "text/plain")
;;   (format nil "Hey~@[ ~A~]!" name))

;; (defvar *test-directory* #p"/User/matthew/lisp/site/porph-screen/data/")
;; (defvar *test-files* nil)

(hunchentoot:define-easy-handler (plots :uri "/plots") ()
  (standard-page (:title "Absorbance Spectra")
    (:h1 "Porphyrin Screen Absorbance Spectra")
    (:ul
     (dolist (spectra *spectra*)
       (let* ((id (spectra-id spectra))
              (triangle (spectra-triangle spectra))
              (base1 (triangle-base1 triangle))
              (peak (triangle-peak triangle))
              (base2 (triangle-base2 triangle)))
         (plot-data spectra)
         (cl-who:htm
          (:li
           (:figure :id (str id)
                (:table (:tr
                         (:th "Sample ID" )
                         (:th "Base 1 (nm)")
                         (:th "Peak (nm)")
                         (:th "Base 2 (nm)")
                         (:th "Net Absorbance"))
                        (:tr
                         (:td (str id))
                         (:td (str (point-x base1)))
                         (:td (str (point-x peak)))
                         (:td (str (point-x base2)))
                         (:td (str (spectra-net-abs spectra)))))
                (:p "Manually enter wavelength (nm) for points if required")
                (:form :action "/update" :method "post" :id "user-input"
                       (:label :for "id" "ID")
                       (:input :type :text :name "id" :value (str id))
                       (:label :for "base_1" "Base 1")
                       (:input :type :text :name "base1-nm")
                       (:label :for "peak" "Peak")
                       (:input :type :text :name "peak-nm")
                       (:label :for "base_2" "Base 2")
                       (:input :type :text  :name "base2-nm")
                       (:input :type :submit :value "Submit"))
                (:img :src  (concatenate 'string  "/data/" id ".png")
                      :alt "plot here")))))))))

(hunchentoot:define-easy-handler (update :uri "/update") (id base1-nm peak-nm base2-nm)
  (let* ((spectra (dolist (spectra *spectra*)
                    (when (equalp id (spectra-id spectra)) (return spectra))))
         (base1 (find-nearest-point (parse-integer base1-nm) spectra))
         (peak (find-nearest-point (parse-integer peak-nm) spectra))
         (base2 (find-nearest-point (parse-integer base2-nm) spectra))
        (triangle (make-triangle :base1 base1 :peak peak :base2 base2)))
    (setf (spectra-triangle spectra) triangle)
    (setf (spectra-net-abs spectra) (net-abs spectra))
    (plot-data spectra)
    (redirect (concatenate 'string "/plots#" id))))
    ;; (standard-page (:title "Absorbance Spectra")
    ;;   (:h1 "Porphyrin Screen Absorbance Spectra")
    ;;   (cl-who:htm
    ;;    (:figure
    ;;     (:table (:tr
    ;;              (:th "Sample ID" )
    ;;              (:th "Base 1 (nm)")
    ;;              (:th "Peak (nm)")
    ;;              (:th "Base 2 (nm)")
    ;;              (:th "Net Absorbance"))
    ;;             (:tr
    ;;              (:td (str (spectra-id spectra)))
    ;;              (:td (str (point-x base1)))
    ;;              (:td (str (point-x peak)))
    ;;              (:td (str (point-x base2)))2
    ;;              (:td (str (spectra-net-abs spectra)))))
    ;;     (:img :src  (concatenate 'string  "/data/" id ".png")
    ;;           :alt "plot here"))))))




;;(publish-static-content)
;;(start-server 8085)
