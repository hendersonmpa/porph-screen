;;;; web-app.lisp
(in-package :porph-screen)

;; Web server - Hunchentoot

(defparameter *spectra* nil "This will hold a list of spectra structs")
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
  "All pages on the site will use the following macro;
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
            (:meta :http-equiv "X-UA-Compatible":content "IE=edge" )
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
            (:header :role "banner"; Porph-screen header
                  (:img :src "/static/cary-60.jpg"
                        :alt "Cary-60"
                        :class "logo")
                  (:span :class "strapline"
                         "Division of Biochemistry: HPLC")
                  (:nav :role "navigation"
                        (:ul
                         (:li (:a :href "/select" "Select a file for Upload")))))
            ,@body
            (:footer :role "contentinfo"
             (:img :src "/static/LISP_logo.svg"
                   :alt "Lisp Alien"
                   :class "stamp")
             (:small (:p "Created with Common Lisp")
                     (:p "by Matthew P.A. Henderson")
                     (:p "Division of Biochemistry, The Ottawa Hospital")
                     (:p "e-mail: mathenderson at toh dot on dot ca")))))))

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
                 (:p "Please indicate sample type")
                 (:fieldset :class "radios"
                            (:legend "Sample Matrix:")
                            (:p :class "row"
                                (:input :type :radio :id "sample-urine"
                                        :name "matrix" :value "urine")
                                (:label :for "sample-urine" "Urine"))
                            (:p :class "row"
                                (:input :type :radio :id "sample-fecal"
                                        :name "matrix" :value "fecal")
                                (:label :for "sample-fecal" "Fecal")))
                 (:p (:input :type :submit :value "Submit"))))))


(defun urine-sample-table (spectra-list)
  (who:with-html-output
      (*standard-output* nil :prologue nil :indent t)
    (dolist (spectra spectra-list)
      (let ((id (spectra-id spectra)))
        (cl-who:htm
         (:li
          (:fieldset
           (:label :for "id" "ID")
           (:input :type :text :name "id" :value (str id))
           (:label :for "vol" "Sample Volume (mL)")
           (:input :type :text :name (format nil "~A-vol" id )
                   :value 1000)
           (:label :for "dil" "Acid Volume (mL)")
           (:input :type :text :name (format nil "~A-dil" id )
                   :value 1.05))))))))

(defun fecal-sample-table (spectra-list)
  (who:with-html-output
      (*standard-output* nil :prologue nil :indent t)
    (dolist (spectra spectra-list)
      (let ((id (spectra-id spectra)))
        (cl-who:htm
         (:li
          (:fieldset
           (:label :for "id" "ID")
           (:input :type :text :name "id" :value (str id))
           (:label :for "vol" "Sample Weight (mg)")
           (:input :type :text :name (format nil "~A-vol" id )
                   :value 25)
           (:label :for "dil" "Acid Volume (mL)")
           (:input :type :text :name (format nil "~A-dil" id )
                   :value 4.5))))))))

(define-easy-handler (upload :uri "/upload") (spectra-csv matrix)
  (cond ((null spectra-csv) (redirect "/select"))
        (t (let ((file-name (concatenate 'string *data-repository* (cadr spectra-csv))))
           (rename-file (car spectra-csv) file-name)
           (setf *spectra* (build-spectra file-name matrix))
           (setf *data-pathname* (pathname file-name))
           (standard-page (:title "Upload Complete")
             (:h1 "Success!")
             (:p "The file has been uploaded to the server")
             (:p "File location: " (str file-name) )
             (:p "Sample type: " (str matrix))
             (:h1 "Please indicate sample volume and the volume of acid added")
             (:form :action "/info" :method "post" :id "info-list"
                    (:ol
                     (cond ((string= "urine" matrix)(urine-sample-table *spectra*))
                           ((string= "fecal" matrix)(fecal-sample-table *spectra*))
                           (t (cl-who:htm (:p "please enter a sample type")))))
                    (:input :type :submit :value "Submit")))))))

(hunchentoot:define-easy-handler (info :uri "/info") ()
  (let ((alop (hunchentoot:post-parameters*)))
    (loop for (id vol dil) on alop by #'cdddr
       do (loop for spectra in *spectra*
             if (string= (spectra-id spectra) (cdr id))
             do (add-info spectra (parse-number (cdr vol))
                          (parse-number (cdr dil))))))
  (plot-data *spectra*)
  (redirect "/plots"))

(hunchentoot:define-easy-handler (plots :uri "/plots") ()
  (standard-page (:title "Absorbance Spectra")
    (:h1 "Porphyrin Screen Absorbance Spectra")
    (:ol
     (dolist (spectra *spectra*)
       (let* ((id (spectra-id spectra))
             (triangle (spectra-triangle spectra))
             (base1 (triangle-base1 triangle))
             (peak (triangle-peak triangle))
             (base2 (triangle-base2 triangle))
             (plot-name (concatenate 'string  "/data/" id ".png")))
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
                       (:label :for "base1-nm" "Base 1")
                       (:input :type :text :name "base1-nm" :value (str (point-x base1)))
                       (:label :for "peak-nm" "Peak")
                       (:input :type :text :name "peak-nm" :value (str (point-x peak)))
                       (:label :for "base2-nm" "Base 2")
                       (:input :type :text  :name "base2-nm" :value (str (point-x base2)))
                       (:input :type :submit :value "Submit"))))
                    (:img :src plot-name :alt "plot goes here")))))
    (:a :href "/report" "Prepare Final Report")))

(hunchentoot:define-easy-handler (update :uri "/update") (id base1-nm peak-nm base2-nm)
  ;; TODO include error handling in case all fields are not completed
  (let* ((spectra (dolist (spectra *spectra*)
                    (when (equalp id (spectra-id spectra)) (return spectra))))
         (base1 (find-nearest-point (parse-integer base1-nm :junk-allowed t) spectra))
         (peak (find-nearest-point (parse-integer peak-nm :junk-allowed t) spectra))
         (base2 (find-nearest-point (parse-integer base2-nm :junk-allowed t) spectra))
        (triangle (make-triangle :base1 base1 :peak peak :base2 base2)))
    (setf (spectra-triangle spectra) triangle)
    (setf (spectra-net-abs spectra) (net-abs spectra))
    (single-plot spectra)
    (redirect (concatenate 'string "/plots#" id))))

(hunchentoot:define-easy-handler (report :uri "/report") ()
  (let ((time (multiple-value-bind (sec min hour date mon year)
                  (get-decoded-time)
                (declare (ignore sec))
                (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year mon date hour min))))
    (standard-page (:title "Porphyrin Screen")
      (:h1 (format t "Porphyrin Screen Results Report ~A" time))
      (:ol
       (dolist (spectra *spectra*)
         (let* ((id (spectra-id spectra))
                (matrix (spectra-matrix spectra))
                (result (results spectra))
                (plot-name  (concatenate 'string  "/data/" id ".png")))
           (cl-who:htm
            (:li
             (:table (:tr
                      (:th "Sample ID" )
                      (:th "Matrix" )
                      (:th "nmol/L")
                      (:th "Result"))
                     (:tr
                      (:td (str id))
                      (:td (str matrix))
                      (:td (str (first result)))
                      (:td (str (second result))))))
            (:img :src plot-name :alt "plot here"))))))))




;;(publish-static-content)
;;(start-server 8085)
