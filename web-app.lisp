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

(defun restart-server ()
  (stop-server)
  (start-server))
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
                            "Chromatography and Spectroscopy")
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
    (:h3 "File Upload for Porphyrin Screen Data")
    (:p "Select the .csv file for this analysis")
    (:form :method :post :enctype "multipart/form-data"
           :action "upload" ;; What happens when you submit
           (:p "Select the file: "
               (:input :type :file
                       :name "spectra-csv" :class "btn"))
           (:p "Indicate sample type")
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
           (:p (:input :type :submit :value "Submit" :class "btn")))))


(defgeneric sample-table (spectra-list)
  (:documentation "Sample table for matrix"))

(defmethod sample-table ((s urine-spectra-list))
  (let ((spectra-list (los s)))
    (who:with-html-output
        (*standard-output* nil :prologue nil :indent t)
      (:table (:tr
               (:th "ID")
               (:th "Volume (mL)")
               (:th "Dilution Factor"))
              (dolist (spectra spectra-list)
                (let ((id (id spectra)))
                  (cl-who:htm
                   (:tr
                    (:td (:input :type "text":name "id" :value (str id) :class "txt"
                                 :readonly "readonly"))
                    (:td (:input :type "text":name (format nil "~A-vol" id )
                                 :value 1000 :class "txt"))
                    (:td (:input :type "text" :name (format nil "~A-dil" id )
                                 :value 1.05 :class "txt"))))))))))

(defmethod sample-table ((s fecal-spectra-list))
  (let ((spectra-list (los s)))
    (who:with-html-output
        (*standard-output* nil :prologue nil :indent t)
      (:table (:tr
               (:th "ID" )
               (:th "Weight (g)"))
              (dolist (spectra spectra-list)
                (let ((id (id spectra)))
                  (cl-who:htm
                   (:tr
                    (:td (:input :type "text" :name "id" :value (str id) :class "txt"
                                 :readonly "readonly"))
                    (:td (:input :type "text" :name "vol"
                                 :value 0.05 :class "txt"))))))))))

(define-easy-handler (upload :uri "/upload") (spectra-csv matrix)
  (cond ((null spectra-csv) (redirect "/select"))
        (t (let ((file-name (concatenate 'string *data-repository* (cadr spectra-csv))))
           (rename-file (car spectra-csv) file-name)
           (setf *spectra* (build-spectra-list file-name matrix))
           (setf *data-pathname* (pathname file-name))
           (standard-page (:title "Upload Complete")
             (:h3 "The file has been uploaded to the server")
             (:p "File location: " (str file-name) )
             (:p "Sample type: " (string-capitalize (str matrix)))
             (:h3 "Please indicate sample volume and the volume of acid added")
             (:form :action "/info" :method "post" :id "info-list"
                    (:ol
                     (sample-table *spectra*))
                    (:input :type :submit :value "Submit" :class "btn")))))))

;; Changing the form cells
;;<input type="hidden" name="hiddenfield" value="text" />
;;<input type="text" name="rofield" value="text" readonly="readonly" />

(defun string-to-float (str)
  (float (pn:parse-number str)))

(defgeneric process-form (spectra-list parameters-a-list)
  (:documentation "Process the a-lists from the sample table"))

(defmethod process-form ((s urine-spectra-list) parameters-a-list)
  (let ((spectra-list (los s)))
    (loop for (id vol dil) on parameters-a-list by #'cdddr
       do (loop for spectra in spectra-list
             if (string= (id spectra) (cdr id))
             do (sample-size-info spectra (string-to-float (cdr vol))
                                  (string-to-float (cdr dil)))))))

(defmethod process-form ((s fecal-spectra-list) parameters-a-list)
  (let ((spectra-list (los s)))
    (loop for (id vol) on parameters-a-list by #'cddr
       do (loop for spectra in spectra-list
             if (string= (id spectra) (cdr id))
             do (sample-size-info spectra (string-to-float (cdr vol)))))))

(hunchentoot:define-easy-handler (info :uri "/info") ()
  (let ((alop (hunchentoot:post-parameters*)))
;;    (setf *alop* alop)
    (process-form *spectra* alop))
  (mapcar #'delete-file (directory (concatenate 'string *data-repository* "/*.png")))
  (plot-data *spectra*)
  (redirect "/plots"))

(hunchentoot:define-easy-handler (plots :uri "/plots") ()
  (let ((spectra-list (los *spectra*)))
    (standard-page (:title "Absorbance Spectra")
      (:h2 "Porphyrin Screen Absorbance Spectra Analysis")
      (:ol
       (dolist (spectra spectra-list)
         (let* ((id (id spectra))
                (net-ab (net-ab spectra))
                (plot-name (concatenate 'string  "/data/" id ".png")))
           (cl-who:htm
            (:li
             (:section
              :id id
              (:h3 (format t "Absorbance Spectra: ~A" id))
              (:form :action "/update" :method "post" :id "user-input"
                     (:table (:tr
                              (:th "Sample ID" )
                              (:th "Net Absorbance"))
                             (:tr
                              (:td (str id))
                              (:td (str net-ab)))))
              (:img :src plot-name :alt "plot goes here")))))))
      (:form :action "/report"
             (:input :type :submit :value "Create Report" :class "btn")))))

;; (hunchentoot:define-easy-handler (update :uri "/update") (id base1-nm peak-nm base2-nm)
;;   ;; TODO include error handling in case all fields are not completed
;;   (let* ((spectra (dolist (spectra *spectra*)
;;                     (when (equalp id (spectra-id spectra)) (return spectra))))
;;          (base1 (find-nearest-point (parse-integer base1-nm :junk-allowed t) spectra))
;;          (peak (find-nearest-point (parse-integer peak-nm :junk-allowed t) spectra))
;;          (base2 (find-nearest-point (parse-integer base2-nm :junk-allowed t) spectra))
;;         (triangle (make-triangle :base1 base1 :peak peak :base2 base2)))
;;     (setf (spectra-triangle spectra) triangle)
;;     (setf (spectra-net-abs spectra) (net-abs spectra))
;;     (single-plot spectra)
;;     (redirect (concatenate 'string "/plots#" id))))

(hunchentoot:define-easy-handler (report :uri "/report") ()
  (let ((time (multiple-value-bind (sec min hour date mon year)
                  (get-decoded-time)
                (declare (ignore sec))
                (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year mon date hour min)))
        (spectra-list (los *spectra*)))
    ;;(results-csv *spectra*)
    ;;(print-spectra-list *spectra*)
    (prog1 (standard-page (:title "Porphyrin Screen")
             (:h2 (format t "Porphyrin Screen Results Report ~A" time))
             (:ol
              (dolist (spectra spectra-list)
                (let* ((spectra (classify-spectra spectra))
                       (id (id spectra))
                       (matrix (string-capitalize (matrix spectra)))
                       (result (result spectra))
                       (conc (concentration spectra))
                       (plot-name  (concatenate 'string  "/data/" id ".png")))
                  (cl-who:htm
                   (:li
                    (:section
                     (:h3 (format t "~A Porphyrin Screen Sample: ~A" matrix id))
                     (:table (:tr
                              (:th "Sample ID" )
                              (:th "Matrix" )
                              (:th "nmol/L")
                              (:th "Result"))
                             (:tr
                              (:td (str id))
                              (:td (str matrix))
                              (:td (str conc))
                              (:td (str result))))
                     (:img :src plot-name :alt "plot here"))))))))
      (update-tables *spectra*))))

;;(publish-static-content)
;;(start-server 8085)
