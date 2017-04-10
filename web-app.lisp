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
              "/static/" "/home/mpah/lisp/site/porph-screen/static/")
             hunchentoot:*dispatch-table* :test #'equal)
    (pushnew (hunchentoot:create-folder-dispatcher-and-handler
              "/data/" "/data/")
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
                   :alt "Lisp Logo"
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
           :action "sample-table" ;; What happens when you submit
           (:p "Select the file: "
               (:input :type :file
                       :name "spectra-csv" :class "btn"))
           (:p "Indicate sample type")
           (:fieldset :class "radios"
                      (:legend "Sample Matrix:")
                      (:p :class "row"
                          (:input :type :radio :id "sample-urine"
                                  :name "matrix" :value "urine" :checked "checked")
                          (:label :for "sample-urine" "Urine"))
                      (:p :class "row"
                          (:input :type :radio :id "sample-fecal"
                                  :name "matrix" :value "fecal")
                          (:label :for "sample-fecal" "Fecal")))
           (:p (:input :type :submit :value "Submit" :class "btn")))))

;; (defun validate-form-js (fun-name form-names)
;;   (labels ((function-template (&rest body)
;;              `(parenscript:ps (defun ,fun-name (evt)
;;                                 (when  ,@body
;;                                   (chain evt (prevent-default))
;;                                   (alert "please enter a volume")))))
;;            (add-conditions (form-names)
;;              (let ((accum '(or)))
;;                (dolist (n form-names (function-template (reverse accum)))
;;                  (push `(= (@ info-list ,n value) "") accum)))))
;;     (eval (add-conditions form-names))))

;; (parenscript:ps ; client side validation
;;   (defvar info-list nil)
;;   (defun validate-volume (evt)
;;     (when (= (@ info-list vol value) "")
;;       (chain evt (prevent-default))
;;       (alert "Please enter a volume.")))
;;   (defun init ()
;;     (setf info-list
;;           (chain document
;;                  (get-element-by-id "infoList")))
;;     (chain info-list
;;            (add-event-listener "submit" validate-volume false)))
;;   (setf (chain window onload) init))

(defgeneric make-sample-table (spectra-list)
  (:documentation "Sample table for matrix"))

(defmethod make-sample-table ((s urine-spectra-list))
  (let ((spectra-list (los s))
        (count 0))
    (who:with-html-output
        (*standard-output* nil :prologue nil :indent t)
      (:table (:tr
               (:th "#")
               (:th "ID")
               (:th "Specimen Vol (mL)")
               (:th "Acid Vol (uL)")
               (:th "Dilution Factor"))
              (dolist (spectra spectra-list)
                (let ((id (id spectra))
                      (count (incf count)))
                  (cl-who:htm
                   (:tr
                    (:td (str count))
                    (:td (:input :type "text":name "id" :value (str id) :class "txt"
                                 :readonly "readonly"))
                    (:td (:input :type "number" :title "Enter the volume" :name "vol"
                                        ;(format nil "~A-vol" id )
                                 :value 1000 :class "txt" :required "required"))
                    (:td (:input :type "number" :title "Enter the volume" :name "acid"
                                        ;(format nil "~A-acid" id )
                                 :value 100 :class "txt" :required "required"))
                    (:td (:input :type "number" :title "Enter the dilution factor"
                                 :name "dil" :value 1 :class "txt"
                                 :required "required"))))))))))

(defmethod make-sample-table ((s fecal-spectra-list))
  (let ((spectra-list (los s))
        (count 0))
    (who:with-html-output
        (*standard-output* nil :prologue nil :indent t)
      (:table (:tr
               (:th "#")
               (:th "ID" )
               (:th "Sample Weight (mg)")
               (:th "Dilution Factor"))
              (dolist (spectra spectra-list)
                (let ((id (id spectra))
                      (count (incf count)))
                  (cl-who:htm
                   (:tr
                    (:td (str count))
                    (:td (:input :type "text" :name "id" :value (str id) :class "txt"
                                 :readonly "readonly"))
                    (:td (:input :type "number" :title "Enter the weight" :name "vol"
                                 :value 25 :class "txt" :required "required"))
                    (:td (:input :type "number" :name "dil"
                                 :value 1 :class "txt" :required "required"))))))))))

(define-easy-handler (sample-table :uri "/sample-table") (spectra-csv matrix)
  (cond ((null spectra-csv) (redirect "/select"))
        (t (let ((file-name (concatenate 'string *data-repository* (cadr spectra-csv))))
             (rename-file (car spectra-csv) file-name)
             (setf *spectra* (build-spectra-list file-name matrix))
             (setf *data-pathname* (pathname file-name))
             (standard-page (:title "Upload Complete")
               (:h3 "The file has been uploaded to the server")
               (:p "File location: " (str file-name) )
               (:p "Sample type: " (string-capitalize (str matrix)))
               (:h3 "Please complete the sample table to determine concentration")
               (:form :action "/plots" :method "post" :id "infoList"
                      (:ol
                       (make-sample-table *spectra*))
                      (:input :type "submit" :value "Submit" :class "btn")))))))

;; Changing the form cells
;;<input type="hidden" name="hiddenfield" value="text" />
;;<input type="text" name="rofield" value="text" readonly="readonly" />

(hunchentoot:define-easy-handler (bad-input :uri "/bad-input") ()
  (standard-page (:title "Error")
    (:h2 "Error in Submitted Sample Table")
    (:p "Please ensure that all parameters are completed correctly
    prior to submission")
    (:p "you can fill-in the table below or navigate back to the previous page")
    (:form :action "/plots" :method "post" :id "infoList"
           (:ol
            (make-sample-table *spectra*))
           (:input :type "submit" :value "Submit" :class "btn"))))

(defgeneric process-form (spectra-list parameters-a-list)
  (:documentation "Process the a-lists from the sample table"))

(defun process-entry (entry)
  "read in string entry and convert to float with error checking"
  (let ((read-entry (handler-case (with-input-from-string (in entry)
                                    (read in))
                      (end-of-file () nil))))
    (if (not (numberp read-entry))
        (redirect "/bad-input")
        (float read-entry))))

(defmethod process-form ((s urine-spectra-list) parameters-a-list)
  (let ((spectra-list (los s)))
    (loop for (id vol acid dil) on parameters-a-list by #'cddddr ;; I hope this works
       do (loop for spectra in spectra-list
             if (string= (id spectra) (cdr id))
             do (sample-size-info spectra (process-entry (cdr vol))
                                  (process-entry (cdr dil))
                                  (process-entry (cdr acid)))))))

(defmethod process-form ((s fecal-spectra-list) parameters-a-list)
  (let ((spectra-list (los s)))
    (loop for (id vol dil) on parameters-a-list by #'cdddr
       do (loop for spectra in spectra-list
             if (string= (id spectra) (cdr id))
             do (sample-size-info spectra (process-entry (cdr vol))
                                  (process-entry (cdr dil)))))))

;; Useful for debugging processing post-parameters
;; Make this form action
(hunchentoot:define-easy-handler (print-param :uri "/print-param") ()
  (let ((params (hunchentoot:post-parameters*)))
    (standard-page (:title "print params")
      (:p (str params))
      (:p (str (mapcar (lambda (x) (type-of (cdr x))) params))))))

(hunchentoot:define-easy-handler (plots :uri "/plots") ()
  (process-form *spectra* (hunchentoot:post-parameters*)) ;;post-parameters* is an a-list of parameters-a-list from the sample-table form
  (mapcar #'delete-file (directory (concatenate 'string *data-repository* "/*.png")))
  (plot-data *spectra*)
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
