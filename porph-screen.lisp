;;;; porph-screen.lisp

(in-package :porph-screen)

;;TODO Add declarations where needed
;;TODO Connect to mysql a database
;;TODO Use vectors for the abs and nm data

;;; "porph-screen" goes here. Hacks and glory await!

;; Data Structures
;; (defstruct spectra "Key information about the spectra"
;;            id nm abs bkgd net-abs matrix vol dil)
;; (defstruct point "Cartesian co-ordinates" x y )
;(defstruct triangle "The three points in an absorbance curve" base1 peak base2)

(clsql:def-view-class spectra ()
  ((uid
    :db-kind :key
    :db-constraints :not-null
    :initarg :uid
    :type (string 100)
    :accessor id)
   (id
    :initarg :id
    :type (string 30)
    :accessor id)
   (nm
    :initarg :nm
    :type list
    :accessor nm)
   (ab
    :initarg :ab
    :type list
    :accessor ab)
   (bkgd
    :initarg :bkgd
    :type list
    :accessor bkgd)
   (net-ab
    :initarg :net-ab
    :type float
    :accessor net-ab)
   (matrix
    :initarg :matrix
    :type (sting 10)
    :accessor matrix)
   (vol
    :initarg :vol
    :type integer
    :accessor vol)
   (concentration
    :initarg :concentration
    :type integer
    :accessor concentration)
   (result
    :initarg :result
    :type (string 15)
    :accessor result)))

(clsql:def-view-class urine-spectra (spectra)
  ((matrix
    :initform "urine")
   (dil
    :initarg :dil
    :type float
    :accessor dil)))

(clsql:def-view-class fecal-spectra (spectra)
  ((matrix
    :initform "fecal")))

;; (defclass spectra ()
;;     ((id :initarg :id :accessor id)
;;      (nm :initarg :nm :accessor nm)
;;      (ab :initarg :ab :accessor ab)
;;      (bkgd :initarg :bkgd :accessor bkgd)
;;      (net-ab :initarg :net-ab :accessor net-ab)
;;      (matrix :initarg :matrix :accessor matrix)
;;      (concentration :initarg :concentration :accessor concentration)
;;      (result :initarg :result :accessor result)))

;; (defclass urine-spectra (spectra)
;;     ((matrix :initform "urine")
;;      (vol :initarg :vol :accessor vol)
;;      (dil :initarg :dil :accessor dil)))

;; (defclass fecal-spectra (spectra)
;;     ((matrix :initform "fecal")
;;      (vol :initarg :mass :accessor vol)))

(defclass spectra-list ()
  ((los :initarg :los :accessor los)
   (matrix :initarg :matrix :accessor matrix)))

(defclass urine-spectra-list (spectra-list)
  ((los :initarg :los :accessor los)
   (matrix :initform "urine")))

(defclass fecal-spectra-list (spectra-list)
  ((los :initarg :los :accessor los)
   (matrix :initform "fecal")))

;;; Data Management
;;(defparameter *test-file* "/Users/matthew/lisp/site/porph-screen/data/FPORS 2014-09-04.csv")
(defparameter *test-file* "/home/mpah/lisp/site/porph-screen/data/FPORS 2014-09-04.csv")
;;(defparameter *data-repository* "/Users/matthew/lisp/site/porph-screen/data/")
(defparameter *data-repository* "/home/mpah/lisp/site/porph-screen/data/")
(defparameter *data-pathname* nil "The local name of the raw data file")

;; (defun clean-up (file-path)
;;   (let ((strm (make-array 0
;;                          :element-type 'character
;;                          :adjustable t
;;                          :fill-pointer 0)))
;;     (with-open-file (in file-path :direction :input)
;;       (loop for line = (read-line in nil 'eof)
;;          until (eq line 'eof)
;;          do (let ((new-line (string-trim '(#\, #\Space #\Tab #\Newline #\Return) line)))
;;               (format strm "~A~%" new-line))))
;;     (make-string-input-stream strm)))

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun remove-empty-lines (text-string)
  (cl-ppcre:regex-replace-all "^$" text-string ""))

(defun normalize-line-endings (text-string)
  "replaces all sorts of weird line endings with the standard cl line ending #\newline"
  (cl-ppcre:regex-replace-all "(,\\r|,\\n|\\r|\\n)+" text-string (string #\newline)))

(defun remove-trailing-comma (text-string)
    (cl-ppcre:regex-replace-all ",$" text-string ""))

(defun clean-up (file-path)
  (with-open-file (in file-path :direction :input)
    (let* ((raw-text (slurp-stream in))
           (line-endings (normalize-line-endings raw-text))
           (removed-empty (remove-empty-lines line-endings)))
      removed-empty)))

(defun get-sample-names (id-line)
  (let ((ids nil))
    (dolist (entry id-line (reverse ids))
      (cond ((not (equalp entry ""))(push entry ids))
            (t nil)))))

(defun rotate (list-of-lists)
"Matrix transpose a list of lists so that column-major data becomes row major."
  (apply #'mapcar #'list list-of-lists))

(defun make-urine-spectra (id nm ab)
  (make-instance 'urine-spectra
                 :uid (concatenate 'string id "-" (write-to-string (local-time:now)))
                 :id id
                 :nm nm
                 :ab ab))

(defun make-fecal-spectra (id nm ab)
  (make-instance 'fecal-spectra
                 :uid (concatenate 'string id "-" (write-to-string (local-time:now)))
                 :id id
                 :nm nm
                 :ab ab))

(defun make-fecal-spectra-list (ids rotated-data)
  (labels ((str-to-num (line)
             (mapcar #'parse-number line)))
    (loop for (nm ab) on rotated-data by #'cddr
       for id in ids collect
         (make-fecal-spectra id
                             (reverse (str-to-num nm))
                             (reverse (str-to-num ab))))))

(defun make-urine-spectra-list (ids rotated-data)
  (labels ((str-to-num (line)
             (mapcar #'parse-number line)))
    (loop for (nm ab) on rotated-data by #'cddr
       for id in ids collect
         (make-urine-spectra id
                             (reverse (str-to-num nm))
                             (reverse (str-to-num ab))))))

(defun parse-data (file-path matrix)
  "Read in the csv and parse the numbers"
  (let* ((data (clean-up file-path))
         (data-set (cl-csv:read-csv data))
         (id-line (car data-set))
         (ids (get-sample-names id-line))
         (rotated-data (rotate (cddr data-set))))
    (cond ((string= matrix "urine")
           (make-urine-spectra-list ids rotated-data))
          ((string= matrix "fecal")
           (make-fecal-spectra-list ids rotated-data)))))

(defun build-spectra-list (file-path matrix)
  "Master function to create a list of spectra objects: from csv file and sample matrix choice"
  (let ((spectra-list (parse-data file-path matrix))
        (accum nil))
    (dolist (spectra spectra-list (cond ((string= matrix "urine")
                                         (make-instance 'urine-spectra-list
                                                        :los (reverse accum)))
                                        ((string= matrix "fecal")
                                         (make-instance 'fecal-spectra-list
                                                        :los (reverse accum)))))
      (setf (net-ab spectra) (find-net-ab spectra))
      (setf (matrix spectra) matrix)
      (push spectra accum))))

(defgeneric sample-size-info (spectra &optional amount dil)
  (:documentation
   "Add information about sample mass or volume for concentration
   calculations"))

(defmethod sample-size-info ((s urine-spectra) &optional amount dil)
  (setf (vol s) amount)
  (setf (dil s) dil))

(defmethod sample-size-info ((s fecal-spectra) &optional amount dil)
  (declare (ignore dil))
    (setf (vol s) amount))

(defparameter *urine-constant* 500 "(Net-Abs/Milimolar extinction coefficient * dil) * volume of urine in mL")

(defparameter *fecal-constant* 14.85 "Net-Abs * 14.85/Weight of sample = “X” nmol/g wet feces")

(defgeneric calculate-concentration (spectra constant)
  (:documentation
   "Calculate the approximate concentration of porphyrins in the sample"))

(defmethod calculate-concentration ((s urine-spectra) &optional (constant *urine-constant*))
  "mmol/L * volume in ml/d * 10^3 = nmol/d"
  (with-accessors ((v vol)
                   (d dil)
                   (n net-ab)) s
    (let ((conc-per-day (* (/ n constant) dil (expt 10 3)))))
    (setf (concentration s) (round conc-per-day))))

(defmethod calculate-concentration ((s fecal-spectra) &optional (constant *fecal-constant*))
  (with-accessors ((v vol)
                   (n net-ab)) s
    (let ((conc-per-day (/ (* n constant) (/ v 1000)) )))
    (setf (concentration s) (round conc-per-day))))

;; Normal if concentration <110  nmol/d for 24 h collections
;; or <110 nmol/L for random specimens.
;; Borderline if concentration is between 110 and 200 nmol/d or nmol/L.
;; Append the footnote POR 3 which expands to “Quantitation to follow”.  Test request a
;; quantitative urine porphyrin.
;; Elevated if concentration is >200 nmol/d or nmol/L.  Append the footnote POR 3.
;; Test request a quantitative urine porphyrin.

(defgeneric classify-spectra (spectra)
  (:documentation
   "Determine Semi-quantitative results using the screening thresholds.
Return concentration and class in a list"))

(defmethod classify-spectra ((s urine-spectra))
  (calculate-concentration s)
  (let ((conc (concentration s)))
    (cond ((and (>= conc -10) (< conc 110))(setf (result s) "Normal") s)
          ((and (>= conc 110) (<= conc 200))(setf (result s) "Borderline") s)
          ((> conc 200)(setf (result s) "Elevated") s)
          (t (setf (result s) "Interference") s))))

(defmethod classify-spectra ((s fecal-spectra))
  (calculate-concentration s)
  (let ((conc (concentration s)))
    (cond ((and (>= conc -10) (<= conc 28))
           (setf (result s) "Normal") s)
          ((> conc 28) (setf (result s) "Elevated") s)
          (t (setf (result s) "Interference") s))))

;; (defparameter *spectra* (complete-spectra *test-file*))
;; Create a method to formate spectra output

(defgeneric print-results (spectra strm)
(:documentation  "Print the results of the spectra analysis"))

(defmethod print-results ((s spectra) strm)
  (with-accessors ((i id)
                   (m matrix)
                   (c concentration)
                   (r result)) s
          (format strm "~A,~A,~A,~A~%" i m c r)))

(defun results-csv (spectra-list-object &optional (data-pathname *data-pathname*))
  "Create a csv file of the results"
  (let* ((file-name (concatenate 'string
                                 "results_" (pathname-name (pathname data-pathname)) ".csv"))
         (spectra-list (los spectra-list-object))
         (out-file (merge-pathnames *data-repository* file-name )))
    (with-open-file (out out-file :direction :output
                         :if-exists :supersede)
      (dolist (spectra spectra-list)
        (print-results spectra out)))))

;; Create tables from our view classes
;; Only the first time !!!!!
;; (defun create-tables (&optional (location *data-repository*)
;;                         (name "porph_screen.sqlite"))
;;   (let ((db-connection (list (concatenate 'string location name))))
;;     (clsql:with-database (db db-connection
;;                              :database-type :sqlite3)
;;       (clsql:create-view-from-class 'urine-spectra :database db)
;;       (clsql:create-view-from-class 'fecal-spectra :database db))))

(defun update-tables (spectra-list-object &optional (location *data-repository*)
                        (name "porph_screen.sqlite"))
  "Update database with spectra-objects"
  (let ((db-connection (list (concatenate 'string location name)))
        (spectra-list (los spectra-list-object)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (dolist (spectra spectra-list)
        (clsql:update-records-from-instance spectra :database db)))))

;; (defun print-spectra-list (spectra-list &optional (data-pathname *data-pathname*))
;;   "Print the spectra list to file"
;;   (let* ((file-name (concatenate 'string
;;                                  "spectra_" (pathname-name (pathname data-pathname))))
;;          (out-file (merge-pathnames *data-repository* file-name )))
;;     (with-open-file (out out-file :direction :output
;;                          :if-exists :supersede)
;;       (print spectra-list out))))

;; (defun read-spectra-list (file-name)
;;   "Read the spectra list from file"
;;   (let ((in-file (merge-pathnames *data-repository* file-name )))
;;     (with-open-file (in in-file :direction :input
;;                          :if-does-not-exist nil)
;;       (read in in-file))))
