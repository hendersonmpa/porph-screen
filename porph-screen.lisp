;;;; porph-screen.lisp

(in-package :porph-screen)

;;TODO Clean-up the graphs after the report generation
;; (mapcar #'delete-file (directory (concatenate 'string *data-repository* "/*.png"))
;;TODO Add declarations where needed
;;TODO Better baseline algorithm - two points on same side?
;;STARTED Create a printable report
;;TODO Connect to mysql a database
;;TODO Replace structures with objects and methods
;;   - Change the fecal sample table as part of this change
;;TODO Use vectors for the abs and nm data
;;TODO Improve the base-line detection algorithm

;;; "porph-screen" goes here. Hacks and glory await!

;; Data Structures
;; (defstruct spectra "Key information about the spectra"
;;            id nm abs bkgd net-abs matrix vol dil)
;; (defstruct point "Cartesian co-ordinates" x y )
;(defstruct triangle "The three points in an absorbance curve" base1 peak base2)

(defclass spectra ()
    ((id :initarg :id :accessor id)
     (nm :initarg :nm :accessor nm)
     (ab :initarg :ab :accessor ab)
     (bkgd :initarg :bkgd :accessor bkgd)
     (net-ab :initarg :net-ab :accessor net-ab)
     (matrix :initarg :matrix :accessor matrix)))

(defclass urine-spectra (spectra)
    ((matrix :initform "urine")
     (vol :initarg :vol :accessor vol)
     (dil :initarg :dil :accessor dil)))

(defclass fecal-spectra (spectra)
    ((matrix :initform "fecal")
     (vol :initarg :mass :accessor vol)))

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
(defparameter *test-file* "/home/mpah/lisp/site/porph-screen/data/FPORS 2014-09-04.csv")
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
                 :id id
                 :nm nm
                 :ab ab))

(defun make-fecal-spectra (id nm ab)
  (make-instance 'fecal-spectra
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

;;(parse-data2 "/home/mpah/lisp/site/porph-screen/data/UPORS_2014-09-15.csv")

(defun build-spectra-list (file-path matrix)
  "Master function to create a list of spectra structs: from csv file and sample matrix choice"
  (let ((spectra-list (parse-data file-path matrix))
        (accum nil))
    (dolist (spectra spectra-list (cond ((string= matrix "urine")
                                         (make-instance 'urine-spectra-list
                                                        :los accum))
                                        ((string= matrix "fecal")
                                         (make-instance 'fecal-spectra-list
                                                        :los accum))))
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

(defparameter *urine-constant* 1.1097)
(defparameter *fecal-constant* 14.85)

;; Fecal: Net-Abs * 14.85/Weight of sample = “X” nmol/g wet feces
;; Urine: Net-Abs * 1.1097 * dil * vol = nmol/d

(defgeneric calculate-concentration (spectra)
  (:documentation
   "Calculate the approximate concentration of porphyrins in the sample"))

(defmethod calculate-concentration ((s urine-spectra))
  (with-accessors ((v vol)
                   (d dil)
                   (n net-ab)) s
    (round (* n *urine-constant* d v))))

(defmethod calculate-concentration ((s fecal-spectra))
  (with-accessors ((m mass)
                   (n net-ab)) s
    (round (/ (* n *fecal-constant*) (/ m 1000)))))

;; Normal if concentration <60  nmol/d for 24 h collections
;; or <60 nmol/L for random specimens.
;; Borderline if concentration is between 60 and 100 nmol/d or nmol/L.
;; Append the footnote POR 3 which expands to “Quantitation to follow”.  Test request a
;; quantitative urine porphyrin.
;; Elevated if concentration is >100 nmol/d or nmol/L.  Append the footnote POR 3.
;; Test request a quantitative urine porphyrin.

(defgeneric classify-spectra (spectra)
  (:documentation
   "Determine Semi-quantitative results using the screening thresholds.
Return concentration and class in a list"))

(defmethod classify-spectra ((s urine-spectra))
  (let ((conc (calculate-concentration s)))
    (cond ((and (>= conc -10) (< conc 60))(list conc "Normal"))
          ((and (>= conc 60) (<= conc 100))(list conc "Borderline"))
          ((> conc 100)(list conc "Elevated"))
          (t (list conc "Interference?")))))

(defmethod classify-spectra ((s fecal-spectra))
  (let ((conc (calculate-concentration s)))
    (cond ((and (>= conc -10) (<= conc 28))(list conc "Normal"))
          ((> conc 28) (list conc "Elevated"))
          (t (list conc "Interference?")))))


;; (defparameter *spectra* (complete-spectra *test-file*))
;; Create a method to formate spectra output

(defgeneric print-results (spectra strm)
(:documentation  "Print the results of the spectra analysis"))

(defmethod print-results ((s spectra) strm)
  (with-accessors ((i id)
                   (m matrix)) s
    (let* ((results-list (classify-spectra s))
           (conc (first results-list))
           (result (second results-list)))
      (format strm "~A,~A,~A,~A~%" i m conc result))))

(defun results-csv (spectra-list &optional (data-pathname *data-pathname*))
  "Create a csv file of the results"
  (let* ((file-name (concatenate 'string
                                 "results_" (pathname-name (pathname data-pathname)) ".csv"))
         (out-file (merge-pathnames *data-repository* file-name )))
    (with-open-file (out out-file :direction :output
                         :if-exists :supersede)
      (dolist (spectra spectra-list)
        (print-results spectra out)))))

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
