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
(defstruct spectra "Key information about the spectra"
           id nm abs bkgd net-abs matrix vol dil)
(defstruct point "Cartesian co-ordinates" x y )
;(defstruct triangle "The three points in an absorbance curve" base1 peak base2)

(defclass spectra ()
    ((id :initarg :id :accessor id)
        (nm :initarg :nm :accessor nm)
        (abs :initarg :abs :accessor abs)
        (bkgd :initarg :bkgd :accessor bkgd)
        (matrix :initarg :matrix :reader matrix)))

(defclass urine-spectra (spectra)
    ((matrix :initform "urine" :allocation :class)
        (vol :initarg :vol :accessor vol)
        (dil :initarg :dil :accessor dil)))

(defclass fecal-spectra (spectra)
    ((matrix :initform "fecal" :allocation :class)
        (mass :initarg :mass :accessor mass)))

;;; Data Management
;;(defparameter *test-file* "/Users/matthew/lisp/site/porph-screen/data/2014-08-14.csv")
(defparameter *data-repository* "/home/mpah/lisp/site/porph-screen/data/")
(defparameter *data-pathname* nil "The local name of the raw data file")

(defun clean-up (file-path)
  (let ((strm (make-array 0
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0)))
    (with-open-file (in file-path :direction :input)
      (loop for line = (read-line in nil 'eof)
         until (eq line 'eof)
         do (let ((new-line (string-trim '(#\, #\Space #\Tab #\Newline #\Return) line)))
              (format strm "~A~%" new-line))))
    (make-string-input-stream strm)))

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

;; (defparameter *text-string*
;;   (with-open-file (in "/home/mpah/lisp/site/porph-screen/data/UPORS_2014-09-15.csv"
;;                       :direction :input)
;;    (slurp-stream in)))

(defun remove-empty-lines (text-string)
  (cl-ppcre:regex-replace-all "^$" text-string ""))

(defun normalize-line-endings (text-string)
  "replaces all sorts of weird line endings with the standard cl line ending #\newline"
  (cl-ppcre:regex-replace-all "(\\r|\\n)+" text-string (string #\newline)))

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

(defun list-of-spectra (ids rotated-data)
  (labels ((str-to-num (line)
             (mapcar #'parse-number line)))
    (loop for (nm abs) on rotated-data by #'cddr
       for id in ids collect
         (make-spectra :id id
                       :nm  (reverse (str-to-num nm))
                       :abs (reverse (str-to-num abs))))))

(defun parse-data (file-path)
  "Read in the csv and parse the numbers"
  (let* ((data (clean-up file-path))
         (data-set (cl-csv:read-csv data))
         (id-line (car data-set))
         (ids (get-sample-names id-line))
         (rotated-data (rotate (cddr data-set)))
         (spectra (list-of-spectra ids rotated-data)))
    spectra))
;;(parse-data2 "/home/mpah/lisp/site/porph-screen/data/UPORS_2014-09-15.csv")

(defun build-spectra (file-path matrix)
  "Master function to create a list of spectra structs: from csv file and sample matrix choice"
  (let ((spectra-list (parse-data file-path))
        (accum nil))
    (dolist (spectra spectra-list (reverse accum))
      ;(setf (spectra-triangle spectra) (find-triangle spectra))
      (setf (spectra-net-abs spectra) (net-abs spectra))
      (setf (spectra-matrix spectra) matrix)
      (push spectra accum))))

(defun add-info (spectra vol dil)
  (setf (spectra-vol spectra) vol)
  (setf (spectra-dil spectra) dil))

(defparameter *urine-constant* 1.1097)
(defparameter *fecal-constant* 14.85)

;; Fecal: Net-Abs * 14.85/Weight of sample = “X” nmol/g wet feces
;; Urine: Net-Abs * 1.1097 * dil * vol = nmol/d
(defun concentration (spectra-struct &optional (urine-constant *urine-constant*)
                                       (fecal-constant *fecal-constant*))
  (let* ((matrix (spectra-matrix spectra-struct))
         (vol (spectra-vol spectra-struct))
         (dil (spectra-dil spectra-struct))
         (net-abs (spectra-net-abs spectra-struct)))
    (cond ((string= "urine" matrix)
           (round (* net-abs urine-constant dil vol)))
          ((string= "fecal" matrix)
           (round (/ (* net-abs fecal-constant) (/ vol 1000))))
          (t (print "No matrix provided")))))

;; Normal if concentration <110 nmol/d for 24 h collections
;; or <110 nmol/L for random specimens.
;; Borderline if concentration is between 110 and 200 nmol/d or nmol/L.
;; Append the footnote POR 3 which expands to “Quantitation to follow”.  Test request a
;; quantitative urine porphyrin.
;; Elevated if concentration is >200 nmol/d or nmol/L.  Append the footnote POR 3.
;; Test request a quantitative urine porphyrin.

(defun results (spectra-struct)
  (let ((matrix (spectra-matrix spectra-struct))
        (conc (concentration spectra-struct)))
    (cond ((string= "urine" matrix)
           (cond ((and (>= conc -10) (< conc 110))(list conc "Normal"))
                 ((and (>= conc 110) (<= conc 200))(list conc "Borderline"))
                 ((> conc 200)(list conc "Elevated"))
                 (t (list conc "Interference?"))))
          ((string= "fecal" matrix)
           (cond ((and (>= conc -10) (<= conc 35))(list conc "Normal"))
                 ((> conc 35) (list conc "Elevated"))
                 (t (list conc "Interference?")))))))

;; (defparameter *spectra* (complete-spectra *test-file*))
(defun results-csv (spectra-list &optional (data-pathname *data-pathname*))
  "Create a csv file of the results"
  (let* ((file-name (concatenate 'string
                                 "results_" (pathname-name (pathname data-pathname)) ".csv"))
         (out-file (merge-pathnames *data-repository* file-name )))
    (with-open-file (out out-file :direction :output
                         :if-exists :supersede)
      (dolist (spectra spectra-list)
        (let* ((id (spectra-id spectra))
               (matrix (spectra-matrix spectra))
               (results-list (results spectra))
               (conc (first results-list))
               (result (second results-list)))
          (format out "~A,~A,~A,~A~%" id matrix conc result))))))

(defun print-spectra-list (spectra-list &optional (data-pathname *data-pathname*))
  "Print the spectra list to file"
  (let* ((file-name (concatenate 'string
                                 "spectra_" (pathname-name (pathname data-pathname))))
         (out-file (merge-pathnames *data-repository* file-name )))
    (with-open-file (out out-file :direction :output
                         :if-exists :supersede)
      (print spectra-list out))))

(defun read-spectra-list (file-name)
  "Read the spectra list from file"
  (let ((in-file (merge-pathnames *data-repository* file-name )))
    (with-open-file (in in-file :direction :input
                         :if-does-not-exist nil)
      (read in in-file))))



;;;;; Calculation of
;; 2 x Dmax - (D1 + D2) = corrected O.D. x 1.1097 x 1.05 (dilution
;; factor) = nmol/mL x total volume in mL = nmol/d.  If the total volume
;; of concentrated acid added is 100 μL then the dilution factor is
;; 1.05. (If 200 μL concentrated acid is added the dilution factor is
;; 1.10, 300 μL is 1.15, etc.). For random urine specimens and controls
;; the volume is 1000 mL.
