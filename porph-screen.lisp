;;;; porph-screen.lisp

(in-package :porph-screen)

;;; "porph-screen" goes here. Hacks and glory await!

;; Data Structures
(defstruct spectra "Key information about the spectra"
           id nm abs triangle net-abs matrix vol dil)
(defstruct point "Cartesian co-ordinates" x y )
(defstruct triangle "The three points in an absorbance curve" base1 peak base2)

;;; Data Managment
;;(defparameter *test-file* "/Users/matthew/lisp/site/porph-screen/data/2014-08-14.csv")
(defparameter *data-repository* "/Users/matthew/lisp/site/porph-screen/data/")
(defparameter *data-pathname* nil "The local name of the raw data file")

(defun clean-up (&optional (file *test-file*))
  (let ((strm (make-array 0
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0)))
    (with-open-file (in file :direction :input)
      (loop for line = (read-line in nil 'foo)
         until (eq line 'foo)
         do (let ((new-line (string-trim '(#\, #\Space #\Tab #\Newline #\Return) line)))
              (format strm "~A~%" new-line))))
    (make-string-input-stream strm)))

(defun get-sample-names (sample-data)
  (let ((id-line (car sample-data))
        (ids nil))
    (dolist (entry id-line (reverse ids))
      (cond ((not (equalp entry ""))(push entry ids))
            (t nil)))))

(defun remove-empty (lol)
  (let ((accum nil))
    (dolist (row lol (reverse accum))
      (cond ((not (equalp row '(""))) (push row accum))
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
  (let* ((data-stream (clean-up file-path))
         (sample-data
          (let ((accum nil))
            (dotimes (line 2 (reverse accum))
              (push (car (cl-csv:read-csv (read-line data-stream nil 'eol))) accum))))
         (ids (get-sample-names sample-data))
         (data-set (remove-empty (cl-csv:read-csv data-stream)))
         (rotated-data (rotate data-set))
         (spectra (list-of-spectra ids rotated-data)))
    spectra))


;; Master function to create populated spectra structs
(defun build-spectra (file-path matrix)
  (let ((spectra-list (parse-data file-path))
        (accum nil))
    (dolist (spectra spectra-list (reverse accum))
      (setf (spectra-triangle spectra) (find-triangle spectra))
      (setf (spectra-net-abs spectra) (net-abs spectra))
      (setf (spectra-matrix spectra) matrix)
      (push spectra accum))))

(defun add-info (spectra vol dil)
  (setf (spectra-vol spectra) vol)
  (setf (spectra-dil spectra) dil))

;; (defparameter *spectra* (complete-spectra *test-file*))

(defun results-csv (spectra-list &optional (data-pathname *data-pathname*))
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
;;;;; Calculation of
;; 2 x Dmax - (D1 + D2) = corrected O.D. x 1.1097 x 1.05 (dilution
;; factor) = nmol/mL x total volume in mL = nmol/d.  If the total volume
;; of concentrated acid added is 100 μL then the dilution factor is
;; 1.05. (If 200 μL concentrated acid is added the dilution factor is
;; 1.10, 300 μL is 1.15, etc.). For random urine specimens and controls
;; the volume is 1000 mL.
