;;;; porph-screen.lisp

(in-package :porph-screen)

;;; "porph-screen" goes here. Hacks and glory await!

;; Data Structures
(defstruct spectra "Key information about the spectra" id nm abs triangle net-abs)
(defstruct point "Cartesian co-ordinates" x y )
(defstruct triangle "The three points in an absorbance curve" base1 peak base2)

;;; Data Managment
(defparameter *test-file* #P "/Users/matthew/lisp/site/porph-screen/data/2014-08-14.csv")

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
                       :nm  (str-to-num nm)
                       :abs (str-to-num abs)))))

(defun parse-data (&optional (data-stream (clean-up)))
  "Read in the csv and parse the numbers"
  (let* ((sample-data
          (let ((accum nil))
            (dotimes (line 2 (reverse accum))
              (push (car (cl-csv:read-csv (read-line data-stream nil 'eol))) accum))))
         (ids (get-sample-names sample-data))
         (data-set (remove-empty (cl-csv:read-csv data-stream)))
         (rotated-data (rotate data-set))
         (spectra (list-of-spectra ids rotated-data)))
  spectra))

    ;;:map-fn
    ;; #'(lambda (line)

(defparameter *raw-data* (clean-up))

;; Master function to create populated spectra structs
(defparameter *spectra* (parse-data))

(defun create-output)
(dolist (spectra *spectra* spectra)
  (setf (spectra-triangle spectra) (find-triangle spectra))
  (setf (spectra-net-abs spectra) (net-abs spectra))
  (plot-data spectra))
