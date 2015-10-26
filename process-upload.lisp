;;; file process-upload.lisp

(in-package :porph-screen)

;;; Data Management
;;(defparameter *test-file* "/Users/matthew/lisp/site/porph-screen/data/FPORS 2014-09-04.csv")
(defparameter *data-repository* "/home/mpah/Data/")
(defparameter *test-file* (concatenate 'string *data-repository* "FPORS 2014-09-04.csv"))
(defparameter *db-file* (concatenate 'string *data-repository* "porph_screen.sqlite"))
;;(defparameter *data-repository* "/Users/matthew/lisp/site/porph-screen/data/")
(defparameter *data-pathname* nil "The local name of the raw data file")

;;; Data Processing
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



;;; Populate the objects
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
