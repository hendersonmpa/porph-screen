;;; file interpret.lisp

(in-package :porph-screen)

;;;Determine Concentration and Interpretation
;; C-c C-u undefine function
(defgeneric sample-size-info (spectra &optional amount dil acid)
  (:documentation
   "Add information about sample mass or volume for concentration
   calculations"))

(defmethod sample-size-info ((s urine-spectra) &optional amount dil acid)
  (setf (vol s) amount)
  (setf (acid s) acid)
  (setf (dil s) dil))

(defmethod sample-size-info ((s fecal-spectra) &optional amount dil acid)
  (declare (ignore acid))
  (setf (vol s) amount)
  (setf (dil s) dil))

(defparameter *urine-constant* 500 "(Net-Abs/Milimolar extinction coefficient * dil) * volume of urine in mL")

(defparameter *fecal-constant* 14.85 "Net-Abs * 14.85/Weight of sample = “X” nmol/g wet feces")

(defgeneric calculate-concentration (spectra &optional constant)
  (:documentation
   "Calculate the approximate concentration of porphyrins in the sample"))

;; 2 ml of patient urine used in every assay
;; the effect of a acid dilution is take into account via
;;(/ (+ 2000 a) 2000) d v (expt 10 3)
(defmethod calculate-concentration ((s urine-spectra) &optional (constant *urine-constant*))
  "mmol/L * volume in ml/d * 10^3  * dilution factor = nmol/d"
  (with-accessors ((v vol)
                   (a acid)
                   (d dil)
                   (n net-ab)
                   (c concentration)) s
    (let ((conc-per-day (* (/ n constant) (/ (+ 2000 a) 2000) v (expt 10 3) d)))
      (setf c (round conc-per-day)))))

(defmethod calculate-concentration ((s fecal-spectra) &optional (constant *fecal-constant*))
  (with-accessors ((v vol)
                   (d dil)
                   (n net-ab)
                   (c concentration)) s
    (let ((conc-per-day (* (/ (* n constant) v) 1000 d)))
      (setf c (round conc-per-day)))))

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
