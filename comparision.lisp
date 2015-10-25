;;; Comparison to user selected drop line and automated drop line

(in-package :porph-screen)

(clsql:file-enable-sql-reader-syntax)

(defun select-urine-spectra (n)
  (let ((db-connection (list *db-file*)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      ;; TODO: Create query that only gets new spectra i.e join the triangle table
      (clsql:select [*] :from [urine_spectra]
                                        ; :where [= [slot-value 'urine-spectra 'dn-conc] "QC1"]
                    :order-by [id]
                    :result-types :auto
                    :limit n
                    :flatp t
                    :field-names nil
                    :database db))))

(clsql:def-view-class triangle (spectra)
  ((matrix :initarg :matrix :type string :accessor matrix)
   (base1-x :initarg :base1-x :type integer :accessor base1-x)
   (base1-y :initarg :base1-y :type float :accessor base1-y)
   (base2-x :initarg :base2-x :type integer :accessor base2-x)
   (base2-y :initarg :base2-y :type float :accessor base2-y)
   (peak-x :initarg :peak-x :type integer :accessor peak-x)
   (peak-y :initarg :peak-y :type float :accessor peak-y)
   (drop-net-ab :initarg :drop-net-ab :type float :accessor drop-net-ab)
   (dn-conc :initarg :dn-conc :type integer :accessor dn-conc)))

(clsql:def-view-class urine-triangle (triangle urine-spectra)
  ())

(defun make-urine-triangle (matrix acid uid id nm ab bkgd net-ab vol dil concentration result interference)
  ;; TODO: Modify slots so that they are the same as urine-spectra, then can use those methods.
  ;; (declare (ignore acid))
  (make-instance 'urine-triangle
                 :matrix matrix
                 :acid acid
                 :uid uid
                 :id id
                 :nm (read-from-string nm) ; Need to read string to list for nm ab and bkgd
                 :ab (read-from-string ab)
                 :bkgd (read-from-string bkgd)
                 :net-ab net-ab
                 :vol vol
                 :dil dil
                 :concentration concentration
                 :result result
                 :interference interference))

;; (single-plot (car *test-object*))
(defun get-objects (&optional (n 10))
  (let ((accum nil)
        (results (select-urine-spectra n)))
    (dolist (row results (nreverse accum))
      (let ((object (apply #'make-urine-triangle row)))
        ;; (single-plot object)
        (push object accum )))))

;; Methods to calculate concentration
(defmethod slope ((triangle-object triangle))
  "What is the slope of the line between base1 and base2"
  (with-accessors ((b1x base1-x)
                   (b1y base1-y)
                   (b2x base2-x)
                   (b2y base2-y)) triangle-object
    (/ (- b1y b2y)
       (- b1x b2x))))

;; Find the tangent line to the absorbance curve
;; y-y1 = m(x-x1)
;; y = m(x-x1) + y1
;; b = m(-x1) + y1
(defmethod intercept ((triangle-object triangle))
  "The intercept of the line"
  (with-accessors ((b1x base1-x)
                   (b1y base1-y)) triangle-object
    (let ((slope (slope triangle-object)))
      (+ b1y (* slope (- b1x))))))

;; (intercept base1 base2 (slope base1 base2))

;; Given x for the peak interpolate point on the baseline
;; result --> point
(defmethod interpolate ((triangle-object triangle))
  (with-accessors ((px peak-x)) triangle-object
    (let ((m (slope triangle-object))
          (b (intercept triangle-object)))
      (+ b (* m px)))))

;; (interpolate peak base1 base2)
(defmethod drop-net-ab ((triangle-object triangle))
  (with-accessors ((py peak-y)
                   (ab drop-net-ab)) triangle-object
    (let* ((drop-line-y (interpolate triangle-object)))
      (setf ab (- py drop-line-y)))))
;; (setf (drop-net-ab (car *test-object*))  0.03125)

;; 2 ml of patient urine used in every assay
;; the effect of a acid dilution is take into account via
;; (/ (+ 2000 a) 2000) d v (expt 10 3)
;; TODO: May not need if urine-triangle inherts from urine-spectra
(defmethod calculate-concentration ((s urine-triangle) &optional (constant *urine-constant*))
  "mmol/L * volume in ml/d * 10^3  * dilution factor = nmol/d"
  (with-accessors ((v vol)
                   (a acid)
                   (d dil)
                   (n drop-net-ab)
                   (c dn-conc))
    (let ((conc-per-day (* (/ n constant) (/ (+ 2000 a) 2000) v (expt 10 3) d)))
      (setf c (round conc-per-day)))))

;; User interface
(defunq prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (parse-number:parse-number (read-line *query-io*)))

(defmethod prompt-for-points ((triangle-object urine-triangle))
  (with-accessors ((b1x base1-x)
                   (b1y base1-y)
                   (b2x base2-x)
                   (b2y base2-y)
                   (px peak-x)
                   (py peak-y)) triangle-object
    (single-plot triangle-object)
    (setf b1x (prompt-read "Base 1 x"))
    (setf b1y (prompt-read "Base 1 y"))
    (setf b2x (prompt-read "Base 2 x"))
    (setf b2y (prompt-read "Base 2 y"))
    (setf px (prompt-read "Peak x"))
    (setf py (prompt-read "Peak y"))))

(defun review-spectra ()
  (let ((response (y-or-n-p "Do you want to review some spectra")))
    (unless (null response)
      (let ((spectra-list (get-objects 1)))
        (dolist (spectra spectra-list)
          (prompt-for-points spectra)
          (drop-net-ab spectra)
          (calculate-concentration spectra)
          (update-triangle-table spectra)))
      (review-spectra))))

;; Create tables from our view classes
;; Only the first time !!!!!
(defun triangle-table (&optional (db-file *db-file*))
  (let ((db-connection (list db-file)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (clsql:create-view-from-class 'urine-triangle :database db))))

(defun update-triangle-table (triangle &optional (db-file *db-file*))
  "Update database with spectra-objects"
  (let ((db-connection (list db-file)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (clsql:update-records-from-instance triangle :database db))))
