;;; file comparison.lisp
;;; Comparison to user selected drop line and automated drop line

(in-package :porph-screen)

(clsql:file-enable-sql-reader-syntax)

(defun select-urine-spectra (n)
  (let ((db-connection (list *db-file*)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (let ((done (clsql:select [id] :from [urine_triangle]
                                :order-by [id]
                                :result-types :auto
                                :flatp t
                                :field-names nil
                                :database db )))
        (clsql:select [*] :from [urine_distinct]
                      :where [and [not [in [id] done]]
                      [not [like [id] "QC%"]]] ;;[not [in [id] done]] ;; [in [id] done] ;;
                      :order-by [id]
                      :result-types :auto
                      :distinct t
                      :limit n
                      :flatp t
                      :field-names nil
                      :database db)))))
;; (let ((db-connection (list *db-file*)))
;;   (clsql:with-database (db db-connection
;;                            :database-type :sqlite3)
;;     (clsql:select [id] :from [urine_triangle]
;;                   :order-by [id]
;;                   :result-types :auto
;;                   :flatp t
;;                   :field-names nil
;;                   :database db )))

(clsql:def-view-class triangle ()
  ((base1-x :initarg :base1-x :type integer :accessor base1-x)
   (base1-y :initarg :base1-y :type float :accessor base1-y)
   (base2-x :initarg :base2-x :type integer :accessor base2-x)
   (base2-y :initarg :base2-y :type float :accessor base2-y)
   (peak-x :initarg :peak-x :type integer :accessor peak-x)
   (peak-y :initarg :peak-y :type float :accessor peak-y)))


(defun make-triangle (b1x b1y b2x b2y px py)
  (make-instance 'triangle
                 :base1-x b1x
                 :base1-y b1y
                 :base2-x b2x
                 :base2-y b2y
                 :peak-x px
                 :peak-y py))

(clsql:def-view-class urine-triangle (triangle urine-spectra)
  ())

(defun make-urine-triangle (matrix acid id datetime nm ab bkgd net-ab vol dil concentration result interference)
  (declare (ignore datetime net-ab concentration result interference))
  "Ignore the snip based results and will replace with drop line results"
  (make-instance 'urine-triangle
                 :matrix matrix
                 :acid acid
                 :id id
                 :nm (read-from-string nm)
                 :ab (read-from-string ab)
                 :bkgd (read-from-string bkgd)
                 :vol vol
                 :dil dil))

;; (single-plot (car *test-object*))
(defun get-objects (&optional (n 10))
  "Query the db and create a list of urine-triangle objects"
  (let ((accum nil)
        (results (select-urine-spectra n)))
    (dolist (row results (nreverse accum))
      (let ((object (apply #'make-urine-triangle row)))
        ;; (single-plot object)
        (push object accum )))))

;;; Update the Database
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


;; (make-triangle 390 .68 420 0.4 405 1.04)
;;; Methods to calculate concentration
(defmethod slope ((triangle-object triangle))
  "Find the slope of the line between base1 and base2"
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
  "Find the intercept of the line"
  (with-accessors ((b1x base1-x)
                   (b1y base1-y)) triangle-object
    (let ((slope (slope triangle-object)))
      (+ b1y (* slope (- b1x))))))

;; (intercept base1 base2 (slope base1 base2))
;; Given x for the peak interpolate point on the baseline
;; result --> point
(defmethod interpolate ((triangle-object triangle))
  "Find the point on the drop line at peak-x"
  (with-accessors ((px peak-x)) triangle-object
    (let ((m (slope triangle-object))
          (b (intercept triangle-object)))
      (+ b (* m px)))))

;; (interpolate peak base1 base2)
(defmethod drop-net-ab ((triangle-object triangle))
  (with-accessors ((py peak-y)
                   (ab net-ab)) triangle-object
    (let* ((drop-line-y (interpolate triangle-object)))
      (setf ab (- py drop-line-y)))))

;;; User interface
(defun prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (parse-number:parse-number (read-line *query-io*)))

(defmethod prompt-for-points ((triangle-object urine-triangle))
  (with-accessors ((id id)
                   (b1x base1-x)
                   (b1y base1-y)
                   (b2x base2-x)
                   (b2y base2-y)
                   (px peak-x)
                   (py peak-y)) triangle-object
    (setf b1x (prompt-read "Base 1 x"))
    (setf b1y (prompt-read "Base 1 y"))
    (setf b2x (prompt-read "Base 2 x"))
    (setf b2y (prompt-read "Base 2 y"))
    (setf px (prompt-read "Peak x"))
    (setf py (prompt-read "Peak y"))))

(defun review-spectra (&optional (n 5))
  "Master loop to review the plots"
  (let ((response (y-or-n-p "Do you want to review ~A spectra" n)))
    (unless (null response)
      (let ((spectra-list (get-objects n)))
        (dolist (spectra spectra-list)
          (format t "Sample ID: ~A~%" (id spectra))
          (single-plot spectra)
          (if (y-or-n-p "Is this as repeat?")
              (format t "Skipped, next..~%")
              (let ((peak-p (y-or-n-p "Is there a peak?")))
                (cond (peak-p
                       (prompt-for-points spectra)
                       (drop-net-ab spectra)
                       (calculate-concentration spectra))
                      ((not peak-p)
                       (setf (net-ab spectra) 0.0)
                       (setf (concentration spectra) 0)))))
          (update-triangle-table spectra)))
      (fresh-line)
      (review-spectra n))))
