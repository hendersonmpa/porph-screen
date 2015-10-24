;;; Comparison to user selected drop line and automated drop line

(in-package :porph-screen)

(clsql:locally-enable-sql-reader-syntax)

(defun select-urine-spectra ()
  (let ((db-connection (list *db-file*)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (clsql:select [*] :from [urine_spectra]
                    ;; :where [= [slot-value 'urine-spectra 'ID] "QC1"]
                    :order-by [id]
                    :result-types :auto
                    :limit 5
                    :flatp t
                    :field-names nil
                    :database db))))

(clsql:def-view-class triangle (spectra)
  ((base1-x :initarg :base1-x :type integer :accessor base1-x)
   (base1-y :initarg :base1-y :type float :accessor base1-y)
   (base2-x :initarg :base2-x :type integer :accessor base2-x)
   (base2-y :initarg :base2-y :type float :accessor base2-y)
   (peak-x :initarg :peak-x :type integer :accessor peak-x)
   (peak-y :initarg :peak-y :type float :accessor peak-y)
   (drop-net-ab :initarg :drop-net-ab :type float :accessor drop-net-ab)))

(defun make-urine-triangle (matrix acid uid id nm ab bkgd net-ab vol dil concentration result interference)
  (declare (ignore acid))
  (make-instance 'triangle
                 :matrix matrix
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

(defparameter *test-object*
  (let ((accum nil)
        (results (select-urine-spectra)))
    (dolist (row results (nreverse accum))
      (let ((object (apply #'make-urine-triangle row)))
        ;;(single-plot object)
        (push object accum )))))

;;(single-plot (car *test-object*))

;; User interface

(defun prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (parse-number:parse-number (read-line *query-io*)))


(defmethod prompt-for-points ((triangle-object triangle))
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


;; Create tables from our view classes
;; Only the first time !!!!!
(defun triangle-table (&optional (db-file *db-file*))
  (let ((db-connection (list db-file)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (clsql:create-view-from-class 'triangle :database db))))

(defun update-triangles (triangle-list &optional (db-file *db-file*))
  "Update database with spectra-objects"
  (let ((db-connection (list db-file)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (dolist (triangle triangle-list)
        (clsql:update-records-from-instance triangle :database db)))))
