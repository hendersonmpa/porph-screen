;;; file data-base.lisp

(in-package :porph-screen)

;; Create tables from our view classes
;; Only the first time !!!!!
(defun create-tables (&optional (db-file *db-file*))
  (let ((db-connection (list db-file)))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (clsql:create-view-from-class 'urine-spectra :database db)
      (clsql:create-view-from-class 'fecal-spectra :database db))))

(defun update-tables (spectra-list-object &optional (db-file *db-file*))
  "Update database with spectra-objects"
  (let ((db-connection (list db-file))
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
