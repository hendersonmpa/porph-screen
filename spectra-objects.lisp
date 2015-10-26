;;; file spectra-objects.lisp

(in-package :porph-screen)

;;; Spectra Classes
(clsql:def-view-class spectra ()
  ((uid :db-kind :key :db-constraints :not-null
        :initarg :uid :type (string 100) :accessor id)
   (id :initarg :id :type (string 30) :accessor id)
   (nm :initarg :nm :type list :accessor nm)
   (ab :initarg :ab :type list :accessor ab)
   (bkgd :initarg :bkgd :type list :accessor bkgd)
   (net-ab :initarg :net-ab :type float :accessor net-ab)
   (matrix :initarg :matrix :type (string 10) :accessor matrix)
   (vol :initarg :vol :type float :accessor vol)
   (dil :initarg :dil :type float :accessor dil)
   (concentration :initarg :concentration :type integer :accessor concentration)
   (result :initarg :result :type (string 15) :accessor result)
   (interference :initarg :interference :type (string 1) :accessor interference)))

(clsql:def-view-class urine-spectra (spectra)
  ((matrix :initform "urine" :type (string 10))
   (acid :initarg :acid :type float :accessor acid)))

(clsql:def-view-class fecal-spectra (spectra)
  ((matrix :initform "fecal" :type (string 10))))

(defclass spectra-list ()
  ;; Parent spectra object
  ((los :initarg :los :accessor los)
   (matrix :initarg :matrix :accessor matrix)))

(defclass urine-spectra-list (spectra-list)
  ;; Urine spectra
  ((los :initarg :los :accessor los)
   (matrix :initform "urine")))

(defclass fecal-spectra-list (spectra-list)
  ;; Fecal spectra
  ((los :initarg :los :accessor los)
   (matrix :initform "fecal")))

;;; Make instance functions
(defun make-urine-spectra (id nm ab)
  (make-instance 'urine-spectra
                 :uid (concatenate 'string id "-" (write-to-string (local-time:now)))
                 :id id
                 :nm nm
                 :ab ab))

(defun make-fecal-spectra (id nm ab)
  (make-instance 'fecal-spectra
                 :uid (concatenate 'string id "-" (write-to-string (local-time:now)))
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
