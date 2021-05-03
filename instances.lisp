;;; instances.lisp

;;; Functions to create class instances
;;; Make instance functions
(defun make-urine-spectra (id nm ab)
  (make-instance 'urine-spectra
                 :id id
                 :nm nm
                 :ab ab))

(defun make-fecal-spectra (id nm ab)
  (make-instance 'fecal-spectra
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
