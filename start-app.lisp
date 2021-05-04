;;;$ sbcl --noinform --load "start-app.lisp"
;; called by start-app.sh

;;load system
(asdf:load-system :porph-screen)
;; create *data-repository* if it doesn't exist
(ensure-directories-exist porph-screen::*data-repository*)
;; check for database file and make if absent
(porph-screen::confirm-db-exists)
;; start server
(porph-screen::start-server)

;; To keep the server running.
(defvar *alive* t)
(loop (sleep 1000) (if (not *alive*) (quit)))

;; (load "/home/mpah/lisp/swank-image.lisp")
;; (load "~/quicklisp/setup.lisp")
;; (ql:quickload :porph-screen)
;; (porph-screen::start-server)
