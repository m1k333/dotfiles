;;; MSR 2015
;;; Start the swank server!

;; Package
(in-package :stumpwm)

;; Require swank server
(require 'swank)
;; Requires swank, slime, and quicklisp-slime-helper for emacs.

;; Is swank running?
(defvar *swank-running* nil "True if swank is running in StumpWM.")

;; Toggle swank on or off
(defcommand swank-server () ()
  "Toggle the swank server on or off."
  (if *swank-running*
      (progn
        (swank:stop-server 4005)
        (echo-string (current-screen) "Stopping swank.")
        (setf *swank-running* nil))
      (progn
        (swank:create-server :port 4005 :dont-close t)
        (echo-string (current-screen) "Starting swank.")
        (setf *swank-running* t))))

;; Start swank
(swank-server)
