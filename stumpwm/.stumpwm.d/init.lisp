;;; -*-lisp-*-

;;; Initialize

;; Package
(in-package :stumpwm)

;; Swank server
(require 'swank)
(swank:create-server :dont-close t)
;; Requires swank, slime, and quicklisp-slime-helper for emacs.

;; RC file load path
(defvar *rc-load-path*
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname) "~/.stumpwm.d")))
  "The directory where the other init/'rc' files live.")

;; Load-rc function
(defun load-rc (filename)
  "Load a file FILENAME (without extension) from `*rc-load-path*'."
  (let ((file (merge-pathnames *rc-load-path* (concat filename ".lisp"))))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

;; Modules load path
(setf *module-dir*
      (directory-namestring
        (truename (merge-pathnames *rc-load-path* "modules"))))

;; Populate module list
(init-load-path *module-dir*)

;;; Settings

;; Shell
(setf *shell-program* (getenv "SHELL"))

;; Welcome message
(setf *startup-message*
      (format nil "Welcome ~a, ~a"
              (getenv "USER") (time-format "%c")))

;; Load other init files
(load-rc "appearance")
(load-rc "functions")
(load-rc "keybindings")

;; Load modules
(load-module "screenshot") ;; Requires zpng
(load-module "surfraw") ;; Requires ...surfraw
(load-module "stumptray") ;; Requires xembed

;;; Run external stuff

;; Finish initializing; everything below this is only run ONCE
(defvar *initialized* nil "Set to t if StumpWM has been started once.")
(unless *initialized*

  ;; Background processes
  (run-shell-command "feh --no-fehbg --bg-center ~/.stumpwm.d/wallpaper*")
  (run-shell-command "urxvtd -q -o -f")

  ;; Programs to start immediately
  ;;(run-shell-command "urxvtc")

  ;; Tell LISP that we've booted up once
  (setf *initialized* t))
