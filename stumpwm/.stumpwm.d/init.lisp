;;; -*-lisp-*-

;;; Initialize

;; Package
(in-package :stumpwm)

;; We haven't initialized yet
(defvar *initialized* nil "True if StumpWM has been started once.")

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

;; Load modules
(load-module "battery-portable")
(load-module "screenshot") ;; Requires zpng
(load-module "surfraw") ;; Requires ...surfraw
(load-module "stumptray") ;; Requires xembed
(load-module "swm-emacs")

;; Load other init files
(load-rc "appearance")
(load-rc "functions")
(load-rc "keybindings")
(load-rc "swank")

;;; Settings

;; Shell
(setf *shell-program* (getenv "SHELL"))

;; Welcome message
(setf *time-format-string-default* "%A %F %R")
(setf *startup-message*
      (format nil "Welcome ~a, ~a"
              (getenv "USER") (time-format *time-format-string-default*)))

;;; Run external stuff

;; Finish initializing; everything below this is only run ONCE
(unless *initialized*

  ;; Background processes
  (run-shell-command "feh --no-fehbg --bg-center ~/.stumpwm.d/wallpaper*")
  (run-shell-command "emacs --daemon")
  (run-shell-command "urxvtd -q -o -f")

  ;; Programs to start immediately
  ;;(run-shell-command "urxvtc")

  ;; Tell LISP that we've booted up once
  (setf *initialized* t))
