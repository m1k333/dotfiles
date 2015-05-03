;;; -*-lisp-*-

;;; Initialize

;; *Package
(in-package :stumpwm)

;; Init file load path
(defvar *init-load-path*
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d")))
  "The directory where the other init files live.")

;; Load-init function
(defun load-init (filename)
  "Load a file FILENAME (without extension) from `*init-load-path*'."
  (let ((file (merge-pathnames *init-load-path* (concat filename ".lisp"))))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

;;; Settings

;; Shell
(setf *shell-program* (getenv "SHELL"))

;; Welcome message
(setf *startup-message*
      (format nil "Welcome ~a, ~a"
              (getenv "USER") (time-format "%c")))

;; Load other init files
(load-init "appearance")
(load-init "functions")
(load-init "keybindings")


;;; Run external stuff

;; Finish initializing; everything below this is only run ONCE
(unless (boundp '*initialized*)

  ;; Background processes
  (run-shell-command "feh --no-fehbg --bg-center ~/.stumpwm.d/wallpaper*")
  (run-shell-command "urxvtd -q -o -f")

  ;; Programs to start immediately
  ;;(run-shell-command "urxvtc")
  
  ;; Tell LISP that we've booted up once
  (defvar *initialized* t "Set to t if StumpWM has been started once."))
