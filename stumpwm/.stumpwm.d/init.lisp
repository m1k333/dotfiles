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
(load-module "screenshot") ;; Requires zpng
(load-module "surfraw") ;; Requires ...surfraw
;;(load-module "battery-portable")
;;(load-module "stumptray") ;; Requires xembed

;;; Appearance

;; Appearance settings
(setf *maxsize-border-width*   1
      *normal-border-width*    1
      *transient-border-width* 1
      *window-border-style*    :thin)

;; Mode-line
(setf *time-modeline-string*    "%A %F %R"
      *screen-mode-line-format* "[^B%n^b] %W ^B||^b %d")

;; Wallpaper
(unless *initialized*
  (run-shell-command "feh --no-fehbg --bg-tile ~/.stumpwm.d/wallpaper.jpg"))

;;; Functions

;; cat (UNIX-style function)
(defun cat (&rest strings)
  "Concatenates strings, like the Unix command `cat'."
  (apply 'concatenate 'string strings))

;; Interactive `colon' command; the args are optional initial contents
(defcommand colon1 (&optional (initial "")) (:rest)
            (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
              (when cmd
                (eval-command cmd t))))

;; ACPI info
(defcommand battery () ()
  (echo-string (current-screen) (run-shell-command "acpi -V" t)))

;; ALSA sound info
(defcommand audio () ()
  (echo-string (current-screen) (run-shell-command "amixer get Master" t)))
(defcommand volup () ()
  (run-shell-command "amixer set Master 5%+"))
(defcommand voldown () ()
  (run-shell-command "amixer set Master 5%-"))
(defcommand voltoggle () ()
  (run-shell-command "amixer set Master toggle"))

;; Emacsclient run or raise
(defcommand emacsclient () ()
            "Run emacsclient, or if it is already open, raise its window."
            (run-or-raise "emc" '(:class "Emacs")))

;; Firefox run or raise
(defcommand firefox () ()
            "Run Firefox, or if it is already open, raise its window."
            (run-or-raise "firefox" '(:class "Firefox")))

;; Thunderbird run or raise
(defcommand thunderbird () ()
            "Run Thunderbird, or if it is already open, raise its window."
            (run-or-raise "thunderbird" '(:class "Thunderbird")))

;; Urxvtd launcher
(defcommand urxvtd () ()
            "Launch the urxvt daemon."
            (run-shell-command "urxvtd -q -o -f"))

;; Poweroff
(defcommand poweroff () ()
            (run-shell-command "~/.stumpwm.d/modules/util/stumpish/stumpish quit && systemctl poweroff"))

;; Reboot
(defcommand reboot () ()
            (run-shell-command "~/.stumpwm.d/modules/util/stumpish/stumpish quit && systemctl reboot"))

;; Suspend
(defcommand suspend () ()
            (run-shell-command "systemctl suspend"))

;;; Settings

;; Keybindings
(load-rc "keybindings")

;; Shell
(setf *shell-program* (getenv "SHELL"))

;; Welcome message
(setf *time-format-string-default* "%A %F %R")
(setf *startup-message*
      (format nil "Welcome ~a, ~a"
              (getenv "USER") (time-format *time-format-string-default*)))

;;; Start Swank server

;; Require swank server
(require 'swank) ;; Requires swank, slime, and quicklisp-slime-helper for emacs.

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
(unless *initialized* (swank-server))

;;; Finish initializing

;; Programs to start immediately
(unless *initialized*
  (run-shell-command "urxvtd -q -o -f"))

;; Tell StumpWM that we've booted up once
(setf *initialized* t)
