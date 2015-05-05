;;;; -*-lisp-*-

;;; Package
(in-package :stumpwm)

;;; Functions and commands

;; ACPI info
(defcommand battery () ()
  (echo-string (current-screen) (run-shell-command "acpi -V" t)))

;; ALSA sound I/O info
(defcommand audio () ()
  (echo-string (current-screen) (run-shell-command "amixer get Master" t)))

;; cat (UNIX-style function)
(defun cat (&rest strings)
  "Concatenates strings, like the Unix command `cat'."
  (apply 'concatenate 'string strings))

;; Emacsclient run or raise
(defcommand emacsclient () ()
            "Run emacsclient, or if it is already open, raise its window."
            (run-or-raise "emc -g" '(:class "Emacs")))

;; Firefox run or raise
(defcommand firefox () ()
            "Run Firefox, or if it is already open, raise its window."
            (run-or-raise "firefox" '(:class "Firefox")))

;; Interactive `colon' command; the args are optional initial contents
(defcommand colon1 (&optional (initial "")) (:rest)
            (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
              (when cmd
                (eval-command cmd t))))

;; Poweroff
(defcommand poweroff () ()
            (run-shell-command "~/.stumpwm.d/modules/util/stumpish/stumpish quit && systemctl poweroff"))

;; Reboot
(defcommand reboot () ()
            (run-shell-command "~/.stumpwm.d/modules/util/stumpish/stumpish quit && systemctl reboot"))

;; Suspend
(defcommand suspend () ()
            (run-shell-command "systemctl suspend"))
