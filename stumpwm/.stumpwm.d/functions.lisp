;;; -*-lisp-*-

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
            (run-shell-command "stumpish quit && systemctl poweroff"))

;; Reboot
(defcommand reboot () ()
            (run-shell-command "stumpish quit && systemctl reboot"))

;; Suspend
(defcommand suspend () ()
            (run-shell-command "stumpish quit && systemctl suspend"))
