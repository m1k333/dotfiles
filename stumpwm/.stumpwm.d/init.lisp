;;; -*-lisp-*-

;;; 2015-05-07
;;; ~/.stumpwm.d/init.lisp
;;; I wrote this instead of doing work.

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
(load-module "surfraw")

;; Get $SHELL
(setf *shell-program* (getenv "SHELL"))

;;; Appearance

;; Window borders
(setf *maxsize-border-width*   1
      *normal-border-width*    1
      *transient-border-width* 1
      *window-border-style*    :thin)

;; Root window
(unless *initialized*
  (run-shell-command "feh --no-fehbg --bg-tile ~/.stumpwm.d/wallpaper.jpg")
  (run-shell-command "xsetroot -cursor_name left_ptr"))

;;; Commands and functions

;; cat (UNIX-style function)
(defun cat (&rest strings)
  "Concatenates strings, like the Unix command `cat'."
  (apply 'concatenate 'string strings))

;; Interactive `colon' command; the args are optional initial contents
(defcommand colon1 (&optional (initial "")) (:rest)
  "Interactive `colon' command with optional initial input-line contents."
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd (eval-command cmd t))))

;; ACPI info
(defcommand battery () ()
  "Show the ACPI power status."
  (echo-string (current-screen) (run-shell-command "acpi -V" t)))

;; ALSA sound info
(defcommand audio () ()
  "Show the ALSA audio status."
  (echo-string (current-screen) (run-shell-command "amixer get Master" t)))
(defcommand volup () ()
  "Raise the ALSA volume."
  (run-shell-command "amixer set Master 5%+"))
(defcommand voldown () ()
  "Lower the ALSA volume."
  (run-shell-command "amixer set Master 5%-"))
(defcommand voltoggle () ()
  "Toggle ALSA audio output on and off."
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
  (let ((psgrep (run-shell-command "ps aux | grep urxvtd | grep -v grep" t)))
    (if (> (length psgrep) 0)
        (echo-string (current-screen) "Urxvtd is already running.")
        (progn
          (echo-string (current-screen) "Starting urxvt daemon.")
          (run-shell-command "urxvtd -q -o -f")))))

;; Poweroff, reboot and suspend
(defcommand poweroff () ()
  "Power off the machine."
  (run-shell-command "~/.stumpwm.d/modules/util/stumpish/stumpish quit && systemctl poweroff"))
(defcommand reboot () ()
  "Reboot the machine."
  (run-shell-command "~/.stumpwm.d/modules/util/stumpish/stumpish quit && systemctl reboot"))
(defcommand suspend () ()
  "Suspend the machine."
  (run-shell-command "systemctl suspend"))

;;; Keybindings

;; Keyboard/mouse setings
(set-prefix-key (kbd "s-x"))
(defvar *abort-key* (kbd "s-g"))
(setf *mouse-focus-policy* :click)

;; Top map
(setf *top-map*
      (let ((m (make-sparse-keymap)))

        ;; Access keyboard maps
        (define-key m *escape-key* '*root-map*)
        (define-key m (kbd "s-e")  '*exchange-window-map*)
        (define-key m (kbd "s-G")  '*groups-map*)
        (define-key m (kbd "s-h")  '*help-map*)
        (define-key m (kbd "s-l")  '*launcher-map*)

        ;; StumpWM interaction
        (define-key m *abort-key* "abort")
        (define-key m (kbd "s-;") "colon")
        (define-key m (kbd "s-b") "banish")
        (define-key m (kbd "s-r") "run-shell-command")
        (define-key m (kbd "s-:") "eval")

        ;; Group commands
        (define-key m (kbd "s-1") "gselect 1")
        (define-key m (kbd "s-2") "gselect 2")
        (define-key m (kbd "s-3") "gselect 3")
        (define-key m (kbd "s-4") "gselect 4")
        (define-key m (kbd "s-5") "gselect 5")
        (define-key m (kbd "s-6") "gselect 6")
        (define-key m (kbd "s-7") "gselect 7")
        (define-key m (kbd "s-8") "gselect 8")
        (define-key m (kbd "s-9") "gselect 9")
        (define-key m (kbd "s-0") "gselect 10")

        ;; Function keys
        (define-key m (kbd "s-RET")                "exec urxvtc")
        (define-key m (kbd "XF86AudioRaiseVolume") "volup")
        (define-key m (kbd "XF86AudioLowerVolume") "voldown")
        (define-key m (kbd "XF86AudioMute")        "voltoggle")
        (define-key m (kbd "Print")                "screenshot")

        ;; Return the keymap
        m))

;; Root map
(setf *root-map*
      (let ((m (make-sparse-keymap)))

        ;; StumpWM interaction
        (define-key m *escape-fake-key* "send-escape")
        (define-key m *abort-key*       "abort")

        ;; Popup information
        (define-key m (kbd "a") "audio")
        (define-key m (kbd "b") "battery")
        (define-key m (kbd "g") "groups")
        (define-key m (kbd "G") "vgroups")
        (define-key m (kbd "m") "lastmsg")
        (define-key m (kbd "t") "time")
        (define-key m (kbd "v") "version")

        ;; Return keymap
        m))

;; Group top map
(setf *group-top-map*
      (let ((m (make-sparse-keymap)))

        ;; Access keyboard maps
        (define-key m *escape-key* '*group-root-map*)

        ;; Window commands
        (define-key m (kbd "s-u")   "next-urgent")
        (define-key m (kbd "s-w")   "windowlist")
        (define-key m (kbd "s-W")   "select")
        (define-key m (kbd "s-F11") "fullscreen")

        ;; Return keymap
        m))

;; Group root map
(setf *group-root-map*
      (let ((m (make-sparse-keymap)))

        ;; Popup information
        (define-key m (kbd "i") "info")

        ;; Window commands
        (define-key m (kbd "w")     "windows")
        (define-key m (kbd "C-w")   "windows")
        (define-key m (kbd "k")     "delete")
        (define-key m (kbd "C-k")   "kill")
        (define-key m (kbd "T")     "title")
        (define-key m (kbd "n")     "number")
        (define-key m (kbd "#")     "mark")
        (define-key m (kbd "C-SPC") "mark")
        (define-key m (kbd "C-0")   "select-window-by-number 0")
        (define-key m (kbd "C-1")   "select-window-by-number 1")
        (define-key m (kbd "C-2")   "select-window-by-number 2")
        (define-key m (kbd "C-3")   "select-window-by-number 3")
        (define-key m (kbd "C-4")   "select-window-by-number 4")
        (define-key m (kbd "C-5")   "select-window-by-number 5")
        (define-key m (kbd "C-6")   "select-window-by-number 6")
        (define-key m (kbd "C-7")   "select-window-by-number 7")
        (define-key m (kbd "C-8")   "select-window-by-number 8")
        (define-key m (kbd "C-9")   "select-window-by-number 9")

        ;; Return keymap
        m))

;; Tile group top map
(setf *tile-group-top-map*
      (let ((m (make-sparse-keymap)))

        ;; Access keyboard maps
        (define-key m *escape-key* '*tile-group-root-map*)

        ;; Window commands
        (define-key m (kbd "s-n")       "pull-hidden-next")
        (define-key m (kbd "s-N")       "next")
        (define-key m (kbd "s-C-n")     "next-in-frame")
        (define-key m (kbd "s-p")       "pull-hidden-previous")
        (define-key m (kbd "s-P")       "prev")
        (define-key m (kbd "s-SPC")     "pull-hidden-other")
        (define-key m (kbd "s-C-SPC")   "other-in-frame")
        (define-key m (kbd "s-Up")      "move-focus up")
        (define-key m (kbd "s-Down")    "move-focus down")
        (define-key m (kbd "s-Left")    "move-focus left")
        (define-key m (kbd "s-Right")   "move-focus right")
        (define-key m (kbd "s-C-Up")    "move-window up")
        (define-key m (kbd "s-C-Down")  "move-window down")
        (define-key m (kbd "s-C-Left")  "move-window left")
        (define-key m (kbd "s-C-Right") "move-window right")
        (define-key m (kbd "s-TAB")     "fnext")

        ;; Return keymap
        m))

;; Tile group root map
(setf *tile-group-root-map*
      (let ((m (make-sparse-keymap)))

        ;; Splitting frame commands
        (define-key m (kbd "1") "only")
        (define-key m (kbd "2") "vsplit")
        (define-key m (kbd "3") "hsplit")
        (define-key m (kbd "0") "remove")
        (define-key m (kbd "r") "iresize")

        ;; Window commands
        (define-key m (kbd "o")     "fother")
        (define-key m (kbd "TAB")   "fnext")
        (define-key m (kbd "f")     "fselect")
        (define-key m (kbd "F")     "curframe")
        (define-key m (kbd "-")     "fclear")
        (define-key m (kbd "+")     "balance-frames")
        (define-key m (kbd "l")     "redisplay")
        (define-key m (kbd "C-l")   "redisplay")

        ;; Return keymap
        m))

;; Launcher map
(defvar *launcher-map* nil)
(setf *launcher-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *abort-key* "abort")
        (define-key m (kbd "c")   "exec urxvtc")
        (define-key m (kbd "RET") "exec urxvtc")
        (define-key m (kbd "e")   "emacsclient")
        (define-key m (kbd "f")   "firefox")
        (define-key m (kbd "t")   "thunderbird")
        m))

;;; Add the abort key to other maps
(define-key *exchange-window-map* *abort-key*  "abort")
(define-key *groups-map*          *abort-key*  "abort")
(define-key *help-map*            *abort-key*  "abort")
(define-key *menu-map*            *abort-key*  "abort")
(define-key *resize-map*          *abort-key*  "abort")

;;; Start Swank server

;; Requires swank, slime, and quicklisp-slime-helper for emacs.
(require 'swank)

;; Is swank running?
(defvar *swank-running* nil "True if swank is running in StumpWM.")

;; Toggle swank on or off
(defcommand swank-server () ()
  "Toggle the swank server on or off."
  (if *swank-running*
      (progn
        (echo-string (current-screen) "Stopping swank.")
        (swank:stop-server 4005)
        (setf *swank-running* nil))
      (progn
        (echo-string (current-screen) "Starting swank.")
        (swank:create-server :port 4005 :dont-close t)
        (setf *swank-running* t))))

;; Start swank
(unless *initialized* (swank-server))

;;; Finish initializing

;; Make some groups
(grename "1")
(loop for x from 2 to 10
   do (gnewbg (write-to-string x)))

;; Welcome message
(setf *time-format-string-default* "%A %F %R")
(setf *startup-message*
      (format nil "Welcome to ~a, ~a.  [~a]"
              (machine-instance)
              (getenv "USER")
              (time-format *time-format-string-default*)))

;; Programs to start immediately
(unless *initialized* (urxvtd))

;; Tell StumpWM that we've booted up once
(setf *initialized* t)
