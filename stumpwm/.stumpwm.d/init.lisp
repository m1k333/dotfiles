;;; -*-lisp-*-

;;; Initialize

;; Package
(in-package :stumpwm)

;;; Settings

;; Appearance
(setf *maxsize-border-width    1
      *normal-border-width*    1
      *transient-border-width* 1
      *window-border-style*    :tight)

;; Shell
(setf *shell-program* (getenv "SHELL"))

;; Welcome message
(setf *startup-message* (run-shell-command "echo Welcome $USER, $(date +%c)" t))

;;;; Functions

;;; ACPI info
(defcommand battery () ()
            (echo-string (current-screen) (run-shell-command "acpi -V" t)))

;;; Firefox run or raise
(defcommand firefox () ()
            "Run Firefox, or if it is already open, raise its window."
            (run-or-raise "firefox" '(:class "Firefox")))

;;; Interactive `colon' for interactive commands; the first arg is an optional
;;; initial argments
(defcommand colon1 (&optional (initial "")) (:rest)
            (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
              (when cmd
                (eval-command cmd t))))

;;;; Keybindings

;;; Keybindings/mouse setings
(set-prefix-key (kbd "s-x"))
(setf *mouse-focus-policy* :sloppy)

;;; Top map
(setf *top-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *escape-key* '*root-map*)
        (define-key m (kbd "s-g") "abort")
        (define-key m (kbd "s-!") "exec")
        (define-key m (kbd "s-;") "colon")
        (define-key m (kbd "s-:") "eval")
        (define-key m (kbd "s-b") "banish")
        (define-key m (kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")
        (define-key m (kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
        (define-key m (kbd "XF86AudioMute")        "exec amixer set Master toggle")
        (define-key m (kbd "Print")                "exec screenshot")
        m))

;;; Root map
(setf *root-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *escape-fake-key* "send-escape")
        (define-key m (kbd "RET") "exec urxvtc")
        (define-key m (kbd "e")   "emacs")
        (define-key m (kbd "a")   "time")
        (define-key m (kbd "b")   "battery")
        (define-key m (kbd "C-g") "abort")
        (define-key m (kbd ";")   "colon")
        (define-key m (kbd ":")   "eval")
        (define-key m (kbd "h")   '*help-map*)
        (define-key m (kbd "v")   "version")
        (define-key m (kbd "m")   "lastmsg")
        (define-key m (kbd "C-m") "lastmsg")
        (define-key m (kbd "G")   "vgroups")
        (define-key m (kbd "g")   '*groups-map*)
        (define-key m (kbd "x")   '*exchange-window-map*)
        (define-key m (kbd "F1")  "gselect 1")
        (define-key m (kbd "F2")  "gselect 2")
        (define-key m (kbd "F3")  "gselect 3")
        (define-key m (kbd "F4")  "gselect 4")
        (define-key m (kbd "F5")  "gselect 5")
        (define-key m (kbd "F6")  "gselect 6")
        (define-key m (kbd "F7")  "gselect 7")
        (define-key m (kbd "F8")  "gselect 8")
        (define-key m (kbd "F9")  "gselect 9")
        (define-key m (kbd "F10") "gselect 10")
        m))

;;; Group top map
(setf *group-top-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *escape-key* '*group-root-map*)
        m))

;;; Group root map
(setf *group-root-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "C-u") "next-urgent")
        (define-key m (kbd "w")   "windows")
        (define-key m (kbd "C-w") "windows")
        (define-key m (kbd "k")   "delete")
        (define-key m (kbd "C-k") "delete")
        (define-key m (kbd "K")   "kill")
        (define-key m (kbd "'")   "select")
        (define-key m (kbd "\"")  "windowlist")
        (define-key m (kbd "0")   "select-window-by-number 0")
        (define-key m (kbd "1")   "select-window-by-number 1")
        (define-key m (kbd "2")   "select-window-by-number 2")
        (define-key m (kbd "3")   "select-window-by-number 3")
        (define-key m (kbd "4")   "select-window-by-number 4")
        (define-key m (kbd "5")   "select-window-by-number 5")
        (define-key m (kbd "6")   "select-window-by-number 6")
        (define-key m (kbd "7")   "select-window-by-number 7")
        (define-key m (kbd "8")   "select-window-by-number 8")
        (define-key m (kbd "9")   "select-window-by-number 9")
        (define-key m (kbd "C-N") "number")
        (define-key m (kbd "#")   "mark")
        (define-key m (kbd "F11") "fullscreen")
        (define-key m (kbd "A")   "title")
        (define-key m (kbd "i")   "info")
        m))

;;; Tile group top map
(setf *tile-group-top-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *escape-key* '*tile-group-root-map*)
        m))

;;; Tile group root map
(setf *tile-group-root-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "n")       "pull-hidden-next")
        (define-key m (kbd "C-n")     "pull-hidden-next")
        (define-key m (kbd "M-n")     "next")
        (define-key m (kbd "C-M-n")   "next-in-frame")
        (define-key m (kbd "SPC")     "pull-hidden-next")
        (define-key m (kbd "C-SPC")   "pull-hidden-next")
        (define-key m (kbd "p")       "pull-hidden-previous")
        (define-key m (kbd "C-p")     "pull-hidden-previous")
        (define-key m (kbd "M-p")     "prev")
        (define-key m (kbd "C-M-p")   "prev-in-frame")
        (define-key m (kbd "W")       "place-existing-windows")
        (define-key m *escape-key*     "pull-hidden-other")
        (define-key m (kbd "M-t")     "other-in-frame")
        (define-key m (kbd "C-0")     "pull 0")
        (define-key m (kbd "C-1")     "pull 1")
        (define-key m (kbd "C-2")     "pull 2")
        (define-key m (kbd "C-3")     "pull 3")
        (define-key m (kbd "C-4")     "pull 4")
        (define-key m (kbd "C-5")     "pull 5")
        (define-key m (kbd "C-6")     "pull 6")
        (define-key m (kbd "C-7")     "pull 7")
        (define-key m (kbd "C-8")     "pull 8")
        (define-key m (kbd "C-9")     "pull 9")
        (define-key m (kbd "R")       "remove")
        (define-key m (kbd "s")       "vsplit")
        (define-key m (kbd "S")       "hsplit")
        (define-key m (kbd "r")       "iresize")
        (define-key m (kbd "o")       "fnext")
        (define-key m (kbd "TAB")     "fnext")
        (define-key m (kbd "M-TAB")   "fother")
        (define-key m (kbd "f")       "fselect")
        (define-key m (kbd "F")       "curframe")
        (define-key m (kbd "-")       "fclear")
        (define-key m (kbd "Q")       "only")
        (define-key m (kbd "Up")      "move-focus up")
        (define-key m (kbd "Down")    "move-focus down")
        (define-key m (kbd "Left")    "move-focus left")
        (define-key m (kbd "Right")   "move-focus right")
        (define-key m (kbd "M-Up")    "move-window up")
        (define-key m (kbd "M-Down")  "move-window down")
        (define-key m (kbd "M-Left")  "move-window left")
        (define-key m (kbd "M-Right") "move-window right")
        (define-key m (kbd "+")       "balance-frames")
        (define-key m (kbd "l")       "redisplay")
        (define-key m (kbd "C-l")     "redisplay")
        m))

;;; Groups map
(setf *groups-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "g")     "groups")
        (define-key m (kbd "c")     "gnew")
        (define-key m (kbd "n")     "gnext")
        (define-key m (kbd "C-n")   "gnext")
        (define-key m (kbd "SPC")   "gnext")
        (define-key m (kbd "C-SPC") "gnext")
        (define-key m (kbd "N")     "gnext-with-window")
        (define-key m (kbd "p")     "gprev")
        (define-key m (kbd "C-p")   "gprev")
        (define-key m (kbd "P")     "gprev-with-window")
        (define-key m (kbd "o")     "gother")
        (define-key m (kbd "'")     "gselect")
        (define-key m (kbd "\"")    "grouplist")
        (define-key m (kbd "m")     "gmove")
        (define-key m (kbd "M")     "gmove-marked")
        (define-key m (kbd "k")     "gkill")
        (define-key m (kbd "A")     "grename")
        (define-key m (kbd "r")     "grename")
        (define-key m (kbd "1")     "gselect 1")
        (define-key m (kbd "2")     "gselect 2")
        (define-key m (kbd "3")     "gselect 3")
        (define-key m (kbd "4")     "gselect 4")
        (define-key m (kbd "5")     "gselect 5")
        (define-key m (kbd "6")     "gselect 6")
        (define-key m (kbd "7")     "gselect 7")
        (define-key m (kbd "8")     "gselect 8")
        (define-key m (kbd "9")     "gselect 9")
        (define-key m (kbd "0")     "gselect 10")
        m))

;;; Exchange window map
(setf *exchange-window-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "Up")    "exchange-direction up"   )
        (define-key m (kbd "Down")  "exchange-direction down" )
        (define-key m (kbd "Left")  "exchange-direction left" )
        (define-key m (kbd "Right") "exchange-direction right")
        (define-key m (kbd "p")     "exchange-direction up"   )
        (define-key m (kbd "n")     "exchange-direction down" )
        (define-key m (kbd "b")     "exchange-direction left" )
        (define-key m (kbd "f")     "exchange-direction right")
        (define-key m (kbd "k")     "exchange-direction up"   )
        (define-key m (kbd "j")     "exchange-direction down" )
        (define-key m (kbd "l")     "exchange-direction left" )
        (define-key m (kbd "h")     "exchange-direction right")
        m))

;;; Help map
(setf *help-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "v") "describe-variable")
        (define-key m (kbd "f") "describe-function")
        (define-key m (kbd "k") "describe-key")
        (define-key m (kbd "c") "describe-command")
        (define-key m (kbd "w") "where-is")
        m))

;;;; Run external stuff

;;; Finish initializing; everything below this is only run ONCE
(unless (boundp '*initialized*)

  ;;; Background processes
  (run-shell-command "feh --no-fehbg --bg-center ~/.stumpwm.d/wallpaper*")
  (run-shell-command "urxvtd -q -o -f")

  ;;; Programs to start immediately
  ;;(run-shell-command "urxvtc")
  
  ;;; Tell LISP that we've booted up once
  (defvar *initialized* nil "Set to t if StumpWM has been started once."))
