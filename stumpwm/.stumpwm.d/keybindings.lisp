;;;; -*-lisp-*-

;;;; Package
(in-package :stumpwm)

;;;; Keybindings

;;; Keyboard/mouse setings
(set-prefix-key (kbd "s-x"))
(defvar *abort-key* (kbd "s-g"))
(setf *mouse-focus-policy* :sloppy)

;;; My own version of existing keymaps

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
        (define-key m (kbd "s-m") "mode-line")
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

        ;; Window commands
        (define-key m (kbd "s-n")       "pull-hidden-next")
        (define-key m (kbd "s-N")       "next")
        (define-key m (kbd "s-C-n")     "next-in-frame")
        (define-key m (kbd "s-p")       "pull-hidden-previous")
        (define-key m (kbd "s-P")       "prev")
        (define-key m (kbd "s-SPC")     "pull-hidden-other")
        (define-key m (kbd "s-C-SPC")   "other-in-frame")
        (define-key m (kbd "s-u")       "next-urgent")
        (define-key m (kbd "s-w")       "windowlist")
        (define-key m (kbd "s-W")       "select")
        (define-key m (kbd "s-Up")      "move-focus up")
        (define-key m (kbd "s-Down")    "move-focus down")
        (define-key m (kbd "s-Left")    "move-focus left")
        (define-key m (kbd "s-Right")   "move-focus right")
        (define-key m (kbd "s-C-Up")    "move-window up")
        (define-key m (kbd "s-C-Down")  "move-window down")
        (define-key m (kbd "s-C-Left")  "move-window left")
        (define-key m (kbd "s-C-Right") "move-window right")
        (define-key m (kbd "s-TAB")     "fnext")

        ;; Function keys
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
        (define-key m (kbd "b") "battery")
        (define-key m (kbd "g") "groups")
        (define-key m (kbd "G") "vgroups")
        (define-key m (kbd "i") "info")
        (define-key m (kbd "m") "lastmsg")
        (define-key m (kbd "t") "time")
        (define-key m (kbd "v") "version")

        ;; Splitting frame commands
        (define-key m (kbd "1") "only")
        (define-key m (kbd "2") "vsplit")
        (define-key m (kbd "3") "hsplit")
        (define-key m (kbd "0") "remove")
        (define-key m (kbd "r") "iresize")

        ;; Window commands
        (define-key m (kbd "w")     "windows")
        (define-key m (kbd "C-w")   "windows")
        (define-key m (kbd "k")     "delete")
        (define-key m (kbd "C-k")   "kill")
        (define-key m (kbd "o")     "fother")
        (define-key m (kbd "TAB")   "fnext")
        (define-key m (kbd "f")     "fselect")
        (define-key m (kbd "F")     "curframe")
        (define-key m (kbd "-")     "fclear")
        (define-key m (kbd "+")     "balance-frames")
        (define-key m (kbd "l")     "redisplay")
        (define-key m (kbd "C-l")   "redisplay")
        (define-key m (kbd "T")     "title")
        (define-key m (kbd "n")     "number")
        (define-key m (kbd "#")     "mark")
        (define-key m (kbd "C-SPC") "mark")
        (define-key m (kbd "F11")   "fullscreen")
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

;; Exchange window map
(setf *exchange-window-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *abort-key*   "abort")
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

;; Groups map
(setf *groups-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *abort-key*   "abort")
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

;; Help map
(setf *help-map*
      (let ((m (make-sparse-keymap)))
        (define-key m *abort-key* "abort")
        (define-key m (kbd "v")   "describe-variable")
        (define-key m (kbd "f")   "describe-function")
        (define-key m (kbd "k")   "describe-key")
        (define-key m (kbd "c")   "describe-command")
        (define-key m (kbd "w")   "where-is")
        m))

;;; Get rid of the default maps I'm not using
(defvar *superfluous-keymaps* '(*group-top-map*
                                *group-root-map*
                                *tile-group-top-map*
                                *tile-group-root-map*))
(dolist (map *superfluous-keymaps*)
  (setf map
        (let ((m (make-sparse-keymap)))
          m)))

