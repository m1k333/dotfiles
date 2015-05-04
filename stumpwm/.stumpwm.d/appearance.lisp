;;; -*-lisp-*-

;; Package
(in-package :stumpwm)

;; Appearance settings
(setf *maxsize-border-width    1
      *normal-border-width*    1
      *transient-border-width* 1
      *window-border-style*    :thin)

;; Mode-line
(setf *time-modeline-string*    "%A %F %R"
      *screen-mode-line-format* "[^B%n^b] %W ^B||^b [%B] ^B||^b %d")
