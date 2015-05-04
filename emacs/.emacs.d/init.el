;;;; $HOME/.emacs.d/init.el
;;;; By Michael Richer
;;;; Since May 5th, 2014

;;;; Initialize stuff

;;; Load path
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; Determine operating system
(defun linuxp () "True if running GNU/Linux."
  (if (or (eq system-type 'gnu/linux) (eq system-type 'linux)) t nil))
(defun windowsp () "True if running Windows."
  (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin)) t nil))

;;; Packages
(require 'package-populate)
(setq package-required-list
      '(color-theme expand-region multiple-cursors slime))

;;; StumpWM swank sever
(require 'stumpwm-mode)
(require 'stumpwm-utils)
(setq stumpwm-shell-program "~/.stumpwm.d/modules/util/stumpish/stumpish")

;;;; Set up Emacs

;;; Appearance

;; GUI settings
(blink-cursor-mode -1)
(show-paren-mode 1)
(setq echo-keystrokes 0.1
      font-lock-maximum-decoration t
      global-font-lock-mode t
      show-paren-delay 0)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Get the font we like
(defun system-font ()
  "Returns the font to use for Emacs frames; nil means only the
default font is available."
  (cond
   ((and (linuxp) (member "terminus" (font-family-list)))
    "Terminus-12")
   ((and (windowsp) (member "Consolas" (font-family-list)))
    "Consolas-12")))

;; Appearance for each frame
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global nil)
(defun frame-setup-appearance (&optional frame)
  "Set the theme for the newly-created frame."
  (when frame (select-frame frame))
  (if (window-system frame)
      (color-theme-dark-laptop) ;; If GUI
    (color-theme-dark-laptop))
  (set-frame-font (system-font))) ;; If terminal

;; Appearance set when creating new frame
(add-hook 'after-make-frame-functions 'frame-setup-appearance)
(frame-setup-appearance)

;;; Apropos
(setq apropos-do-all t)

;;; Backups and autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir))
      auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-list-file-prefix autosave-dir
      tramp-auto-save-directory autosave-dir
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 5)

;;; Buffers and files
(require 'uniquify)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(setq uniquify-buffer-name-style 'forward
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      load-prefer-newer t
      recentf-max-saved-items 50
      recentf-max-menu-items recentf-max-saved-items
      recentf-save-file "~/.emacs.d/recentf-file")
(recentf-mode 1)

;;; Calendar and date stuff
(require 'calendar)
(require 'insert-date)
(calendar-set-date-style 'iso)

;;; Case sensitivity
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      pcomplete-ignore-case t
      eshell-cmpl-ignore-case t)

;;; Commands/features/functions
(setq disabled-command-hook nil)
(require 'expand-region)
(require 'multiple-cursors)
(require 'unfill)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Games
(autoload 'typing-of-emacs "typing-of-emacs.el"
  "The Typing of Emacs, a game." t)

;;; Keybindings and mouse bindings
(require 'keybindings)
(setq mouse-yank-at-point t)

;;; Mode line
(setq display-time-24hr-format t
      show-help-function nil)
(display-time-mode)
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(tooltip-mode -1)

;;; Saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace-file")

;;; SLIME

;; Quicklisp path
(defvar quicklisp-path
  (expand-file-name "~/.quicklisp/")
  "The path to my quicklisp installation.")

;; Quicklisp SLIME helper
(defvar quicklisp-slime-helper
  (concat quicklisp-path "slime-helper.el")
  "The location of the quicklisp-slime-helper elisp file.")

;; Initialize SLIME and helpers
(require 'slime)
(when (file-exists-p quicklisp-slime-helper)
  (load quicklisp-slime-helper))
(setq inferior-lisp-program "/usr/bin/sbcl")

;;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

;;; Tabs
(setq-default indent-tabs-mode nil)

;;; Unicode
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Username
(setq user-full-name "Michael Richer"
      user-mail-address "msricher1993@gmail.com")

;;; X windows options
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;;;; Misc functions

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun goto-line-with-feedback ()
  "Go to line interactively, while showing the user the line numbers."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun stumpwm-connect ()
  "Connect to StumpWM's swank server at 127.0.0.1:4005."
  (interactive)
  (stumpwm-mode 1)
  (slime-connect "127.0.0.1" 4005))

(defun stumpwm-disconnect ()
  "Close the current connection."
  (interactive)
  (stumpwm-mode -1)
  (slime-disconnect))

(defun sudo-edit (&optional arg)
  "Edit a file as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file
        (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file
      (concat "/sudo:root@localhost:" buffer-file-name))))

(defun toggle-window-split ()
  "Toggle a two-window-split layout between horizontal and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
