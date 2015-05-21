;;;; $HOME/.emacs.d/init.el
;;;; By Michael Richer
;;;; Since May 5th, 2014

;;;; Initialize stuff

;;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Determine operating system
(defun linuxp () "True if running GNU/Linux."
       (if (or (eq system-type 'gnu/linux) (eq system-type 'linux)) t nil))
(defun windowsp () "True if running Windows."
       (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin)) t nil))

;;; Common lisp functionality
(require 'cl)

;;; Packages

;; Add the package repositories and intialize
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Define `package-required-list'
(defvar package-required-list
  '(color-theme-solarized slime stumpwm-mode)
  "A list of packages that should be installed for this Emacs
configuration.  If any are not installed, they should be able
to be installed by running `package-populate'.")

;; Test for missing packages
(defun package-missing ()
  "Test for missing packages from the list of desired packages in
the `package-required-list' variable."
  (catch 'done
    (unless package-required-list (throw 'done nil))
    (dolist (pkg package-required-list)
      (unless (package-installed-p pkg)
        (throw 'done t)))))

;; Populate the system with the desired packages
(defun package-populate ()
  "If packages from `package-required-list' are missing, install them."
  (interactive)
  (when (package-missing)
    (package-refresh-contents)
    (dolist (pkg package-required-list)
      (unless (package-installed-p pkg)
        (package-install pkg)))
    (package-initialize)))

;; Do the package installs if required
(package-populate)

;;; StumpWM swank sever

;; Minor mode for sending StumpWM commands
(require 'stumpwm-mode)
(setq stumpwm-shell-program "~/.stumpwm.d/modules/util/stumpish/stumpish")

;; Connect to StumpWM's swank server
(defun stumpwm-connect ()
  "Connect to StumpWM's swank server at 127.0.0.1:4005."
  (interactive)
  (stumpwm-mode 1)
  (slime-connect "127.0.0.1" 4005))

;; Disconnect from StumpWM's swank server
(defun stumpwm-disconnect ()
  "Close the current connection."
  (interactive)
  (stumpwm-mode -1)
  (slime-disconnect))

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
(when (window-system)
  (require 'color-theme)
  (setq color-theme-is-global t)
  (color-theme-initialize)
  (color-theme-solarized))

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

;; Various useful things
(require 'uniquify)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(setq uniquify-buffer-name-style 'forward
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      load-prefer-newer t)

;; Recentf mode
(setq recentf-max-saved-items 50
      recentf-max-menu-items recentf-max-saved-items
      recentf-save-file "~/.emacs.d/recentf-file")
(recentf-mode 1)

;;; Delete current buffer file
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

;;; Rename current buffer file
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

;;; Calendar and date stuff

;; Set up calendar
(require 'calendar)
(calendar-set-date-style 'iso)

;; Date function; returns the date in specified format
(defvar time-string-standard "%F %T" "The preferred stfrtime format.")
(defun date (&optional format)
  "Return the date in specified stfrtime format."
  (if format
    (progn
      (if (stringp format)
          (if (= 0 (length format))
              (format-time-string time-string-standard)
              (format-time-string format))
          (error "Argument passed must be a string")))
    (format-time-string time-string-standard)))

;; Insert the date interactively
(defun insert-date (&optional format)
  (interactive "MStfrtime format [RET for default]: ")
  (insert (date format)))

;; Insert the date quickly
(defun insert-standard-date ()
  (interactive)
  (insert (date)))

;;; Case sensitivity
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      pcomplete-ignore-case t
      eshell-cmpl-ignore-case t)

;;; Commands/features/functions
(setq disabled-command-hook nil)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Fill

;; Default fill column
(setq-default fill-column 80)

;; Remove hard wrapping/filling for paragraph
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

;; Remove hard wrapping/filling for region
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;; LISP interaction stuff

;; Eval and replace last sexp function
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; Mode line
(setq display-time-24hr-format t
      show-help-function nil)
(column-number-mode)
(display-battery-mode)
(display-time-mode)
(line-number-mode)
(size-indication-mode)
(tooltip-mode -1)

;;; Saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace-file")

;;; SLIME

;; Quicklisp path
(defvar quicklisp-path
  (expand-file-name "~/quicklisp/")
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

;;; TRAMP

;;; Edit a file as root using sudo (from `what the .emacs.d?!')
(defun sudo-edit (&optional arg)
  "Edit a file as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file
        (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file
      (concat "/sudo:root@localhost:" buffer-file-name))))

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

;;; Window/frame management

;;; Toggle a 2-window split between horizontal and vertical split
(defun toggle-window-split ()
  "Toggle a two-window-split layout between horizontal and
vertical split."
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

;;; X windows options
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;;;; Keybindings and mouse bindings

;;; Keyboard
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "C-x C-h") 'mark-whole-buffer)
(global-set-key (kbd "C-(") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-)") 'kmacro-end-or-call-macro)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-RET") 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;; Mouse
(setq mouse-yank-at-point t)

;;;; EOF
