;;;; $HOME/.emacs.d/init.el
;;;; By Michael Richer
;;;; Since May 5th, 2014

;;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add the package repositories and intialize
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Load use-package, as well as diminish and bind-key (used by use-package)
(require 'use-package)
(require 'bind-key)
(use-package diminish :ensure t)

;;; Facilitate easy byte-compilation of the users' init file
(defun byte-compile-init () "Byte-compile Emacs' init file."
       (interactive) (byte-compile-file user-init-file))

;;; Key maps to be populated throughout init file
(define-prefix-command 'ctl-x-m-map)

;;;; Appearance settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GUI settings
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(show-paren-mode 1)
(blink-cursor-mode -1)
(setq show-paren-delay 0
      echo-keystrokes 0.1
      global-font-lock-mode t
      font-lock-maximum-decoration t)
(when window-system
  (add-hook 'after-init-hook
            '(lambda () (set-frame-size (selected-frame) 82 26))))

;;; Theme
(use-package solarized-theme
  :ensure t
  :if window-system
  :config
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

;;; Mode line
(tooltip-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(setq show-help-function nil)
(setq-default mode-line-format
              '(" "
                (:eval (substring-no-properties evil-mode-line-tag 0 nil))
                "  (%b)  "
                (:eval (if vc-mode
                           (concat "("
                                   (substring-no-properties vc-mode 1 nil)
                                   ")  ")))
                "(%z%*%@)  ("
                (:eval (system-name))
                ")  (%p of %I)  (%l,%c)  "
                "%[(" mode-name mode-line-process minor-mode-alist "%n)%]  "))
(defvar evil-mode-line-tag (concat "(v" emacs-version ")"))

;;;; Emacs settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Apropos
(setq apropos-do-all t)

;;; Anzu
(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode 1))

;;; Auto-complete
(use-package auto-complete
  :ensure t
  :defer t
  :diminish auto-complete-mode
  :bind ("C-<tab>" . auto-complete)
  :init (use-package fuzzy :ensure t)
  :config
  (ac-config-default)
  (setq ac-auto-start nil
        ac-comphist-file (expand-file-name "ac-comp-history-file"
                                           user-emacs-directory))
  (use-package jedi
    ;; Python auto-complete via Jedi python backend.  M-x
    ;; jedi:install-server is required on first run.
    ;; jed:install-server is dependent on the virtualenv
    ;; python library, which is available as
    ;; python-virtualenv in the Arch repos.
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))


;;; Backups and autosave
(defvar backup-dir (expand-file-name "backup" user-emacs-directory))
(defvar autosave-dir (expand-file-name "autosave" user-emacs-directory))
(setq backup-directory-alist `(("." . ,backup-dir))
      auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-list-file-prefix autosave-dir
      backup-by-copying t
      vc-make-backup-files t
      version-control 1
      delete-old-versions -1
      kept-new-versions 10
      kept-old-versions 5)

;;; Bookmarks
(setq bookmark-save-flag 1)
(setq bookmark-default-file
      (expand-file-name "bookmark-file" user-emacs-directory))

;;; Buffers

;; Settings
(global-auto-revert-mode 1)
(auto-compression-mode t)
(setq load-prefer-newer t
      auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

;; Delete current buffer file
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

;; Rename current buffer file
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

;;; Calendar
(use-package calendar
  :ensure t
  :defer t
  :commands (calendar calendar-mode)
  :config (calendar-set-date-style 'iso))

;;; Case sensitivity
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      pcomplete-ignore-case t
      eshell-cmpl-ignore-case t)

;;; Commands/features/functions
(setq disabled-command-function nil)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Eshell

;; Parse git branch
(defun eshell/current-git-branch ()
  "Returns current git branch as a string, or the empty string if the
working directory is not in a git repo (or the git command is not found)."
  (interactive)
  (let ((pwd (expand-file-name (eshell/pwd))))
    (if (and (eshell-search-path "git") (locate-dominating-file pwd ".git"))
        (shell-command-to-string
         (concat "cd "
                 (expand-file-name (eshell/pwd))
                 " && git branch | grep '\*' | sed 's/\* //' | tr -d '\n'"))
      (concat ""))))

;; Prompt
(setq eshell-highlight-prompt nil
      eshell-history-size 25000
      eshell-prompt-function
      (lambda ()
        (concat
         (let ((git-branch (eshell/current-git-branch)))
           (if (= 0 (length git-branch))
               "" (concat "[" git-branch "]~")))
         "(" (user-login-name) "@" (system-name) ")~"
         "(" (eshell/pwd) ")~"
         (propertize (if (= (user-uid) 0) "#" "$")
                     'face `(:foreground "orchid"))
         " ")))

;;; Fill

;; Settings
(setq-default fill-column 80
              auto-fill-function 'do-auto-fill)
(setq comment-auto-fill-only-comments t)
(diminish 'auto-fill-function)

;; Remove hard wrapping/filling for paragraph
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum)) (fill-paragraph nil)))

;; Remove hard wrapping/filling for region
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum)) (fill-region start end)))

;;; Flyspell
(use-package flyspell
  :ensure t
  :defer t
  :commands (flyspell-mode flyspell-prog-mode flyspell-buffer flyspell-region)
  :config
  (define-key flyspell-mouse-map
    (kbd "C-<down-mouse-3>") #'flyspell-correct-word)
  (define-key flyspell-mouse-map
    (kbd "C-<mouse-3>") 'undefined))

;;; Garbage collection (use more RAM before doing GC)
(setq gc-cons-threshold 52428800)

;;; History
(setq history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-file (expand-file-name "history-file" user-emacs-directory)
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;;; Ido
(setq ido-everywhere t
      ido-enable-flex-matching t)
(setq ido-save-directory-list-file
      (expand-file-name "ido-file" user-emacs-directory))
(ido-mode 1)
(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode 1))

;;; LISP interaction stuff

;; Eval and replace last sexp function
(defun eval-replace-last-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0))) (current-buffer))
    (error (message "Invalid expression") (insert (current-kill 0)))))

;;; Org
(setq org-completion-use-ido t)

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (bind-key "r" 'rainbow-delimiters-mode ctl-x-m-map))

;;; Saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (expand-file-name "saveplace-file" user-emacs-directory))

;;; Scrolling
(setq scroll-margin 5
      scroll-conservatively 101)

;;; SLIME
(use-package slime
  :ensure t
  :defer t
  :commands (slime slime-connect slime-mode)
  :init
  ;; Quicklisp path
  (defvar quicklisp-path
    (expand-file-name "~/Source/quicklisp/")
    "The path to my quicklisp installation.")
  ;; Quicklisp SLIME helper
  (defvar quicklisp-slime-helper
    (concat quicklisp-path "slime-helper.el")
    "The location of the quicklisp-slime-helper elisp file.")
  :config
  ;; Initialize SLIME and helpers
  (when (file-exists-p quicklisp-slime-helper)
    (load quicklisp-slime-helper))
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;;; Smartparens
(use-package smartparens
  :ensure t
  :defer t
  :init (bind-key "p" 'smartparens-mode ctl-x-m-map))

;;; Smex
(use-package smex
  :ensure t
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-save-file
        (expand-file-name "smex-items-file" user-emacs-directory))
  (smex-initialize))

;;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

;;; Terminal emulator

;; Function for running $SHELL in ansi-term
(defun ansi-term-shell ()
  "Run the user's shell ($SHELL) in emacs' own ansi-term terminal emulator."
  (interactive) (ansi-term (getenv "SHELL")))

;; Don't show passwords
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;; TeX

;; External commands
(setq tex-run-command "pdflatex"
      tex-pdf-view-command "zathura"
      tex-pdf-print-command "lpr -d")

;; Hack to use PDF in place of DVI in the built-in functions
(defun tex-dvi-command-to-pdf (arg)
  (concat "(f=*; pdflatex \"${f%.dvi}.tex\" && "
          arg
          " \"${f%.dvi}.pdf\")"))
(setq tex-dvi-view-command (tex-dvi-command-to-pdf tex-pdf-view-command)
      tex-dvi-print-command (tex-dvi-command-to-pdf tex-pdf-print-command))

;;; TRAMP

;; Settings
(setq tramp-persistency-file-name
      (expand-file-name "tramp-file" user-emacs-directory))

;; Edit a file as root using sudo
(defun sudo-edit (&optional arg)
  "Edit a file as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file
       (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

;;; Undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;;; Unicode
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Whitespace
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

;;; Window/frame management

;; Toggle a 2-window split between horizontal and vertical split
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
              (if (= (car this-win-edges) (car (window-edges (next-window))))
                  'split-window-horizontally 'split-window-vertically))
             )
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Winner mode
(winner-mode 1)

;;; X windows options
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;;; Yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  (setq yas-minor-mode-map (make-sparse-keymap)))

;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Keyboard

;; Top-level keys
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "C-<f1>") 'ansi-term)
(global-set-key (kbd "M-<f1>") 'ansi-term-shell)
(global-set-key (kbd "<f10>") 'menu-bar-mode)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "C-x C-h") 'mark-whole-buffer)
(global-set-key (kbd "C-x m") 'ctl-x-m-map)
(global-set-key (kbd "C-x M") 'compose-mail)
(global-set-key (kbd "C-c <up>") 'toggle-window-split)
(global-set-key (kbd "C-c <down>") 'toggle-window-split)
(global-set-key (kbd "M-RET") 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; C-x m map
(define-key ctl-x-m-map (kbd "f") 'flyspell-prog-mode)
(define-key ctl-x-m-map (kbd "s") 'flyspell-mode)
(define-key ctl-x-m-map (kbd "w") 'whitespace-mode)

;;; Mouse

;; Behaviour
(setq mouse-yank-at-point t)

;;; Evil
(load-file (expand-file-name "evil-setup.el" user-emacs-directory))

;;;; EOF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
