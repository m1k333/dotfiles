;;;; Emacs settings

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
                                           user-emacs-directory)))

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
  (bind-key (kbd "C-<down-mouse-3>") #'flyspell-correct-word flyspell-mouse-map)
  (bind-key (kbd "C-<mouse-3>") 'undefined flyspell-mouse-map))

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

;;; Python
(use-package anaconda-mode
  :ensure t
  :defer t
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))
(use-package cython-mode :ensure t)

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

;;; Done
(provide 'emacs-setup)

;;;;
