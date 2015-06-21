;;;; $HOME/.emacs.d/init.el
;;;; By Michael Richer
;;;; Since May 5th, 2014

;;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Common lisp functionality
(require 'cl)

;;; Add the package repositories and intialize
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; Define `package-required-list'
(defvar package-required-list
  '(anzu
    async
    auctex
    diminish
    evil
    evil-anzu
    evil-leader
    evil-numbers
    evil-org
    evil-surround
    evil-visualstar
    flx-ido
    ido-ubiquitous
    magit
    smartparens
    solarized-theme
    slime
    smex
    vi-tilde-fringe
    yasnippet)
  "A list of packages that should be installed for this Emacs
configuration.  If any are not installed, they should be able
to be installed by running `package-populate'.")

;;; Test for missing packages
(defun package-missing ()
  "Test for missing packages from the list of desired packages in
the `package-required-list' variable."
  (catch 'done
    (unless package-required-list (throw 'done nil))
    (dolist (pkg package-required-list)
      (unless (package-installed-p pkg)
        (throw 'done t)))))

;;; Populate the system with the desired packages
(defun package-populate ()
  "If packages from `package-required-list' are missing, install them."
  (interactive)
  (when (package-missing)
    (package-refresh-contents)
    (dolist (pkg package-required-list)
      (unless (package-installed-p pkg)
        (package-install pkg)))
    (package-initialize)))

;;; Do the package installs if required
(package-populate)

;;;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GUI settings
(show-paren-mode 1)
(setq echo-keystrokes 0.1
      font-lock-maximum-decoration t
      global-font-lock-mode t
      show-paren-delay 0)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (window-system)
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

;;; Mode line
(setq display-time-24hr-format t
      show-help-function nil)
(column-number-mode)
(line-number-mode)
(size-indication-mode)
(tooltip-mode -1)

;;; Vi tilde fringe
(global-vi-tilde-fringe-mode)
(diminish 'vi-tilde-fringe-mode)

;;;; Keybindings and mouse bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Keyboard

;; Mode map keys
(define-prefix-command 'ctl-x-m-map)
(global-set-key (kbd "C-x m") 'ctl-x-m-map)
(define-key ctl-x-m-map (kbd "e") 'evil-mode)
(define-key ctl-x-m-map (kbd "f") 'flyspell-prog-mode)
(define-key ctl-x-m-map (kbd "p") 'smartparens-global-mode)
(define-key ctl-x-m-map (kbd "s") 'flyspell-mode)
(define-key ctl-x-m-map (kbd "s") 'yas-minor-mode)

;; Top-level keys
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "C-<f1>") 'shell)
(global-set-key (kbd "M-<f1>") 'ansi-term)
(global-set-key (kbd "<f10>") 'menu-bar-mode)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "C-x C-h") 'mark-whole-buffer)
(global-set-key (kbd "C-x M") 'compose-mail)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-RET") 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Keys that need to be defined later in the init file
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

;;; Mouse
(setq mouse-yank-at-point t)

;;;; Emacs settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Apropos
(setq apropos-do-all t)

;;; Anzu
(global-anzu-mode 1)
(diminish 'anzu-mode)

;;; Backups and autosave
(defvar backup-dir (expand-file-name "backup" user-emacs-directory))
(defvar autosave-dir (expand-file-name "autosave" user-emacs-directory))
(setq backup-directory-alist `(("." . ,backup-dir))
      auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-list-file-prefix autosave-dir
      backup-by-copying t
      delete-old-versions -1
      version-control 1
      vc-make-backup-files t
      kept-new-versions 10
      kept-old-versions 5)

;;; Buffers and files

;; Various useful things
(global-auto-revert-mode 1)
(auto-compression-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      load-prefer-newer t)

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

;;; Calendar and date stuff

;; Set up calendar
(require 'calendar)
(calendar-set-date-style 'iso)

;;; Case sensitivity
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      pcomplete-ignore-case t
      eshell-cmpl-ignore-case t)

;;; Commands/features/functions
(setq disabled-command-hook nil)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Evil mode

;; Evil functionality
(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)

;; Evil leader mode
(global-evil-leader-mode)
(evil-leader/set-key "e" 'find-file)
(evil-leader/set-key "h" 'help-command)
(evil-leader/set-key "w" 'whitespace-cleanup)

;; Escape quits out of everything!
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)

;; Activate evil mode
(evil-mode 1)

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

;;; Git
(setq magit-last-seen-setup-instructions "1.4.0")

;;; History
(setq savehist-file (expand-file-name "history-file" user-emacs-directory))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;;; Ido

;; Enable Ido everywhere humanly possible
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-save-directory-list-file (expand-file-name "ido-file" user-emacs-directory)
      ido-use-faces nil
      org-completion-use-ido t
      magit-completing-read-function 'magit-ido-completing-read)
(ido-mode 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode 1)

;; RAM is cheap now; let Ido use it
(setq flx-ido-threshold 20000
      gc-cons-threshold 20000000)

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

;; Quicklisp path
(defvar quicklisp-path
  (expand-file-name "~/quicklisp/")
  "The path to my quicklisp installation.")

;; Quicklisp SLIME helper
(defvar quicklisp-slime-helper
  (concat quicklisp-path "slime-helper.el")
  "The location of the quicklisp-slime-helper elisp file.")

;; Initialize SLIME and helpers
(when (file-exists-p quicklisp-slime-helper)
  (load quicklisp-slime-helper))
(setq inferior-lisp-program "/usr/bin/sbcl")

;;; Recentf mode
(setq recentf-max-saved-items 50
      recentf-max-menu-items recentf-max-saved-items
      recentf-save-file (expand-file-name "recentf-file" user-emacs-directory))
(recentf-mode 1)

;;; Saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "saveplace-file" user-emacs-directory))

;;; Smex
(setq smex-save-file (expand-file-name "smex-items-file" user-emacs-directory))
(smex-initialize)

;;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

;;; Tabs
(setq-default indent-tabs-mode nil)

;;; TRAMP

;; Edit a file as root using sudo (from `what the .emacs.d?!')
(defun sudo-edit (&optional arg)
  "Edit a file as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file
        (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file
      (concat "/sudo:root@localhost:" buffer-file-name))))

;;; Undo tree
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

;;; Unicode
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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

;; Winner mode
(winner-mode 1)

;;; X windows options
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;;; Yasnippet
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
(diminish 'yas-minor-mode)

;;;; EOF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
