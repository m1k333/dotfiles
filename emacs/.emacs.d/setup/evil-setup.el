;;;; Evil settings

;;; Activate evil mode
(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init
  ;; Evil leader
  (use-package evil-leader
    :ensure t
    :config (global-evil-leader-mode))
  ;; Evil toggle between normal and emacs state
  (defun evil-toggle-vim-emacs-state ()
    "Toggle between evil's vim and emacs states."
    (interactive)
    (if (eq evil-state 'emacs)
        (evil-normal-state) (evil-emacs-state)))
  (bind-key "e" 'evil-toggle-vim-emacs-state 'ctl-x-m-map)
  ;; Evil default modes
  (evil-set-initial-state 'term-mode 'emacs)
  :config (evil-mode 1))

;;; Use evil packages
(use-package evil-anzu :ensure t)
(use-package evil-org :ensure t)
(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))
(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode 1))
(use-package evil-numbers
  :ensure t
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

;; Evil normal keys (default <leader> is (evil-leader/set-leader "\\"))
(evil-leader/set-key
  "d" 'evil-insert-digraph
  "e" 'evil-toggle-vim-emacs-state
  "h" 'help-command
  "m" 'ctl-x-m-map
  "t" 'untabify
  "w" 'whitespace-cleanup)

;; Move keys that interfere with emacs state from motion state to normal state
(dolist (key (list (kbd "RET") (kbd "SPC")))
  (define-key evil-normal-state-map key
    (lookup-key evil-motion-state-map key))
  (define-key evil-motion-state-map key nil))

;; Evil insert keys (essentially the same as Emacs mode)
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(define-key evil-insert-state-map [mouse-2] 'mouse-yank-primary)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; Evil - ESC quits out of everything
;; (Evil recognizes [escape] as a single press of ESC)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Evil-aware minibuffer quit command
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; Evil settings
(let ((grey    "#586e75")
      (green   "#859900")
      (violet  "#6c71c4")
      (magenta "#d33682")
      (red     "#dc322f"))
  (setq evil-mode-line-format nil ;; This was set in mode line section
        evil-emacs-state-cursor    '(grey    box)
        evil-insert-state-cursor   '(magenta bar)
        evil-normal-state-cursor   '(green   box)
        evil-motion-state-cursor   '(green   box)
        evil-operator-state-cursor '(red     hollow)
        evil-replace-state-cursor  '(red     bar)
        evil-visual-state-cursor   '(violet  box)
        evil-emacs-state-tag    "(EMACS)"
        evil-insert-state-tag   "(INSERT)"
        evil-motion-state-tag   "(MOTION)"
        evil-normal-state-tag   "(NORMAL)"
        evil-operator-state-tag "(OPERATOR)"
        evil-replace-state-tag  "(REPLACE)"
        evil-visual-state-tag   "(VISUAL)"))

;;; Done
(provide 'evil-setup)

;;;;
