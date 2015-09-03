;;;; Keybindings

;;; Keyboard

;; Keymaps
(define-prefix-command 'ctl-x-m-map)

;; Top-level keys
(bind-key (kbd "<f1>") 'eshell)
(bind-key (kbd "C-<f1>") 'ansi-term)
(bind-key (kbd "M-<f1>") 'ansi-term-shell)
(bind-key (kbd "<f10>") 'menu-bar-mode)
(bind-key (kbd "M-/") 'hippie-expand)
(bind-key (kbd "M-?") 'mark-paragraph)
(bind-key (kbd "C-h") 'backward-delete-char-untabify)
(bind-key (kbd "M-h") 'backward-kill-word)
(bind-key (kbd "C-s") 'isearch-forward-regexp)
(bind-key (kbd "C-r") 'isearch-backward-regexp)
(bind-key (kbd "C-M-s") 'isearch-forward)
(bind-key (kbd "C-M-r") 'isearch-backward)
(bind-key (kbd "C-x h") 'help-command)
(bind-key (kbd "C-x C-h") 'mark-whole-buffer)
(bind-key (kbd "C-x m") 'ctl-x-m-map)
(bind-key (kbd "C-x M") 'compose-mail)
(bind-key (kbd "C-c <up>") 'toggle-window-split)
(bind-key (kbd "C-c <down>") 'toggle-window-split)
(bind-key (kbd "M-RET") 'newline-and-indent)
(bind-key (kbd "M-SPC") 'cycle-spacing)

;; C-x m map
(bind-key (kbd "f") 'flyspell-prog-mode ctl-x-m-map)
(bind-key (kbd "s") 'flyspell-mode ctl-x-m-map)
(bind-key (kbd "w") 'whitespace-mode ctl-x-m-map)

;;; Mouse

;; Behaviour
(setq mouse-yank-at-point t)

;;; Done
(provide 'keybindings-setup)

;;;;
