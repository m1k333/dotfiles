;;; Keybindings
;;; MSR 2014

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-RET") 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(provide 'keybindings)

