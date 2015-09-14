;;;; Appearance settings

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
(use-package material-theme
  :ensure t
  :if window-system
  :config (load-theme 'material-light t))

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

;;; Done
(provide 'appearance-setup)

;;;;
