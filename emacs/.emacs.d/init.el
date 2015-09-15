;;;; $HOME/.emacs.d/init.el
;;;; By Michael Richer
;;;; Since May 5th, 2014

;;;; Initialization

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

;;; Load other init files
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
(require 'appearance-setup)
(require 'keybindings-setup)
(require 'emacs-setup)
;;(require 'evil-setup)

;;;;
