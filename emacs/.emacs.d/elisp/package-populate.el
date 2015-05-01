;; MSR 2015-04-27 15:43:32
;; Install missing packages and initialize them

;; Require `package.el'
(require 'package)

;; Set the packages you want
(defvar package-required-list nil
  "A list of packages that should be installed for this Emacs
configuration.  If any are not installed, they should be able
to be installed by running `package-populate'.")

;; Add the package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Initialize
(package-initialize)

;;; Functions

;; Test for missing packages
(defun package-missing ()
  "Test for missing packages from the list of desired packages in
the `package-required-list' variable."
  (catch 'done
    (unless package-required-list (throw 'done nil))
    (dolist (pkg package-required-list)
      (when (not (package-installed-p pkg)) (throw 'done t)))))

;; Populate the system with the desired packages
(defun package-populate ()
  "If packages from `package-required-list' are missing, install them."
  (interactive)
  (when (package-missing)
    (package-refresh-contents)
    (dolist (p package-required-list)
      (when (not (package-installed-p p)) (package-install p)))
    (package-initialize)))

(provide 'package-populate)
