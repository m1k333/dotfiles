;; MSR 2015
;; Functions for quickly inserting the current date and time in the
;; desired formats.

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

(defun insert-date (&optional format)
  (interactive "MStfrtime format [RET for default]: ")
  (insert (date format)))

(defun insert-standard-date ()
  (interactive)
  (insert (date)))

(provide 'insert-date)
