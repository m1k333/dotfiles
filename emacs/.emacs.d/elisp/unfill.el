;; Idea by Xah Lee, 2010
;; Fixed portability by me, 2015

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;; EOF
(provide 'unfill)
