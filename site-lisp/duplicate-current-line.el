;; duplicate current line without kill-ring
(defun duplicate-current-line()
  (interactive)
  (let ((current-line))
    (setq current-line (buffer-substring (point-at-bol)(point-at-eol)))
    (end-of-line)
    (newline)
    (insert current-line)))
(provide 'duplicate-current-line)
