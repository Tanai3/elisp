(defun latex-pdf()
  (interactive)
  (if (eq major-mode  'latex-mode)
      (let ((buf (buffer-name))
	    (command1 "platex ")
	    (command2 "dvipdfmx ")
	    (command3 "extractbb "))
	(get-buffer-create "*result-latex-pdf*")
	(with-temp-buffer
	  (insert-file-contents (concat "./" buf))
	  (replace-regexp "、" "，")
	  (goto-char 0)
	  (replace-regexp "。" "．")
	  (write-region (point-min)(point-max) (concat "tmp-" buf))
	  (setq command1 (concat command1 (concat "tmp-" buf)))
	  (setq command2 (concat command2 (substring (concat "tmp-" buf) 0 -4) ".dvi"))
	  (shell-command command1 "*result-latex-pdf*" "*result-latex-pdf*")
	  (shell-command command2 "*result-latex-pdf*" "*result-latex-pdf*")
	))))
(provide 'latex-pdf)
