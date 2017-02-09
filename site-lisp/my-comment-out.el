;; my-comment-out
;; 現在カーソルの行をコメントアウトorアンコメントアウト
(defun mco-empty-line-p()
  (string-match "^[[:blank:]]*$" (buffer-substring (line-beginning-position)(line-end-position))))

(defun mco-current-major-mode()
  (with-current-buffer (current-buffer) major-mode))

(defun my-comment-out()
  (interactive)
  (save-excursion
    (if(use-region-p)
	(comment-or-uncomment-region(region-beginning)(region-end))
      (if(not(mco-empty-line-p))
	  (comment-or-uncomment-region(line-beginning-position)(line-end-position))
	(beginning-of-line)
	(if (eq (mco-current-major-mode) 'emacs-lisp-mode)
	    (insert ";;")
	  (if (or (eq (mco-current-major-mode) 'html-mode) (eq (mco-current-major-mode) 'xhtml-mode))
	      (insert "<!-- -->")
	    (insert comment-start)))))))

(defun past1-my-comment-out()
  (interactive)
  (save-excursion
    (let ((pStart)(pEnd))
	 (beginning-of-line)
	 (setq pStart (point))
	 (make-marker)
	 (push-mark (point))
	 (end-of-line)
	 (setq pEnd (point))
	 (if (equal pStart pEnd)
	     ;; (message "This line is a blank")
	     (insert comment-start)
	   (comment-or-uncomment-region(region-beginning)(region-end))))))
(provide 'my-comment-out)
