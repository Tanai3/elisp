;;
;;

(require 'google-translate)
(require 'popwin)
(custom-set-variables
  '(google-translate-default-source-language "en")
  '(google-translate-default-target-language "ja"))
(delete '("*Google Translate*") popwin:special-display-config)


(defun special-exercises-blank-line-p()
  (string-match "^[[:blank:]]*$" (buffer-substring (line-beginning-position)(line-end-position))))

(defun special-exercises()
  (interactive)
  (save-excursion
	()))

(defun special-exercises-shaping-buffer()
  (interactive)
  (save-excursion
	(goto-char 0)
	(while(not(eobp))
	  (if (search-forward "." (point-at-eol) t nil)
		  (newline)
		(if (test-func-blank-line-p)
			(next-line 1)
		  (goto-char (point-at-eol))
		  (delete-char 1)
		  (insert " "))))))

(defun special-exercises-translate-en-ja()
  (interactive)
  (let ((save-point (point))
		(current-point)
		(translate-words)
		(edit-buffer (current-buffer)))
	(while (re-search-forward "." nil t nil)
	  (goto-char (point-at-eol))
	  (setq current-point (point))
	  (set-mark (point))
	  (goto-char (point-at-bol))
	  (setq save-point current-point)
	  (google-translate-at-point)
	  (sleep-for 3)
	  (switch-to-buffer "*Google Translate*")
	  (re-search-forward "[^\x01-\x7E]" nil t nil)
	  (setq translate-words (buffer-substring (point-at-bol) (point-max)))
	  (switch-to-buffer edit-buffer)
	  (goto-char save-point)
	  (newline)
	  (insert translate-words)))
  (message "%s" "finished"))

(provide 'special-exercises)
