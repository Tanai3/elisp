;; 対応するカッコをカッコの中身ごと消去
(defun delete-paren()
  (interactive)
  (save-excursion
    (let ((cp (point))
	  (blp)
	  (bcp))
      (if (equal nil (string-match "[)}]" (buffer-substring (progn
                                                              (backward-char)
                                                              (setq bcp (point)))
                                                            cp) 0))
          (message "No paren")
        (goto-char cp)
        (backward-list)
        (setq blp (point))
        (kill-region cp blp)))))
(provide 'delete-paren)
