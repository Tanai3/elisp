;; Usage
;; (auto-revert-file t)

(defun auto-revert-file ()
  (interactive)
  (if (not(string-match "/Dropbox/" (buffer-file-name)))
      (message "%s" "no-auto-revert")
    (message "%s:%s" "auto-revert" (buffer-file-name))
    (auto-revert-mode t)))

(add-hook 'find-file-hooks 'auto-revert-file)
(setq auto-revert-mode-text "ARev")
(provide 'auto-revert-file)

