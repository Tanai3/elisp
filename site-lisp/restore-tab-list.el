(defvar restore-tab-list-file-path "~/.emacs.d/tablist")

(defvar restore-tab-list-mode-map (make-sparse-keymap))
(define-key restore-tab-list-mode-map (kbd "C-m") 'restore-tab-list-afile)

(defun restore-tab-list-afile()
  (interactive)
  (find-file (buffer-substring (point-at-bol)(point-at-eol))))

(defun restore-tab-list-mode()
  (interactive)
  (setq major-mode 'restore-tab-list-mode)
  (setq mode-name "last-tablist")
  (use-local-map restore-tab-list-mode-map)
  (toggle-read-only t))

(defun restore-tab-list-save-tabs()
  (with-temp-buffer
    (dolist (x (buffer-list))
      (if (equal " " (substring (buffer-name x) 0 1))
          ()
        (if (not(char-or-string-p (buffer-file-name x)))
            ()
          (insert (buffer-file-name x) "\n"))))
    (write-region (point-min)(point-max) restore-tab-list-file-path)))
(add-hook 'kill-emacs-hook 'restore-tab-list-save-tabs)

(defun restore-tab-list-open-list()
  (get-buffer-create "*last-tablists*")
  (switch-to-buffer "*last-tablists*")
  (insert-file-contents restore-tab-list-file-path)
  (restore-tab-list-mode))
(add-hook 'after-init-hook 'restore-tab-list-open-list)

(provide 'restore-tab-list)
