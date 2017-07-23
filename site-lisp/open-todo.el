(defvar open-todo-file "~/Dropbox/TODO.org")

(defun open-todo-start(bool)
  (interactive)
  (if (and bool (not(file-exists-p open-todo-file)))
      ()
  (find-file open-todo-file)))

(defun open-todo-set-filepath(path)
  (setq open-todo-file path))
(provide 'open-todo)
