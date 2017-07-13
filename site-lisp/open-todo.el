(defvar open-todo-file "~/Dropbox/TODO.org")

(defun open-todo-start()
  (interactive)
  (find-file open-todo-file))

(defun open-todo-set-filepath(path)
  (setq open-todo-file path))
(provide 'open-todo)
