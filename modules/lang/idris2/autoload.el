;;; lang/idris2/config.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +idris2/jump-to-definition ()
  "Move cursor to the definition of the name at point."
  (interactive)
  (let* ((name (car (idris2-thing-at-point t)))
         (res (car (idris2-eval (list :name-at name))))
    (if (null res)
        (user-error "Symbol '%s' not found" name)
    (if (null (cdr res))
        (idris2-jump-to-location (car res) t)
    (idris2-jump-to-location
      (assoc (completing-read "Name: " res nil t) res) t))))))
