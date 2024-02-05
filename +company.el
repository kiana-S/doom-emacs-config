;;; $DOOMDIR/+company.el -*- lexical-binding: t; -*-


(after! company
  (require 'company-spell)
  (setq company-global-modes '(not idris2-mode idris2-repl-mode))
  (let ((item `(menu-item nil company-complete-selection
                :filter ,(lambda (cmd)
                           (when (company-explicit-action-p)
                             cmd)))))
    (map! :map company-active-map
          "RET" item
          "<return>" item
          "TAB" #'company-complete-selection
          "<tab>" #'company-complete-selection
          "S-TAB" #'company-complete-common)))

(after! company-spell
  ;; Use ispell's personal dictionary
  (setq company-spell-args
        (concat company-spell-args " -p " ispell-personal-dictionary))
  (map! :map evil-insert-state-map
        "C-x s" #'company-spell))

;; Icons

(defadvice! ~/company-spell-text-property (words)
  :filter-return #'company-spell-lookup-words
  (dolist (word words)
    (put-text-property 0 1 'spell-completion-item t word))
  words)

(defun ~/company-box-icons--spell (candidate)
  (when (get-text-property 0 'spell-completion-item candidate)
    'Text))

(defun ~/company-box-icons--text (candidate)
  (when (derived-mode-p 'text-mode) 'Text))

(after! company-box
  (pushnew! company-box-icons-functions #'~/company-box-icons--text)
  (setq company-box-icons-functions
        (append company-box-icons-functions '(~/company-box-icons--text))))

;; Company backends

(set-company-backend! 'text-mode
  '(:separate company-dabbrev company-yasnippet company-spell))
