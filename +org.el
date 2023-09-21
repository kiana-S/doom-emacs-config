;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-


;;; Org config

(after! org
  (setq org-cycle-emulate-tab nil
        org-attach-dir-relative t
        org-log-into-drawer t
        org-footnote-auto-label 'confirm
        org-agenda-span 'day
        org-agenda-start-day nil
        org-agenda-start-on-weekday 1

        org-todo-keywords
          '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "|" "DONE(d)")
            (sequence "|" "KILL(k)"))
        org-todo-keyword-faces
          '(("STRT" . +org-todo-active)
            ("WAIT" . +org-todo-onhold)
            ("KILL" . +org-todo-cancel)
            ("PROJ" . +org-todo-project)
            ("WORK" . +org-todo-active)
            ("HOLD" . +org-todo-onhold))
        org-capture-templates
        '(("t" "Task")
          ("tt" "Task" entry (file+headline "~/org/events.org" "Tasks")
           "* TODO %?" :empty-lines 1)
          ("td" "Task with Deadline" entry (file+headline "~/org/events.org" "Tasks")
           "* TODO %?\nDEADLINE: %^{Deadline}T" :empty-lines 1)
          ("tD" "Task with Deadline (date only)" entry (file+headline "~/org/events.org" "Tasks")
           "* TODO %?\nDEADLINE: %^{Deadline}t" :empty-lines 1)
          ("ts" "Scheduled Task" entry (file+headline "~/org/events.org" "Tasks")
           "* TODO %?\nSCHEDULED: %^{Time}T" :empty-lines 1)
          ("tS" "Scheduled Task (date only)" entry (file+headline "~/org/events.org" "Tasks")
           "* TODO %?\nSCHEDULED: %^{Date}t" :empty-lines 1)
          ("e" "Event" entry (file+headline "~/org/events.org" "Events")
           "* %?\n%^T" :empty-lines 1)
          ("E" "Event (date only)" entry (file+headline "~/org/events.org" "Events")
           "* %?\n$^t" :empty-lines 1)
          ("p" "Project" entry (file "~/org/projects.org")
           "* PROJ %?\n:LOGBOOK:\n- Created                              %U\n:END:"
           :empty-lines 1 :prepend t)))
  ;; Customize appearance
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars nil
        org-superstar-item-bullet-alist '((42 . 8226)
                                          (43 . 8226)
                                          (45 . 8226)))
  (~/org-agenda-files-update)

  ;; Face customization - not sure about this...
  ;; (custom-set-faces! '(org-level-4 :height 1.1 :inherit outline-4)
  ;;                    '(org-level-3 :height 1.2 :inherit outline-3)
  ;;                    '(org-level-2 :height 1.3 :inherit outline-2)
  ;;                    '(org-level-1 :height 1.5 :inherit outline-1)
  ;;                    '(org-document-title :height 1.75)
  ;;                    '(org-document-info :height 1.4)

  ;;                    '(org-block :inherit fixed-pitch)
  ;;                    '(org-drawer :inherit fixed-pitch)
  ;;                    '(org-document-info-keyword :inherit fixed-pitch)
  ;;                    '(org-link :foreground "royal blue" :underline t)
  ;;                    '(org-meta-line :inherit fixed-pitch)
  ;;                    '(org-property-value :inherit fixed-pitch)
  ;;                    '(org-special-keyword :inherit fixed-pitch)
  ;;                    '(org-table :inherit fixed-pitch)
  ;;                    '(org-tag :inherit fixed-pitch :weight bold :size 12)
  ;;                    '(org-verbatim :inherit fixed-pitch)
  ;;                    '(org-date :inherit fixed-pitch))
)
;; (add-hook! org-mode #'variable-pitch-mode
;;                     (lambda () (setq-local display-line-numbers nil)))

;; Org agenda

(defun directory-dirs (dir)
  "Find all subdirectories of DIR, ignoring dotfiles."
  (unless (file-directory-p dir)
    (user-error "Not a directory `%s'" dir))
  (let ((dirs '())
        (dir (directory-file-name dir))
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (= (aref file 0) ?.)
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (directory-dirs file))
                               dirs))))))
    dirs))


(defun org-agenda-files-function (get-dirs)
  (funcall get-dirs '("~/org")))

(defvar org-agenda-files-function #'org-agenda-files-function
  "The function to determine the org agenda files.")

(defun ~/org-agenda-files-update (&optional fn)
  "Populate `org-agenda-files' with the result of calling FN, or
`org-agenda-files-function' by default."
  (interactive)
  (unless fn
    (setq fn org-agenda-files-function))
  (setq org-agenda-files
        (funcall fn (lambda (dirs)
                      (append dirs (mapcan #'directory-dirs dirs))))))


(map! :localleader
      :after org
      :map org-mode-map
      "C" #'org-columns
      "c D" #'org-clock-display
      "m b f" #'org-table-eval-formula
      "m b F" #'org-table-edit-formulas

      "v" (lookup-key org-mode-map (kbd "C-c C-v")))


;; Exporting directory

(after! org
  (defvar org-export-dir "~/org/export/"
    "The directory to export Org mode files to.

If nil, then `default-directory' for the org buffer is used."))

(defadvice! ~/modify-org-export-dir (orig-fn extension &optional subtreep pub-dir)
    :around #'org-export-output-file-name
  (unless pub-dir
    (setq pub-dir org-export-dir))
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (funcall orig-fn extension subtreep pub-dir))


;;; Org journal

(after! org-journal
  (setq org-journal-file-format "%Y-%m-%d"
        org-extend-today-until 4
        org-journal-hide-entries-p nil))

(defun +org/org-journal-open-latest ()
  (interactive)
  (require 'org-journal)
  (funcall org-journal-find-file
           (car (last (seq-filter #'file-regular-p
             (directory-files org-journal-dir 'full))))))


;;; Org roam

(defun org-roam-node-file-maybe (node &optional dir)
  "Get file name from NODE, or return a default filename in directory DIR."
  (or (org-roam-node-file node)
      (expand-file-name (concat "%<%Y%m%d%H%M%S>-" (org-roam-node-slug node) ".org")
                        dir)))

(defun org-roam-node-file-maybe-pick-dir (node)
  "Get file name from NODE, or ask for directory and return a default filename."
  (org-roam-node-file-maybe
    node (read-directory-name "Directory: " org-roam-directory)))


(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "Default" plain "%?"
           :target (file+head "${file-maybe-pick-dir}"
                              "#+title: ${title}\n\n")
           :unnarrowed t)
          ("n" "Notes" entry "* %u\n\n%?"
           :target (file+head+olp "${file-maybe-pick-dir}"
                                  "#+title: ${title}\n\n"
                                  ("Notes"))
           :empty-lines 1))))

;; Projectile link type


(defun org-projectile-follow (path _)
  "Open a projectile link to PATH."
  (projectile-switch-project-by-name path))

(defun org-projectile-completion (&optional arg)
  (let ((project (completing-read "Project: " projectile-known-projects nil 'confirm)))
    (concat "projectile:" project)))

(defun org-roam-completion (&optional arg)
  (let ((node (org-roam-node-read nil nil nil t)))
    (concat "id:" (org-roam-node-id node))))

(after! org
  (org-link-set-parameters "projectile"
                           :follow #'org-projectile-follow
                           :complete #'org-projectile-completion)
  (org-link-set-parameters "roam"
                           :complete #'org-roam-completion))
