;;; $DOOMDIR/org.el -*- lexical-binding: t; -*-


;;; Org config

(after! org
  (setq-default org-cycle-emulate-tab nil
                org-attach-dir-relative t
                org-log-into-drawer t)
  (setq org-capture-templates
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
           "* %^T %?" :empty-lines 1)
          ("E" "Event (date only)" entry (file+headline "~/org/events.org" "Events")
           "* %^t %?" :empty-lines 1)
          ("p" "Project" entry (file "~/org/projects.org")
           "* %?\n:PROPERTIES:\n:Status:   Backlog\n:Created:  %U\n:END:" :empty-lines 1))))

(map! :localleader
      :after org
      :map org-mode-map
      "C" #'org-columns
      "c D" #'org-clock-display)


;;; Org journal

(after! org-journal
  (setq-default org-journal-file-format "%Y-%m-%d"
                org-extend-today-until 4
                org-journal-hide-entries-p nil))

(defun +org/org-journal-open-latest ()
  (interactive)
  (require 'org-journal)
  (funcall org-journal-find-file
           (car (last (seq-filter #'file-regular-p
             (directory-files org-journal-dir 'full))))))


;;; Org roam

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "Default" plain "%?"
           :target (file+head "%(read-directory-name \"Directory: \" org-roam-directory)%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("n" "Course Notes" entry "* %u\n\n%?"
           :target (file+head+olp "courses/%<%Y%m%d%H%M%S>-${slug}.org"
                                  "#+title: ${title}\n"
                                  ("Notes"))
           :empty-lines 1))))
