;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-


(defun ~/disable-line-numbers ()
  (setq-local display-line-numbers nil))


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
           "* %?\n:PROPERTIES:\n:Status:   Backlog\n:Created:  %U\n:END:" :empty-lines 1)))
  ;; Customize appearance
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars nil)

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
;; (add-hook! org-mode #'variable-pitch-mode #'~/disable-line-numbers)

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
                              "#+TITLE: ${title}\n\n")
           :unnarrowed t)
          ("n" "Course Notes" entry "* %u\n\n%?"
           :target (file+head+olp "courses/%<%Y%m%d%H%M%S>-${slug}.org"
                                  "#+TITLE: ${title}\n\n"
                                  ("Notes"))
           :empty-lines 1))))
