;;; $DOOMDIR/bindings.el -*- lexical-binding: t; -*-


;; General keybindings

(map! :leader
  ;; Bind "SPC 0" to treemacs
  ;; Map window bindings to "SPC 1" through "SPC 9"
  "w 0" #'treemacs-select-window
  :desc "Select project tree window"
    "0" #'treemacs-select-window
  :desc "Select window 1"
    "1" #'winum-select-window-1
  :desc "Select window 2"
    "2" #'winum-select-window-2
  :desc "Select window 3"
    "3" #'winum-select-window-3
  :desc "Select window 4"
    "4" #'winum-select-window-4
  :desc "Select window 5"
    "5" #'winum-select-window-5
  :desc "Select window 6"
    "6" #'winum-select-window-6 :desc "Select window 7"
    "7" #'winum-select-window-7
  :desc "Select window 8"
    "8" #'winum-select-window-8
  :desc "Select window 9"
    "9" #'winum-select-window-9

  :desc "Create new project"
    "p n" #'create-new-project

  ;; Replace M-x binding with something more useful
  "w :" nil
  :desc "Ex"
    ":" #'evil-ex

  :desc "Move workspace left"
    "TAB h" #'+workspace/swap-left
  :desc "Move workspace right"
    "TAB l" #'+workspace/swap-right

  :desc "Calendar"
    "o c" #'+calendar/open-calendar
  :desc "Journal"
    "o j" #'+org/org-journal-open-latest

  "o A" nil
  :desc "Org agenda"
    "o a" #'org-agenda

  :desc "Open errors buffer"
    "c X" #'flymake-show-buffer-diagnostics)

;; Rebind macro key
(map! :map evil-normal-state-map
      "q" nil
      "C-q" #'evil-record-macro)
