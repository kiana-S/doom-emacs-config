;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-


;; General keybindings

(map! :leader

  :desc "Find file in project"
    "SPC" #'projectile--find-file
  :desc "Find file in project"
    "p f" #'projectile--find-file

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
    "o c" #'cfw:open-org-calendar
  :desc "Journal"
    "o j" #'+org/org-journal-open-latest

  "o A" nil
  :desc "Org agenda"
    "o a" #'org-agenda

  :desc "Select LSP code lens"
    "c L" #'lsp-avy-lens
  :desc "Open errors buffer"
    "c X" #'flymake-show-buffer-diagnostics

  :desc "Open URL"
    "s u" #'goto-address-at-point)

;; Rebind macro key
(map! :map evil-normal-state-map
      "q" nil
      "C-q" #'evil-record-macro)

;; calc mode
(defadvice! ~/evil-collection-calc-bindings ()
  :after #'evil-collection-calc-setup
  (map! :map calc-mode-map
    :n "C-r" #'calc-redo
    :n "[" #'calc-begin-vector
    :n "]" #'calc-end-vector))

(defun ~/calc-grab-region (top bot &optional arg)
  "Perform either `calc-grab-region' or `calc-grab-rectangle' depending on
what type of visual state is currently active."
  (interactive "r\nP")
  (if (eq (evil-visual-type) 'block)
      (calc-grab-rectangle top bot arg)
    (calc-grab-region top bot arg)))


(map! :leader
  (:prefix ("#" . "calc")
          :desc "Emacs Calc"
            "#" #'calc
          :desc "Emacs Calc"
            "c" #'calc
          :desc "Emacs Calc (full window)"
            "C" #'full-calc
          :desc "Quick Calc"
            "q" #'quick-calc
          :desc "Keypad"
            "k" #'calc-keypad
          :desc "Grab region into Calc"
            "g" #'~/calc-grab-region
          :desc "Paste from stack"
            "y" #'calc-copy-to-buffer
          :desc "Read keyboard macro"
            "m" #'read-kbd-macro

          (:prefix ("e" . "embedded")
                  :desc "Embedded mode"
                    "e" #'calc-embedded
                  :desc "Embedded mode (select)"
                    "s" #'calc-embedded-select
                  :desc "Embedded mode (word)"
                    "w" #'calc-embedded-word

                  :desc "Activate special operators"
                    "a" #'calc-embedded-activate
                  :desc "Duplicate formula at point"
                    "d" #'calc-embedded-duplicate
                  :desc "New formula"
                    "f" #'calc-embedded-new-formula
                  :desc "Next formula"
                    "j" #'calc-embedded-next
                  :desc "Previous formula"
                    "k" #'calc-embedded-previous
                  :desc "Refresh formula at point"
                    "r" #'calc-embedded-update-formula
                  :desc "Edit formula at point"
                    "`" #'calc-embedded-edit)))
