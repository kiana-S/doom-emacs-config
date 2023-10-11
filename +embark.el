;;; $DOOMDIR/+embark.el -*- lexical-binding: t; -*-


;; Targets

(defun ~/embark-target-prog-mode (old-fn)
  "Advise an embark target to only activate in `prog-mode'."
  (when (derived-mode-p 'prog-mode) (funcall old-fn)))

(defun ~/embark-target-identifier (old-fn)
  "Advise an embark target to only activate in `prog-mode' and not in `lsp-mode'."
  (when (and (derived-mode-p 'prog-mode) (not lsp-mode)) (funcall old-fn)))

(advice-add #'embark-target-expression-at-point :around #'~/embark-target-prog-mode)
(advice-add #'embark-target-identifier-at-point :around #'~/embark-target-identifier)

(defun embark-target-lsp-symbol-at-point ()
  (when lsp-mode
    (require 'lsp-ui-doc)
    (when-let ((bounds (lsp-ui-doc--extract-bounds
                        (lsp-request "textDocument/hover"
                                     (lsp--text-document-position-params)))))
      (cons 'lsp-symbol
            (cons (buffer-substring (car bounds) (cdr bounds))
                  bounds)))))

(after! embark
  (embark-define-thingatpt-target defun emacs-lisp-mode)

  (embark-define-thingatpt-target word
    text-mode help-mode Info-mode man-common)
  (pushnew! embark-target-finders #'embark-target-word-at-point #'embark-target-lsp-symbol-at-point))


;; Actions

(defun ~/embark-change (beg end)
  (interactive "r")
  (kill-region beg end)
  (evil-insert-state))


(after! embark
  ;; Invert mark hook to set the mark to the beginning instead of the end
  (cl-defun embark--mark-target (&rest rest &key run bounds &allow-other-keys)
    "Mark the target if its BOUNDS are known.
After marking the target, call RUN with the REST of its arguments."
    (cond
     ((and bounds run)
       (save-mark-and-excursion
         (set-mark (car bounds))
         (goto-char (cdr bounds))
         (apply run :bounds bounds rest)))
     (bounds ;; used as pre- or post-action hook
       (set-mark (car bounds))
       (goto-char (cdr bounds)))
     (run (apply run rest))))

  (cl-pushnew #'embark--mark-target
        (alist-get #'~/embark-change embark-around-action-hooks))
  (cl-pushnew #'embark--mark-target
        (alist-get #'+eval:region embark-around-action-hooks))
  (cl-pushnew #'embark--mark-target
        (alist-get #'+eval:replace-region embark-around-action-hooks))
  (cl-pushnew #'embark--ignore-target
        (alist-get #'lsp-rename embark-target-injection-hooks))
  (cl-pushnew #'embark--beginning-of-target
        (alist-get #'backward-word embark-pre-action-hooks))
  (cl-pushnew #'embark--end-of-target
        (alist-get #'forward-word embark-pre-action-hooks))
  (cl-pushnew #'embark--ignore-target
        (alist-get #'+spell/correct embark-target-injection-hooks))

  (pushnew! embark-repeat-actions
            #'lsp-ui-find-next-reference
            #'lsp-ui-find-prev-reference
            #'forward-word
            #'backward-word)

  (setf (alist-get #'kill-buffer embark-pre-action-hooks nil t) nil
        (alist-get #'embark-kill-buffer-and-window embark-pre-action-hooks nil t) nil
        (alist-get #'bookmark-delete embark-pre-action-hooks nil t) nil
        (alist-get #'tab-bar-close-tab-by-name embark-pre-action-hooks nil t) nil))

;; Keymaps

(defmacro ~/embark-target-wrapper (fn prompt)
  (let ((fsym (make-symbol (symbol-name fn))))
  ;;; Love me some uninterned symbols
    `(progn
      (defun ,fsym (ident &optional arg)
        ,(documentation fn)
        (interactive (list (read-from-minibuffer ,prompt) current-prefix-arg))
        (,fn ident arg))
      #',fsym)))


(after! embark
  (defvar-keymap embark-word-map
    :doc "Keymap for Embark word actions."
    :parent embark-general-map
    "j" #'forward-word
    "k" #'backward-word
    "$" #'+spell/correct)
  (defvar-keymap embark-lsp-symbol-map
    :doc "Keymap for Embark LSP symbol actions."
    :parent embark-identifier-map
    "j" #'lsp-ui-find-next-reference
    "k" #'lsp-ui-find-prev-reference
    "r" #'lsp-rename)

  (pushnew! embark-keymap-alist
            '(word . embark-word-map)
            '(lsp-symbol . embark-lsp-symbol-map))

  (map! (:map embark-general-map
         "y" #'embark-copy-as-kill
         "v" #'mark
         "C-q" #'embark-toggle-quit
         "d" #'kill-region
         "c" #'~/embark-change
         "/" #'evil-ex-search-forward
         "?" #'evil-ex-search-backward
         "w" nil "C-SPC" nil "q" nil "C-s" nil "C-r" nil)
        (:map embark-heading-map
         "v" #'mark
         "V" #'outline-mark-subtree
         "j" #'outline-next-visible-heading
         "k" #'outline-previous-visible-heading
         "J" #'outline-forward-same-level
         "K" #'outline-backward-same-level
         "h" #'outline-up-heading
         "M-j" #'outline-move-subtree-down
         "M-k" #'outline-move-subtree-up
         "M-l" #'outline-demote
         "M-h" #'outline-promote
         "n" nil "p" nil "f" nil "b" nil "^" nil
         "u" nil "C-SPC" nil)
        (:map embark-prose-map
         "c" #'~/embark-change
         "u" #'downcase-region
         "U" #'upcase-region
         "q" #'fill-region
         "C" #'capitalize-region
         "l" nil "f" nil)
        (:map embark-sentence-map
         "j" #'forward-sentence
         "k" #'backward-sentence
         "n" nil "p" nil)
        (:map embark-paragraph-map
         "j" #'forward-paragraph
         "k" #'backward-paragraph
         "n" nil "p" nil)
        (:map embark-identifier-map
         "j" #'embark-next-symbol
         "k" #'embark-previous-symbol
         "d" #'kill-region
         "RET" (~/embark-target-wrapper +lookup/definition "Identifier: ")
         "K" (~/embark-target-wrapper +lookup/documentation "Identifier: ")
         "D" (~/embark-target-wrapper +lookup/definition "Identifier: ")
         "R" (~/embark-target-wrapper +lookup/references "Identifier: ")
         "n" nil "p" nil "r" nil "a" nil "o" nil "H" nil "$" nil)
        (:map embark-expression-map
         "j" #'forward-list
         "k" #'backward-list
         "h" #'backward-up-list
         "=" #'indent-region
         "RET" #'+eval:region
         "e" #'+eval:region
         "E" #'+eval:replace-region
         "TAB" nil "<" nil "u" nil "n" nil "p" nil)
        (:map embark-defun-map
         "C" #'compile-defun
         "RET" nil "e" nil "c" nil)
        (:map embark-symbol-map
         "s" nil "h" nil "d" nil "e" nil)
        (:map embark-variable-map
         "Y" #'embark-save-variable-value
         "RET" #'+eval:region
         "e" #'+eval:region
         "E" #'+eval:replace-region
         "i" #'embark-insert-variable-value
         "v" #'mark
         "c" #'~/embark-change
         "<" nil)
        (:map embark-function-map
         "e" #'debug-on-entry
         "E" #'cancel-debug-on-entry
         "K" nil)
        (:map embark-command-map
         "w" #'where-is
         "b" nil "g" nil "l" nil)
        (:map embark-package-map
         "Y" #'embark-save-package-url
         "i" #'embark-insert
         "a" nil "I" nil "d" nil "r" nil "W" nil)
        (:map embark-unicode-name-map
         "Y" #'embark-save-unicode-character
         "W" nil)
        (:map embark-flymake-map
         "j" #'flymake-goto-next-error
         "k" #'flymake-goto-prev-error
         "n" nil "p" nil)
        (:map embark-tab-map
         "d" #'tab-bar-close-tab-by-name)
        (:map embark-region-map
         "u" #'downcase-region
         "U" #'upcase-region
         "C" #'capitalize-region
         "w" #'write-region
         "W" #'count-words-region
         "q" #'fill-region
         "Q" #'fill-region-as-paragraph
         "N" #'narrow-to-region
         "D" #'delete-duplicate-lines
         "=" #'indent-region
         "g" #'vc-region-history
         "d" #'kill-region
         "c" #'~/embark-change
         "TAB" nil "n" nil "l" nil "f" nil "p" nil
         "*" nil ":" nil "_" nil)
        (:map embark-file-map
         "g" 'embark-vc-file-map
         "Y" #'copy-file
         "v" #'mark
         "c" #'~/embark-change)))

(after! embark-org
  (map! (:map embark-org-table-cell-map
         "v" #'mark
         "-" #'org-table-insert-hline
         "H" #'org-table-move-column-left
         "L" #'org-table-move-column-right
         "J" #'org-table-move-row-down
         "K" #'org-table-move-row-up
         (:prefix ("i" . "insert")
                  "h" #'+org/table-insert-column-left
                  "l" #'org-table-insert-column
                  "j" #'+org/table-insert-row-below
                  "k" #'org-table-insert-row
                  "-" #'org-table-insert-hline)
         "^" nil "<" nil ">" nil "h" nil)
        (:map embark-org-table-map
         "p" #'org-table-paste-rectangle
         "C" #'org-table-convert
         "D" #'org-table-toggle-formula-debugger
         "y" #'embark-copy-as-kill
         "d" #'kill-region
         "c" #'~/embark-change)
        (:map embark-org-link-copy-map
         "y" #'embark-org-copy-link-in-full
         "w" nil)
        (:map embark-org-link-map
         "e" #'org-insert-link
         "y" 'embark-org-link-copy-map
         "w" nil)
        (:map embark-org-heading-map
         ">" #'org-do-demote
         "<" #'org-do-promote
         "j" #'org-next-visible-heading
         "k" #'org-previous-visible-heading
         "J" #'org-forward-heading-same-level
         "K" #'org-backward-heading-same-level
         "q" #'org-set-tags-command
         "o" #'org-set-property
         "D" #'org-cut-subtree
         "s" #'org-sort
         "i" #'embark-insert
         "d" #'kill-region
         "I" #'org-insert-heading-respect-content
         "l" #'org-store-link
         "L" #'embark-live
         (:prefix ("t" . "time")
                  "d" #'org-deadline
                  "s" #'org-schedule)
         (:prefix ("c" . "clock")
                  "i" #'org-clock-in
                  "o" #'org-clock-out))
        (:map embark-org-src-block-map
         "v" #'org-babel-mark-block
         "y" #'embark-org-copy-block-contents
         "Y" #'embark-copy-as-kill
         "D" #'org-babel-remove-result-one-or-many
         "j" #'org-babel-next-src-block
         "k" #'org-babel-previous-src-block
         "e" #'org-edit-special
         "=" #'org-indent-block
         "c" #'~/embark-change)
        (:map embark-org-inline-src-block-map
         "e" #'org-edit-inline-src-code
         "D" #'org-babel-remove-inline-result
         "k" nil)
        (:map embark-org-babel-call-map
         "D" #'org-babel-remove-result
         "k" nil)
        (:map embark-org-item-map
         "j" #'org-next-item
         "k" #'org-previous-item
         "M-j" #'org-move-item-down
         "M-k" #'org-move-item-up
         "c" #'~/embark-change
         "n" nil "p" nil)
        (:map embark-org-plain-list-map
         "c" #'~/embark-change
         "C" #'org-toggle-checkbox)
        (:map embark-org-agenda-item-map
         "RET" #'org-agenda-switch-to
         "TAB" #'org-agenda-goto
         "j" #'org-agenda-next-item
         "k" #'org-agenda-previous-item
         "d" #'org-agenda-kill
         "q" #'org-agenda-set-tags
         "o" #'org-agenda-set-property
         (:prefix ("t" . "time")
                  "d" #'org-agenda-deadline
                  "s" #'org-agenda-schedule)
         (:prefix ("c" . "clock")
                  "i" #'org-agenda-clock-in
                  "o" #'org-agenda-clock-out)
         "u" nil "i" nil ":" nil "s" nil "P" nil)))