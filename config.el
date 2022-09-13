 ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Kiana Sheibani"
      user-mail-address "kiana.a.sheibani@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "VictorMono" :size 12 :weight 'semi-bold))
(setq doom-variable-pitch-font (font-spec :family "NotoSans" :size 14 :weight 'regular))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-variable-name-face :slant italic))


;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)
(after! doom-themes
  (setq doom-themes-treemacs-theme "doom-colors"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :map evil-normal-state-map "q" nil)

(use-package! pinentry
  :init (setq-default epg-pinentry-mode `loopback)
        (pinentry-start))

;; Restart pinentry due to inactivity
(defun pinentry-restart ()
  "Restart a Pinentry service."
  (interactive)
  (pinentry-stop) (pinentry-start))


(after! evil-escape
  (setq-default evil-escape-key-sequence "fd"))

;; Idris 2
(after! idris2-mode
  (custom-set-faces! '(idris2-operator-face :slant normal :inherit font-lock-variable-name-face))
  (set-face-foreground 'idris2-semantic-data-face "#f7768e")
  (set-face-foreground 'idris2-semantic-function-face "#9ece6a")
  (set-face-foreground 'idris2-semantic-bound-face "#bb9af7")

  ;; Close windows instead of killing buffers
  (map! :map idris2-compiler-notes-mode-map "q" #'quit-window)
  (map! :map idris2-info-mode-map "q" #'quit-window)
  (map! :map idris2-hole-list-mode-map "q" #'quit-window))

(add-hook 'idris2-mode-hook #'turn-on-idris2-simple-indent)
(set-repl-handler! 'idris2-mode 'idris2-pop-to-repl)
(set-lookup-handlers! 'idris2-mode
  :documentation #'idris2-docs-at-point)
(setq evil-emacs-state-modes (cons 'idris2-repl-mode evil-emacs-state-modes))
(map! :localleader
      :map idris2-mode-map
      "q" #'idris2-quit
      "r" #'idris2-pop-to-repl
      "l" #'idris2-load-file
      "t" #'idris2-type-at-point
      "a" #'idris2-add-clause
      "e" #'idris2-make-lemma
      "c" #'idris2-case-dwim
      "w" #'idris2-make-with-block
      "m" #'idris2-add-missing
      "p" #'idris2-proof-search
      "h" #'idris2-docs-at-point
      "d" #'idris2-jump-to-def-same-window
      "i" '(:ignore t :which-key "ipkg")
      "i f" #'idris2-open-package-file
      "i b" #'idris2-ipkg-build
      "i c" #'idris2-ipkg-clean
      "i i" #'idris2-ipkg-install)


;; Fixes lag when editing idris code with evil
(defun ~/evil-motion-range--wrapper (fn &rest args)
  "Like `evil-motion-range', but override field-beginning for performance.
See URL `https://github.com/ProofGeneral/PG/issues/427'."
  (cl-letf (((symbol-function 'field-beginning)
             (lambda (&rest args) 1)))
    (apply fn args)))
(advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)

;; Move window to position
(defun ~/idris2-repl--wrapper ()
  "Moves the idris 2 repl window to the bottom of the frame."
  (save-selected-window
    (select-window (get-buffer-window idris2-repl-buffer-name))
    (evil-window-move-very-bottom)
    (enlarge-window (- 8 (window-body-height)))))
(advice-add #'idris2-repl-buffer :after #'~/idris2-repl--wrapper)
(advice-add #'idris2-pop-to-repl :after #'~/idris2-repl--wrapper)

(after! company
  (setq company-global-modes '(not idris2-mode idris2-repl-mode)))


(after! highlight-indent-guides
  (setq-default highlight-indent-guides-method 'character)
  (setq-default highlight-indent-guides-character 9615)
  (setq-default highlight-indent-guides-responsive 'top)

  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "#2d303f")
  (set-face-foreground 'highlight-indent-guides-top-character-face "#515880"))

;; Bind "SPC 0" to treemacs
;; Map window bindings to "SPC 1" through "SPC 9"
(map! :leader
  "w 0" #'treemacs-select-window
  "0" #'treemacs-select-window
  "1" #'winum-select-window-1
  "2" #'winum-select-window-2
  "3" #'winum-select-window-3
  "4" #'winum-select-window-4
  "5" #'winum-select-window-5
  "6" #'winum-select-window-6
  "7" #'winum-select-window-7
  "8" #'winum-select-window-8
  "9" #'winum-select-window-9)
