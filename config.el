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

(setq-default tab-width 2
              evil-shift-width 2
              evil-auto-indent nil)

;; Enable all commands
(setq disabled-command-function nil)

(after! treemacs
  (setq treemacs-read-string-input 'from-minibuffer))

(after! dired-mode
  (setq dired-kill-when-opening-new-dired-buffer t))

(after! eldoc
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose))

;; This seems to have broken on Doom Emacs's side for some reason?
(after! git-gutter-fringe
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(use-package! pinentry
  :init (setq epg-pinentry-mode 'loopback)
        (pinentry-start))

(use-package! evil-goggles
  :hook (doom-first-input . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.15
        evil-goggles-blocking-duration 0.12
        evil-goggles-async-duration 0.2)
  :config
  (pushnew! evil-goggles--commands
            '(evil-magit-yank-whole-line
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(+evil:yank-unindented
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(+eval:region
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice))
  (custom-set-faces! '(evil-goggles-default-face :background "#2b3a7f")
                     '(evil-goggles-delete-face :inherit magit-diff-removed-highlight)
                     '(evil-goggles-paste-face :inherit magit-diff-added-highlight)
                     '(evil-goggles-change-face :inherit evil-goggles-delete-face)))

;; Restart pinentry due to inactivity
(defun pinentry-restart ()
  "Restart a Pinentry service."
  (interactive)
  (pinentry-stop) (pinentry-start))


(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

;; Idris 2
(after! idris2-mode
  (custom-set-faces! '(idris2-operator-face :slant normal :inherit font-lock-variable-name-face))
  (set-face-foreground 'idris2-semantic-data-face (doom-color 'red))
  (set-face-foreground 'idris2-semantic-function-face (doom-color 'dark-green))
  (set-face-foreground 'idris2-semantic-bound-face (doom-color 'magenta))
  (set-face-foreground 'idris2-semantic-type-face (doom-color 'blue))
  (add-hook 'idris2-mode-hook #'turn-on-idris2-simple-indent)
  (set-repl-handler! 'idris2-mode 'idris2-pop-to-repl)
  (set-lookup-handlers! 'idris2-mode
    :definition #'+idris2/jump-to-definition
    :documentation #'+idris2/jump-to-definition))

(map! :localleader
      :after idris2-mode
      :map idris2-mode-map
      "q" #'idris2-quit
      "l" #'idris2-load-file
      "t" #'idris2-type-at-point
      "a" #'idris2-add-clause
      "e" #'idris2-make-lemma
      "c" #'idris2-case-dwim
      "w" #'idris2-make-with-block
      "m" #'idris2-add-missing
      "p" #'idris2-proof-search
      "h" #'idris2-docs-at-point
      "d" #'+idris2/jump-to-definition
      (:prefix ("i" . "ipkg")
               "f" #'idris2-open-package-file
               "b" #'idris2-ipkg-build
               "c" #'idris2-ipkg-clean
               "i" #'idris2-ipkg-install))


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

(defun +idris2/type-at-point ()
  "Return the type of the name at point."
  (interactive)
  (let ((name (car (idris2-thing-at-point t))))
    (car (idris2-eval (list :type-of name)))))



;; Fixes lag when editing idris code with evil
(defun ~/evil-motion-range--wrapper (fn &rest args)
  "Like 'evil-motion-range', but override 'field-beginning' for performance.
See URL 'https://github.com/ProofGeneral/PG/issues/427'."
  (cl-letf (((symbol-function 'field-beginning)
             (lambda (&rest _) 1)))
    (apply fn args)))
(advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)

(after! company
  (setq company-global-modes '(not idris2-mode idris2-repl-mode)))


(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character 9615
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 15
        highlight-indent-guides-auto-top-character-face-perc 45))

;; Flymake
(add-hook! prog-mode #'flymake-mode)
(setq-hook! flymake-mode next-error-function #'flymake-goto-next-error)
(after! lsp-mode
  (setq lsp-diagnostics-provider :flymake))

;; Use Flycheck's double arrow fringe indicator
(after! flymake
  (define-fringe-bitmap 'flymake-double-arrow [216 108 54 27 54 108 216])
  (setf (car flymake-error-bitmap) 'flymake-double-arrow
        (car flymake-warning-bitmap) 'flymake-double-arrow
        (car flymake-note-bitmap) 'flymake-double-arrow))

;; Dired
(defun +dired/up-directory-alternative ()
  "Use single instance of Dired buffer when going up a directory."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((up (file-name-directory (directory-file-name (dired-current-directory)))))
    (or (dired-goto-subdir up) (find-alternate-file up))))

(defun +dired/find-alt-file-for-directories ()
  "Use single instance of Dired buffer when opening files."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (progn
          (set-buffer-modified-p nil)
          (find-alternate-file file))
      (find-file file))))

(map! :mode dired-mode :after dired
      [remap dired-find-file]    #'+dired/find-alt-file-for-directories
      [remap dired-up-directory] #'+dired/up-directory-alternative)

(defun create-new-project (dir &optional parents)
  "Create a new git directory and add it to the list of known projects."
  (interactive (list (read-directory-name "Create new project: ") t))
  (make-directory dir parents)
  (let ((default-directory dir))
    (shell-command "git init"))
  (projectile-add-known-project dir))


(setq calc-highlight-selections-with-faces t)
(custom-set-faces!
  `(calc-selected-face :weight extra-bold :foreground ,(doom-color 'highlight))
  `(calc-nonselected-face :weight semi-light :foreground ,(doom-color 'comments)))


;; Declare popup rules

(set-popup-rules!
  '(("^\\*Flymake diagnostics for .+\\*$" :side bottom :size 0.25))
  '(("^\\*idris2-repl\\*$" :side bottom :size 0.16)
    ("^\\*idris2-info\\*$" :side bottom :size +popup-shrink-to-fit :vslot 1)
    ("^\\*idris2-notes\\*$" :side right :size 80 :slot 1)
    ("^\\*idris2-holes\\*$" :side right :size 80 :slot 2)))


;; Load extra files
(load! "bindings.el")
(load! "org.el")
