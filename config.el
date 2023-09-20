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
(setq doom-font (font-spec :family "VictorMono" :size 13)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16)
      nerd-icons-scale-factor 1.1
      doom-modeline-height 24)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-variable-name-face :slant italic))

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

(setq disabled-command-function nil
      compile-command "nix build")

(use-package! treemacs
  :init
  (setq +treemacs-git-mode 'deferred
        treemacs-python-executable "/home/kiana/.emacs.d/python3-bin/bin/python")
  :config
  (setq treemacs-read-string-input 'from-minibuffer
        treemacs-select-when-already-in-treemacs 'stay))

(after! dired-mode
  (setq dired-kill-when-opening-new-dired-buffer t))

(after! eldoc
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose))

(after! calfw
  ;; Start week on Monday
  (setq calendar-week-start-day 1)
  (setq cfw:org-face-agenda-item-foreground-color (doom-color 'magenta)))


;; This seems to have broken on Doom Emacs's side for some reason?
(after! git-gutter-fringe
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(use-package! pinentry
  :init (setq epg-pinentry-mode 'loopback)
        (pinentry-start))


;; Restart pinentry due to inactivity
(defun pinentry-restart ()
  "Restart a Pinentry service."
  (interactive)
  (pinentry-stop) (pinentry-start))


(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(after! haskell-mode
  (custom-set-faces! '(haskell-operator-face :slant normal)))

;; Idris 2
(after! company
  (setq company-global-modes '(not idris2-mode idris2-repl-mode)))


(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character 9615
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 50
        highlight-indent-guides-auto-top-character-face-perc 90))

;; Flymake
(add-hook! prog-mode #'flymake-mode)
(setq-hook! flymake-mode next-error-function #'flymake-goto-next-error)
(after! lsp-mode
  (require 'avy) ; Needed for code lens selection
  (setq lsp-diagnostics-provider :flymake))

;; Use Flycheck's double arrow fringe indicator
(after! flymake
  (define-fringe-bitmap 'flymake-double-arrow [216 108 54 27 54 108 216])
  (setf (car flymake-error-bitmap) 'flymake-double-arrow
        (car flymake-warning-bitmap) 'flymake-double-arrow
        (car flymake-note-bitmap) 'flymake-double-arrow))

;; Configuration for flymake-popon
(add-hook! flymake-mode #'flymake-popon-mode)
(after! flymake-popon
  (setq flymake-popon-width 120)
  (set-face-foreground 'flymake-popon-posframe-border (doom-color 'selection)))
(custom-set-faces! '(compilation-warning :slant normal :weight bold)
                   '(flymake-note-echo :underline nil :inherit compilation-info))

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

(defun create-new-project (dir &optional type parents)
  "Create a new directory DIR and add it to the list of known projects.

TYPE specifies the type of project to create. It can take the following values:
- git creates a new Git repository.
- projectile creates a .projectile file in the project root.
- A string, which is used as a filename to create in the project root.
- A function, which is called with no arguments inside the root of the project.
If nil, then a Git repository is created by default.

If PARENTS is non-nil, the parents of the specified directory will also be created."
  (interactive (list (read-directory-name "Create new project: ") nil t))
  (make-directory dir parents)
  (let ((default-directory dir))
    (pcase type
      ((or 'git 'nil)
       (shell-command "git init"))
      ('projectile
       (make-empty-file ".projectile"))
      ((pred stringp)
       (make-empty-file type))
      ((pred functionp)
       (funcall type))))
  (projectile-add-known-project dir))

(defadvice! ~/projectile-find-file (invalidate-cache &optional ff-variant)
    :override #'projectile--find-file
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (projectile-acquire-root))
         (file (read-file-name "Find file: " project-root project-root
                               (confirm-nonexistent-file-or-buffer) nil
                               ))
         (ff (or ff-variant #'find-file)))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))


(after! calc
  (setq calc-highlight-selections-with-faces t
        calc-show-selections nil
        calc-window-height 13)
  (custom-set-faces!
    `(calc-selected-face :weight extra-bold :foreground ,(doom-color 'highlight))
    `(calc-nonselected-face :weight semi-light :foreground ,(doom-color 'comments))))


;; Declare popup rules
(set-popup-rule! "^\\*Flymake diagnostics for .+\\*$" :side 'bottom :size 0.25)
(set-popup-rule! "^\\*Agda information\\*$" :side 'bottom :size 0.2)


;; Load extra files
(load! "+bindings")
(load! "+org")
