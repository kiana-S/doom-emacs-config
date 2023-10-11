;; -*- no-byte-compile: t; -*-
;;; ui/ophints/config.el

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
              :advice evil-goggles--generic-async-advice)
            '(evil-fill
              :face evil-goggles-fill-and-move-face
              :switch evil-goggles-enable-fill-and-move
              :advice evil-goggles--generic-async-advice)
            '(evil-fill-and-move
              :face evil-goggles-fill-and-move-face
              :switch evil-goggles-enable-fill-and-move
              :advice evil-goggles--generic-async-advice))
  (custom-set-faces! '(evil-goggles-default-face :background "#2b3a7f")
                     '(evil-goggles-delete-face :inherit magit-diff-removed-highlight)
                     '(evil-goggles-paste-face :inherit magit-diff-added-highlight)
                     '(evil-goggles-change-face :inherit evil-goggles-delete-face)))
