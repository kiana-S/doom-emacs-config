;;; lang/idris2/config.el -*- lexical-binding: t; -*-


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
    :documentation #'idris2-docs-at-point))
  (map! :localleader
        :mode idris2-mode
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


;; Fixes lag when editing idris code with evil
(defun +idris2/evil-motion-range--wrapper (fn &rest args)
  "Like 'evil-motion-range', but override 'field-beginning' for performance.
See URL 'https://github.com/ProofGeneral/PG/issues/427'."
  (cl-letf (((symbol-function 'field-beginning)
             (lambda (&rest _) 1)))
    (apply fn args)))

(advice-add #'evil-motion-range :around #'+idris2/evil-motion-range--wrapper)
