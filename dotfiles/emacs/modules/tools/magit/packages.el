;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; HACK: Fixes magit/magit#5462. Remove when addressed upstream.
(defvar magit-auto-revert-mode nil)

(package! transient) 
(package! magit)
(when (modulep! +forge)
  (package! forge
    ;; forge depends on ghub, which requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! ghub
    ;; ghub requires Emacs 29.1+
    :disable (version< emacs-version "29.1"))
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))
