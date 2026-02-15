;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(package! exwm)

;; go dev
(package! templ-ts-mode)
(package! go-mode)

;; denote
(package! denote)
(package! denote-refs)
(package! denote-org)
(package! denote-menu)
(package! denote-search)
(package! denote-explore)
(package! denote-journal)

;; org
(package! ox-hugo)
(package! org-noter)
(package! org-noter-pdftools)
(package! org-caldav)
(package! org-habit-stats)
(package! org-web-tools)

;; utils
(package! nov)
(package! djvu)
(package! company-emoji)
(package! tmr)
;; accounting
(package! evil-ledger)
(package! ledger-mode)
(package! inhibit-mouse)
(package! elfeed-tube)
(package! elfeed-tube-mpv)
(package! focus)
(package! forge)

;; programs
(package! bluetooth)
(package! elfeed)
(package! dirvish)
(package! pipewire)
(package! harpoon)
(package! syncthing)
(package! emacs-w3m)
(package! jabber)
;; 
;; Dependencies for tui.el
(package! dash)
(package! s)
