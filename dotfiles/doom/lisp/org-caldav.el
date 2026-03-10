;;; ../../nixos-config/dotfiles/doom/lisp/org-caldav.el -*- lexical-binding: t; -*-

(use-package! org-caldav
  :config
  (setq org-caldav-url "https://radicale.matthewcloud.us/matthewkennedy"
        org-caldav-calendar-id "83ba2b41-55fe-d2f3-7701-c5411d5de66b"
        org-caldav-inbox "~/org/caldav-inbox.org"
        org-caldav-files '("~/org/calendar.org")
        org-caldav-sync-direction 'twoway
        org-caldav-delete-org-entries 'ask
        org-caldav-delete-calendar-entries 'ask
        org-icalendar-timezone "America/Chicago"
        ;; CRITICAL: Allow broken links during icalendar export
        org-icalendar-include-todo nil
        org-export-with-broken-links t)  ; Global override

  ;; Force org-export to ignore broken links
  (advice-add 'org-export-data :around
              (lambda (orig-fun &rest args)
                (let ((org-export-with-broken-links t))
                  (apply orig-fun args)))))

;; Also set globally for all org exports
(after! org
  (setq org-export-with-broken-links t))

