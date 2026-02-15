;;; ../../nixos-config/dotfiles/doom/lisp/jitsi-meeting.el -*- lexical-binding: t; -*-

;; Define jitsi base URL
;; (defvar jitsi-base-url "https://meet.jit.si/"
(defvar jitsi-base-url "https://meet.matthewservers.org/"
  "Base URL for Jitsi meetings.")

(defun my/jitsi-find-timestamp ()
  "Find the first timestamp in current org entry."
  (save-excursion
    (org-back-to-heading)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (when (re-search-forward org-ts-regexp end t)
        (org-parse-time-string (match-string 0))))))

(defun my/jitsi-generate-room-name ()
  "Generate room name using entry timestamp if available, current time otherwise."
  (let* ((timestamp-parts (when (derived-mode-p 'org-mode)
                            (my/jitsi-find-timestamp)))
         (use-time (if timestamp-parts
                       (encode-time timestamp-parts)
                     (current-time)))
         (date-part (format-time-string "%Y%m%d-%H%M" use-time))
         (random-part (format "%04x" (random 65536))))
    (format "meeting-%s-%s" date-part random-part)))

(defun my/jitsi-create-room ()
  "Prompt for a Jitsi meeting room name, with auto-generation fallback, then insert after properties drawer."
  (interactive)
  (let* ((input (read-string "Room name (leave empty to auto-generate): "))
         (room-name (if (string-empty-p input)
                        (my/jitsi-generate-room-name)
                      input))
         (full-url (concat jitsi-base-url room-name)))

    (when (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading)
        ;; Look for :END: line (end of properties drawer)
        (if (re-search-forward "^[ \t]*:END:[ \t]*$"
                               (save-excursion (outline-next-heading) (point)) t)
            (progn
              (end-of-line)
              (insert "\n" full-url))
          ;; No properties drawer, insert after heading and timestamp/scheduling
          (progn
            (forward-line 1)
            (while (looking-at "^[ \t]*[<[]")
              (forward-line 1))
            (insert full-url "\n")))))

    (kill-new full-url)
    (message "Jitsi room created and copied: %s" full-url)
    full-url))

;; Keybinding
(map! :leader
      (:prefix ("j" . "jitsi")
       :desc "Create jitsi room" "c" #'my/jitsi-create-room))
