;;; pomodoro.el --- A simple pomodoro timer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author: Joshua Blais
;; Maintainer: Joshua Blais
;; Created: February 04, 2025
;; Modified: February 24, 2025
;; Version: 0.0.2
;; Keywords: tools
;; Homepage: https://github.com/jblais493/pomodoro
;; Package-Requires: ((emacs "29.1"))
;;
;;; Commentary:
;;
;; A simple pomodoro timer implementation with countdown display, org logging,
;; work.org timeblock integration, and preset session support.
;;
;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-datetree)

;; ============================================================
;; Customization Group
;; ============================================================

(defgroup pomodoro nil
  "Simple Pomodoro timer for Emacs."
  :group 'tools
  :prefix "pomodoro-")

(defcustom pomodoro-work-minutes 25
  "Work period length in minutes."
  :type 'integer
  :group 'pomodoro)

(defcustom pomodoro-break-minutes 5
  "Break period length in minutes."
  :type 'integer
  :group 'pomodoro)

(defcustom pomodoro-presets
  '(("Time to Work (25/5)"     :work 25 :break 5  :task nil)
    ("Lock In (50/10)"    :work 50 :break 10 :task "Deep Work")
    ("Quickie (15/5)"  :work 15 :break 5  :task nil)
    ("Ultra Focus (90/15)" :work 90 :break 15 :task "Long Session"))
  "Preset pomodoro configurations.
Each entry is (NAME :work MINUTES :break MINUTES :task TASK-OR-NIL).
If :task is nil, you will be prompted for a task name on start."
  :type '(repeat (list string plist))
  :group 'pomodoro)

(defcustom pomodoro-done-file "~/org/done.org"
  "Path to the file where completed sessions are logged."
  :type 'file
  :group 'pomodoro)

(defcustom pomodoro-work-file "~/org/work.org"
  "Path to the work.org file for timeblock entries."
  :type 'file
  :group 'pomodoro)

(defcustom pomodoro-alert-sound "~/cloud/downloads/Bell.mp3"
  "Path to sound file for pomodoro alerts."
  :type 'file
  :group 'pomodoro)

;; ============================================================
;; State Variables
;; ============================================================

(defvar pomodoro-task ""
  "Current task being worked on.")

(defvar pomodoro-timer nil
  "Timer object for the main pomodoro countdown.")

(defvar pomodoro-mode-line ""
  "String to display in mode line.")

(defvar pomodoro-end-time nil
  "When the current period ends.")

(defvar pomodoro-update-timer nil
  "Timer object for updating the display.")

(defvar pomodoro-start-time nil
  "When the current work period started.")

(defvar pomodoro-break-p nil
  "Flag to track if we're in a break period.")

;; ============================================================
;; Mode Line
;; ============================================================

(unless (member 'pomodoro-mode-line global-mode-string)
  (setq global-mode-string (append global-mode-string '(pomodoro-mode-line))))

;; ============================================================
;; Alert / Sound
;; ============================================================

(defun pomodoro-play-alert (message)
  "Send notification MESSAGE and play alert sound."
  (when (fboundp 'notifications-notify)
    (notifications-notify
     :title "Pomodoro Timer"
     :body message
     :urgency 'critical))
  (let ((sound-file (expand-file-name pomodoro-alert-sound)))
    (cond
     ((executable-find "pw-play")
      (start-process "pomodoro-sound" nil "pw-play" sound-file))
     ((executable-find "mpv")
      (start-process "pomodoro-sound" nil "mpv" "--no-video" "--volume=100" sound-file))
     ((executable-find "ffplay")
      (start-process "pomodoro-sound" nil "ffplay" "-nodisp" "-autoexit" "-volume" "100" sound-file))
     (t
      (message "No audio player found for pomodoro alert")))))

;; ============================================================
;; done.org Logging
;; ============================================================

(defun pomodoro-ensure-done-file ()
  "Ensure the done.org file exists and has today's date heading."
  (let ((done-file (expand-file-name pomodoro-done-file))
        (today (format-time-string "* %Y-%m-%d %A")))
    (unless (file-exists-p done-file)
      (append-to-file "" nil done-file))
    (with-temp-buffer
      (when (file-exists-p done-file)
        (insert-file-contents done-file))
      (goto-char (point-min))
      (unless (search-forward today nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert today "\n")
        (write-region (point-min) (point-max) done-file nil 'quiet)))))

(defun pomodoro-log-session (task completed)
  "Log the completed pomodoro session for TASK with COMPLETED notes to done.org."
  (let* ((done-file (expand-file-name pomodoro-done-file))
         (today (format-time-string "* %Y-%m-%d %A"))
         (start-time (format-time-string "%H:%M" pomodoro-start-time))
         (end-time (format-time-string "%H:%M" (current-time)))
         (entry (format "** %s-%s: %s\n   %s\n" start-time end-time task completed)))
    (with-temp-buffer
      (when (file-exists-p done-file)
        (insert-file-contents done-file))
      (goto-char (point-min))
      (if (search-forward today nil t)
          (forward-line 1)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert today "\n"))
      (insert entry)
      (write-region (point-min) (point-max) done-file nil 'quiet))))

;; ============================================================
;; work.org Timeblock Logging
;; ============================================================

(defun pomodoro-log-to-work-org ()
  "Add current task to work.org with timeblock entry and clock in."
  (let ((work-file (expand-file-name pomodoro-work-file))
        (scheduled-time (format-time-string "[%Y-%m-%d %a %H:%M]" pomodoro-start-time)))
    (with-current-buffer (find-file-noselect work-file)
      (org-datetree-find-date-create
       (calendar-gregorian-from-absolute
        (time-to-days pomodoro-start-time)))
      (let ((date-point (point)))
        (unless (re-search-forward "^\\*\\* Time Blocks" nil t)
          (goto-char date-point)
          (end-of-line)
          (insert "\n** Time Blocks"))
        (end-of-line)
        (insert (format "\n*** Work on %s\nSCHEDULED: %s"
                        pomodoro-task scheduled-time))
        (org-clock-in)
        (save-buffer)))))

;; ============================================================
;; Display
;; ============================================================

(defun pomodoro-update-display ()
  "Update the mode line display."
  (when pomodoro-end-time
    (let* ((remaining-seconds (round (float-time (time-subtract pomodoro-end-time (current-time)))))
           (remaining-minutes (/ remaining-seconds 60))
           (remaining-secs (mod remaining-seconds 60)))
      (if (> remaining-seconds 0)
          (setq pomodoro-mode-line
                (format " [%s %02d:%02d %s] "
                        (if pomodoro-break-p "☕" "🍅")
                        remaining-minutes
                        remaining-secs
                        (if pomodoro-break-p "Break" pomodoro-task)))
        (setq pomodoro-mode-line "")))
    (force-mode-line-update)))

;; ============================================================
;; Core Timer Functions
;; ============================================================

(defun pomodoro-work-period ()
  "Start a work period."
  (setq pomodoro-break-p nil)
  (message "Starting %d minute work period on: %s" pomodoro-work-minutes pomodoro-task)
  (setq pomodoro-start-time (current-time))
  (setq pomodoro-end-time (time-add (current-time) (seconds-to-time (* pomodoro-work-minutes 60))))
  (when pomodoro-update-timer (cancel-timer pomodoro-update-timer))
  (setq pomodoro-update-timer (run-at-time nil 1 #'pomodoro-update-display))
  (when pomodoro-timer (cancel-timer pomodoro-timer))
  (setq pomodoro-timer (run-at-time (* pomodoro-work-minutes 60) nil #'pomodoro-work-done))
  (pomodoro-log-to-work-org))

(defun pomodoro-work-done ()
  "Handle work period completion."
  (when (org-clocking-p) (org-clock-out nil t))
  (pomodoro-play-alert "Work period complete!")
  (let ((completed (read-string "What did you accomplish? ")))
    (pomodoro-log-session pomodoro-task completed)
    (message "Work period complete! Accomplished: %s" completed)
    (pomodoro-break-period)))

(defun pomodoro-break-period ()
  "Start a break period."
  (setq pomodoro-break-p t)
  (message "Starting %d minute break" pomodoro-break-minutes)
  (setq pomodoro-end-time (time-add (current-time) (seconds-to-time (* pomodoro-break-minutes 60))))
  (when pomodoro-timer (cancel-timer pomodoro-timer))
  (setq pomodoro-timer (run-at-time (* pomodoro-break-minutes 60) nil #'pomodoro-break-done)))

(defun pomodoro-break-done ()
  "Handle break period completion."
  (pomodoro-play-alert "Break complete! Start new session?")
  (pomodoro-start))

;; ============================================================
;; Interactive Commands
;; ============================================================

(defun pomodoro-start ()
  "Start a new Pomodoro session."
  (interactive)
  (when pomodoro-update-timer (cancel-timer pomodoro-update-timer))
  (when pomodoro-timer (cancel-timer pomodoro-timer))
  (pomodoro-ensure-done-file)
  (setq pomodoro-task (read-string "What are you working on? "))
  (pomodoro-work-period))

(defun pomodoro-start-preset ()
  "Start a pomodoro session using a preset configuration."
  (interactive)
  (when pomodoro-update-timer (cancel-timer pomodoro-update-timer))
  (when pomodoro-timer (cancel-timer pomodoro-timer))
  (pomodoro-ensure-done-file)
  (let* ((names (mapcar #'car pomodoro-presets))
         (choice (completing-read "Select preset: " names nil t))
         (preset (cdr (assoc choice pomodoro-presets)))
         (work   (plist-get preset :work))
         (break  (plist-get preset :break))
         (task   (plist-get preset :task)))
    (setq pomodoro-work-minutes work
          pomodoro-break-minutes break
          pomodoro-task (or task (read-string "What are you working on? ")))
    (message "Preset: %s (%d/%d min)" choice work break)
    (pomodoro-work-period)))

(defun pomodoro-kill ()
  "Kill the current pomodoro session."
  (interactive)
  (when (org-clocking-p) (org-clock-out nil t))
  (when pomodoro-update-timer
    (cancel-timer pomodoro-update-timer)
    (setq pomodoro-update-timer nil))
  (when pomodoro-timer
    (cancel-timer pomodoro-timer)
    (setq pomodoro-timer nil))
  (setq pomodoro-mode-line "")
  (setq pomodoro-end-time nil)
  (force-mode-line-update)
  (message "Pomodoro timer stopped."))

(defun pomodoro-toggle ()
  "Start or kill the pomodoro timer based on current state."
  (interactive)
  (if (or pomodoro-timer pomodoro-update-timer)
      (pomodoro-kill)
    (pomodoro-start)))

(defun pomodoro-toggle-preset ()
  "Start with a preset or kill the pomodoro timer based on current state."
  (interactive)
  (if (or pomodoro-timer pomodoro-update-timer)
      (pomodoro-kill)
    (pomodoro-start-preset)))

;; Keybindings

(global-set-key (kbd "C-c t s") 'pomodoro-toggle)
(global-set-key (kbd "C-c t p") 'pomodoro-toggle-preset)

(provide 'pomodoro)
;;; pomodoro.el ends here
