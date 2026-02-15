;;; ../../.dotfiles/.config/doom/lisp/exwm.el -*- lexical-binding: t; -*-

;; Define helper functions before use-package to ensure they exist
(defun my/exwm-toggle-floating ()
  "Toggle floating mode for current window."
  (interactive)
  (exwm-floating-toggle-floating))

(defun my/exwm-fullscreen ()
  "Toggle fullscreen for current window."
  (interactive)
  (exwm-layout-toggle-fullscreen))

(exwm-input-set-key (kbd "s-d") #'counsel-linux-app)

(exwm-input-set-key (kbd "C-s-<return>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "kitty" nil "kitty")))
(exwm-input-set-key (kbd "s-w")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "browser" nil "helium-browser")))
(exwm-input-set-key (kbd "C-s-m")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "kitty" nil "kitty profanity")))
(exwm-input-set-key (kbd "C-s-S-m")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "kitty" nil "kitty jellyfin-tui")))

;; brightness keys
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "brightnessctl" nil "brightnessctl set +5%")))
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "brightnessctl" nil "brightnessctl set 5%-")))

;; volume keys
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "pactl " nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")))

(exwm-input-set-key (kbd "<XF86AudioPlay>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "playerctl" nil "playerctl play-pause")))
(exwm-input-set-key (kbd "<XF86AudioNext>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "playerctl" nil "playerctl next")))
(exwm-input-set-key (kbd "<XF86AudioPrev>")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "playerctl" nil "playerctl previous")))

;; media keys

(use-package exwm
  :config
  ;; Set the initial number of workspaces
  (setq exwm-workspace-number 6)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Global keybindings
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k to char-mode)
          ([?\s-r] . exwm-reset)
          ;; Switch workspace
          ([?\s-W] . exwm-workspace-switch)
          ;; Launch application
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))))

  ;; Line-mode keybindings
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  ;; Additional keybindings
  (exwm-input-set-key (kbd "s-f") #'my/exwm-fullscreen)
  (exwm-input-set-key (kbd "s-q") #'kill-current-buffer)
  (exwm-input-set-key (kbd "s-e") #'my/new-frame-with-vterm)
  (exwm-input-set-key (kbd "s-<return>") #'my-open-vterm-at-point)
  (exwm-input-set-key (kbd "s-r") #'dirvish)
  (exwm-input-set-key (kbd "C-s-r") #'dired)
  (exwm-input-set-key (kbd "s-c") #'org-capture)
  (exwm-input-set-key (kbd "s-m") #'mu4e)
  (exwm-input-set-key (kbd "s-S-m") #'emms-browser)
  (exwm-input-set-key (kbd "s-n") #'org-agenda)
  (exwm-input-set-key (kbd "s-S-d") #'pass)
  (exwm-input-set-key (kbd "s-S-c") #'calendar)
  (exwm-input-set-key (kbd "s-<XF86AudioPlay>") #'bluetooth-list-devices)

  ;; Enable EXWM
  (exwm-wm-mode))

;; focus on workspace switch
(defun my/exwm-focus-on-switch ()
  "Focus the current buffer after workspace switch."
  (when (derived-mode-p 'exwm-mode)
    (exwm-input--set-focus (exwm--buffer->id (window-buffer)))))

(add-hook 'exwm-workspace-switch-hook #'my/exwm-focus-on-switch)

(provide 'exwm-setup)
