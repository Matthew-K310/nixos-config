;;; ../../nixos-config/dotfiles/doom/lisp/emms-config.el -*- lexical-binding: t; -*-

;; EMMS full configuration with Nord theme, centered layout, and swaync notifications
(use-package! emms
  :defer t
  :commands (emms 
             emms-browser 
             emms-playlist-mode-go
             emms-pause
             emms-stop
             emms-next
             emms-previous
             emms-shuffle)
  :init
  ;; Set these early so they're available when EMMS loads
  (setq emms-source-file-default-directory "~/cloud/music"
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t
        emms-browser-default-browse-type 'artist)
  
  :config
  ;; Initialize EMMS - only runs when you actually use it
  (emms-all)
  (emms-default-players)
  (emms-mode-line-mode 1)
  (emms-playing-time-mode 1)

  ;; Basic settings
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

  ;; MPD integration - critical for your workflow
  (require 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory (expand-file-name "~/cloud/music"))

  ;; Connect to MPD and add it to player list
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  
  ;; Connect to MPD with slight delay to avoid blocking
  (run-with-timer 0.1 nil #'emms-player-mpd-connect)

  ;; Ensure players are properly set up
  (setq emms-player-list '(emms-player-mpd
                           emms-player-mplayer
                           emms-player-vlc
                           emms-player-mpg321
                           emms-player-ogg123))

  ;; Info functions
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (add-to-list 'emms-info-functions 'emms-info-tinytag)

  ;; Nord theme colors
  (custom-set-faces
   ;; Nord
   ;; '(emms-browser-artist-face ((t (:foreground "#ECEFF4" :height 1.1))))
   ;; '(emms-browser-album-face ((t (:foreground "#88C0D0" :height 1.0))))
   ;; '(emms-browser-track-face ((t (:foreground "#A3BE8C" :height 1.0))))
   ;; '(emms-playlist-track-face ((t (:foreground "#D8DEE9" :height 1.0))))
   ;; '(emms-playlist-selected-face ((t (:foreground "#BF616A" :weight bold)))))
   
   ;; Nowhere
   '(emms-browser-artist-face ((t (:foreground "#e0dcd4" :height 1.1))))   ; Parchment - most prominent
   '(emms-browser-album-face ((t (:foreground "#b4bec8" :height 1.0))))    ; Steel-blue - secondary accent
   '(emms-browser-track-face ((t (:foreground "#b4beb4" :height 1.0))))    ; Sage-green - individual tracks
   '(emms-playlist-track-face ((t (:foreground "#c0bdb8" :height 1.0))))   ; Muted foreground - neutral
   '(emms-playlist-selected-face ((t (:foreground "#ccc4b0" :weight bold))))) ; Wheat-gold - warm selection

  ;; Browser keybindings
  (define-key emms-browser-mode-map (kbd "RET") 'emms-browser-add-tracks-and-play)
  ;; (define-key emms-browser-mode-map (kbd "SPC") 'emms-pause)

  ;; Add notification hook
  (add-hook 'emms-player-started-hook 'emms-notify-song-change-with-artwork))

;; Helper functions - defined outside use-package so they're always available
(defun my/update-emms-from-mpd ()
  "Update EMMS cache from MPD and refresh browser."
  (interactive)
  (require 'emms)  ; Ensure EMMS is loaded
  (message "Updating EMMS cache from MPD...")
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "EMMS cache updated. Refreshing browser...")
  (when (get-buffer "*EMMS Browser*")
    (with-current-buffer "*EMMS Browser*"
      (emms-browser-refresh))))

(defun emms-center-buffer-in-frame ()
  "Add margins to center the EMMS buffer in the frame."
  (let* ((window-width (window-width))
         (desired-width 80)
         (margin (max 0 (/ (- window-width desired-width) 2))))
    (setq-local left-margin-width margin)
    (setq-local right-margin-width margin)
    (setq-local line-spacing 0.2)
    (set-window-buffer (selected-window) (current-buffer))))

(defun emms-cover-art-path ()
  "Return the path of the cover art for the current track."
  (when (bound-and-true-p emms-playlist-buffer)
    (let* ((track (emms-playlist-current-selected-track))
           (path (emms-track-get track 'name))
           (dir (file-name-directory path))
           (standard-files '("cover.jpg" "cover.png" "folder.jpg" "folder.png"
                             "album.jpg" "album.png" "front.jpg" "front.png"))
           (standard-cover (cl-find-if
                            (lambda (file)
                              (file-exists-p (expand-file-name file dir)))
                            standard-files)))
      (if standard-cover
          (expand-file-name standard-cover dir)
        (let ((cover-files (directory-files dir nil ".*\\(jpg\\|png\\|jpeg\\)$")))
          (when cover-files
            (expand-file-name (car cover-files) dir)))))))

(defun emms-notify-song-change-with-artwork ()
  "Send song change notification with album artwork to swaync via libnotify."
  (when (bound-and-true-p emms-playlist-buffer)
    (let* ((track (emms-playlist-current-selected-track))
           (artist (or (emms-track-get track 'info-artist) "Unknown Artist"))
           (title (or (emms-track-get track 'info-title) "Unknown Title"))
           (album (or (emms-track-get track 'info-album) "Unknown Album"))
           (cover-image (emms-cover-art-path)))
      
      (apply #'start-process
             "emms-notify" nil "notify-send"
             "-a" "EMMS"
             "-c" "music"
             (append
              (when cover-image
                (list "-i" cover-image))
              (list
               (format "Now Playing: %s" title)
               (format "Artist: %s\nAlbum: %s" artist album)))))))

(defun emms-signal-waybar-mpd-update ()
  "Signal waybar to update its MPD widget."
  (start-process "emms-signal-waybar" nil "pkill" "-RTMIN+8" "waybar"))

;; Hooks for EMMS modes - use with-eval-after-load to avoid premature loading
(with-eval-after-load 'emms-browser
  (add-hook 'emms-browser-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:background "#1a1d21"))
              (emms-center-buffer-in-frame))))

(with-eval-after-load 'emms-playlist-mode
  (add-hook 'emms-playlist-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:background "#1a1d21"))
              (emms-center-buffer-in-frame))))

;; Window resize hook - only add when EMMS is actually loaded
(with-eval-after-load 'emms
  (add-hook 'window-size-change-functions
            (lambda (_)
              (when (or (eq major-mode 'emms-browser-mode)
                        (eq major-mode 'emms-playlist-mode))
                (emms-center-buffer-in-frame)))))

