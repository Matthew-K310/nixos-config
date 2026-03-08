;;; jellyfin.el --- Jellyfin music browser -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Matthew Kennedy
;; Author: Matthew Kennedy <matthew@matthew-kennedy.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
;;
;;; Commentary:
;;
;; Stream your Jellyfin music library from Emacs
;;
;;; Code:

(require 'request)
(require 'json)

(defvar jellyfin-token nil
  "Jellyfin authentication token.")

(defvar jellyfin-url "http://jellyfin.matthewcloud.us"
  "Base URL of the Jellyfin server.")

(defvar jellyfin-music-id nil
  "Item ID of the Music library folder.")

(defun jellyfin-auth ()
  "Authenticate with Jellyfin and store the token."
  (let* ((creds (car (auth-source-search :host "jellyfin.matthewcloud.us"
                                         :require '(:user :secret))))
         (username (plist-get creds :user))
         (password (funcall (plist-get creds :secret))))
    (setq jellyfin-url "http://jellyfin.matthewcloud.us")
    (request (concat jellyfin-url "/Users/AuthenticateByName")
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(format "MediaBrowser Client=%S, Device=%S, DeviceId=%S, Version=%S"
                                             "jellyfin-emacs" "Emacs" "emacs-device-001" "1.0.0")))
      :data (json-encode `(("Username" . ,username) ("Pw" . ,password)))
      :parser 'json-read
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (setq jellyfin-token (alist-get 'AccessToken (request-response-data response)))
                   (message "Authenticated")
                   (jellyfin-get-libraries))))))

(defun jellyfin-get-libraries ()
  "Fetch library folders and store the Music library ID."
  (request (concat jellyfin-url "/Library/MediaFolders")
    :headers `(("Authorization" . ,(format "MediaBrowser Token=%S" jellyfin-token)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (let* ((items (alist-get 'Items (request-response-data response)))
                        (music (seq-find (lambda (item)
                                           (string= (alist-get 'Name item) "Music"))
                                         items)))
                   (setq jellyfin-music-id (alist-get 'Id music))
                   (message "Music library ID: %s" jellyfin-music-id))))))

(defun jellyfin-get-artists ()
  "Fetch and display music albums in a buffer."
  (request (concat jellyfin-url "/Artists")
    :params `(("ParentId" . ,jellyfin-music-id))
    :headers `(("Authorization" . ,(format "MediaBrowser Token=%S" jellyfin-token)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (let* ((items (alist-get 'Items (request-response-data response)))
                        (buf (get-buffer-create "*Jellyfin Artists*")))
                   (with-current-buffer buf
                     (erase-buffer)
                     (seq-do (lambda (item)
                               (insert (format "%s\n"
                                               (alist-get 'Name item))))
                             items)
                     (goto-char (point-min))))))))

(defun jellyfin-get-albums ()
  "Fetch and display music albums in a buffer."
  (request (concat jellyfin-url "/Items")
    :params `(("ParentId" . ,jellyfin-music-id)
              ("IncludeItemTypes" . "MusicAlbum")
              ("Recursive" . "true")
              ("Fields" . "Artists"))
    :headers `(("Authorization" . ,(format "MediaBrowser Token=%S" jellyfin-token)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (let* ((items (alist-get 'Items (request-response-data response)))
                        (buf (get-buffer-create "*Jellyfin Music*")))
                   (with-current-buffer buf
                     (erase-buffer)
                     (seq-do (lambda (item)
                               (insert (format "%s > %s\n"
                                               (alist-get 'Name item)
                                               (string-join (or (alist-get 'Artists item) []) ", "))))
                             items)
                     (goto-char (point-min))))))))

(provide 'jellyfin)
;;; jellyfin.el ends here
