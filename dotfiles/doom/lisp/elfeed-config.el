;;; ../../nixos-config/dotfiles/doom/lisp/elfeed-config.el -*- lexical-binding: t; -*-

(use-package! elfeed
  :commands (elfeed)
  :config
  ;; Where to store the Elfeed database
  (setq elfeed-db-directory (expand-file-name "~/.local/share/elfeed"))
  (make-directory elfeed-db-directory t)

  ;; Update every hour automatically
  (run-at-time nil (* 60 60) #'elfeed-update)

  (use-package! elfeed-org
    :after elfeed
    :config
    (setq rmh-elfeed-org-files
          (list (expand-file-name "~/.config/doom/feeds.org")))
    (elfeed-org)))

(defvar elfeed-download-base-dir (expand-file-name "~/cloud/downloads/elfeed/")
  "Base directory for all Elfeed downloads.")

(defvar elfeed-download-youtube-dir "youtube/"
  "Subdirectory for YouTube videos (relative to base dir).")

(defvar elfeed-download-articles-dir "articles/"
  "Subdirectory for saved articles (relative to base dir).")

(defun elfeed-download--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun elfeed-download--sanitize-filename (filename)
  "Make FILENAME safe for use on the filesystem."
  (replace-regexp-in-string "[^a-zA-Z0-9-_. ]" "_" filename))

(defun elfeed-download--youtube-dir ()
  (expand-file-name elfeed-download-youtube-dir elfeed-download-base-dir))

(defun elfeed-download--articles-dir ()
  (expand-file-name elfeed-download-articles-dir elfeed-download-base-dir))

(defun elfeed-download-youtube (url title)
  "Download a YouTube video at URL with TITLE using yt-dlp."
  (let ((dir (elfeed-download--youtube-dir)))
    (elfeed-download--ensure-directory dir)
    (start-process "yt-dlp" "*yt-dlp*"
                   "yt-dlp"
                   "-o" (concat dir "%(title)s.%(ext)s")
                   url)))

(defun elfeed-download-article (url title)
  "Save article at URL with TITLE as a PDF using Node/Playwright."
  (let* ((dir (elfeed-download--articles-dir))
         (safe-title (elfeed-download--sanitize-filename title))
         (pdf-file (concat dir safe-title ".pdf")))
    (elfeed-download--ensure-directory dir)
    (start-process "playwright-pdf" "*pdf-gen*"
                   "node"
                   elfeed-download-node-script-path
                   url pdf-file)))

(defun elfeed-download-current-entry ()
  "Download the current Elfeed entry (video or article) and mark it read."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (when entries
      (let* ((entry (if (listp entries) (car entries) entries))
             (url (elfeed-entry-link entry))
             (title (elfeed-entry-title entry)))
        (if (string-match-p "youtube\\.com\\|youtu\\.be" url)
            (elfeed-download-youtube url title)
          (elfeed-download-article url title))
        (elfeed-untag entry 'unread)
        (elfeed-tag entry 'read)
        (elfeed-search-update-entry entry)))))

(defun elfeed-download-setup ()
  "Ensure directories exist and add keybindings for Elfeed downloads."
  (elfeed-download--ensure-directory elfeed-download-base-dir)
  (elfeed-download--ensure-directory (elfeed-download--youtube-dir))
  (elfeed-download--ensure-directory (elfeed-download--articles-dir))
  (when (boundp 'elfeed-search-mode-map)
    (map! :map elfeed-search-mode-map
          :n "d" #'elfeed-download-current-entry))
  (message "Elfeed download integration ready."))

(add-hook 'elfeed-search-mode-hook #'elfeed-download-setup)

(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

