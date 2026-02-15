;;; bases.el --- Add FILENAME to the database in OUTPUT-FILE -*- lexical-binding: t; -*-

;;; Commentary:
;;; An implementation of https://help.obsidian.md/bases in org-mode

;;; Code:

(defvar bases/output-file "~/org/bases.org")

(defun bases/get-filetags (filename)
  "Extract filetags from FILENAME and return as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+filetags:\\s-*\\(.*\\)$" nil t)
        (let ((tags-str (match-string 1)))
          (if (string-empty-p (string-trim tags-str))
              "none"
            (string-trim tags-str)))
      "none")))

(defun bases/append-file-info-to-table (filename &optional output-file)
  "Add the FILENAME to the database OUTPUT-FILE."
  (interactive "fFile to analyze: ")
  (setq output-file (or output-file bases/output-file))
  (let* ((attrs (file-attributes filename))
         ;; file extension
         (extension (or (file-name-extension filename) "none"))
         ;; filesize
         (size (nth 7 attrs))
         ;; modification time
         (mod-time (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 attrs)))
         ;; filetags
         (filetags (bases/get-filetags filename))
         ;; add link to the file for quick navigation
         (file-link (format "[[file:%s][%s]]"
                            filename
                            (file-name-nondirectory filename)))
         (table-row (format "| %s | %s | %s | %s | %s |\n"
                            file-link
                            extension
                            size
                            mod-time
                            filetags)))
    (with-current-buffer (find-file-noselect output-file)
      (goto-char (point-max))
      (when (or (= (point-min) (point-max))
                (not (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "^| Filename" nil t))))
        (insert "| Filename | Extension | Size (bytes) | Modified Time | Filetags |\n")
        (insert "|----------+-----------+--------------+---------------+----------|\n"))
      (insert table-row)
      (save-buffer)
      (message "Added file info to %s" output-file))))

                                        ; for batch processing
(defun bases/process-directory (directory &optional output-file)
  "Process all .org files in DIRECTORY and to the database table in OUTPUT-FILE."
  (interactive "DDirectory: ")
  (setq output-file (or output-file bases/output-file))
  (let ((org-files (directory-files directory t "\\.org$")))
    (dolist (file org-files)
      (when (file-regular-p file)
        (bases/append-file-info-to-table file output-file)))
    (message "Processed %d files from %s" (length org-files) directory)))

(provide 'bases)

;;; bases.el ends here
