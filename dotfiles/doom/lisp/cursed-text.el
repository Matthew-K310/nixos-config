;;; cursed-text.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Matthew Kennedy
;;
;; Author: Matthew Kennedy <matthew@matthew-kennedy.com>
;; Maintainer: Matthew Kennedy <matthew@matthew-kennedy.com>
;; Created: January 16, 2026
;; Modified: January 16, 2026
;; Version: 0.0.1
;; Homepage: https://github.com/Matthew-K310/cursed-text
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar cursed-text-combining-marks
  (number-sequence #x0300 #x0364)
  "List of Unicode combining mark codepoints.")

(defun cursed-text-generate (text intensity)
  "Add INTENSITY amount of random diacritics to each character in TEXT."
  (mapconcat
   (lambda (char)
     (concat (string char)
             (mapconcat
              (lambda (_)
                (string (nth (random (length cursed-text-combining-marks))
                             cursed-text-combining-marks)))
              (number-sequence 1 intensity)
              "")))
   text
   ""))

(defun cursed-text-region (start end intensity)
  "Apply cursed text effect to region from START to END with INTENSITY."
  (interactive "r\nnIntensity (1-10): ")
  (let* ((text (buffer-substring start end))
         (cursed (cursed-text-generate text intensity)))
    (delete-region start end)
    (insert cursed)))

(provide 'cursed-text)
;;; cursed-text.el ends here
