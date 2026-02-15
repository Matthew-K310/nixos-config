;;; ../../.dotfiles/.config/doom/lisp/change-fonts.el -*- lexical-binding: t; -*-
(require 'ivy)

(defun modify-frame-parameter (parameter value)
  (if (null (assoc parameter default-frame-alist))
      (add-to-list 'default-frame-alist `(,parameter . ,value))
    (setf (cdr (assoc parameter default-frame-alist)) value)))

(defvar zyd/font-history ())

(defvar zyd/fonts
  '(("JetBrainsMono Nerd Font Mono" . "JetBrainsMono Nerd Font Mono-14")
    ("Alegreya" . "Alegreya-14")
    ("Iosevka Nerd Font Mono" . "Iosevka Nerd Font Mono-12")
    ("Terminess Nerd Font Mono" . "Terminess Nerd Font Mono-14")))

(defun zyd/lookup-font (font)
  (cdr (assoc font zyd/fonts)))

(defun zyd/update-frame-font (font)
  (modify-frame-parameter 'font font)
  (set-frame-font font))

(defun zyd/change-font ()
  (interactive)
  (let ((current-font
         (symbol-name (font-get (face-attribute 'default :font) :family))))
    (ivy-read "Font: " zyd/fonts
              :preselect current-font
              :require-match t
              :history 'zyd/font-history
              :action (lambda (font) (zyd/update-frame-font (cdr font)))
              :caller 'zyd/change-font)))

(defun zyd/font-with-sample (font-name)
  (format "%-75s%s" font-name
          (propertize
           "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 {}[]()!@#$%^&*-_=+;:'\",./<>?`~"
           'face (list :family font-name))))

(ivy-configure 'zyd/change-font
  :display-transformer-fn #'zyd/font-with-sample)

(defmacro define-font (symbol font-name)
  `(defvar ,symbol (zyd/lookup-font ,font-name)))

(defun zyd/default-font (font)
  (zyd/update-frame-font font))

;;; What I have in my init, defining some fonts (with a defvar wrapper) and
;;; setting the default font. Also convenient alias for `M-x'ing:
(defalias 'font 'zyd/change-font)

;; Example usage - uncomment and define tx02 before using:
;; (define-font tx02 "JetBrainsMono Nerd Font Mono")
;; (zyd/default-font tx02)
