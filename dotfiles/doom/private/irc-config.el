;;; ../../.dotfiles/.config/doom/private/irc-config.el -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'erc)
(require 'circe)

;; erc setup
(defun run-erc ()
  "Connect to irc.matthewservers.org, reading the password from ~/.authinfo.gpg."
  (interactive)
  (let* ((host "irc.matthewservers.org")
         (port 6697)
         (auth (car (auth-source-search :host host :port port :require '(:secret))))
         (secret (plist-get auth :secret))
         (password (if (functionp secret) (funcall secret) secret)))
    (erc-tls :server host
             :port port
             :nick "mattken"
             :user "mattken/irc.libera.chat"
             :password password)))

(defun erc-cmd-LATEST (&optional channel)
  "Manually retrieve latest messages from an IRCv3 bouncer."
  (interactive "sTarget Channel: ")
  (let ((channel (if (> (length channel) 0) channel (erc-default-target))))
    (erc-cmd-QUOTE
     (format "CHATHISTORY LATEST %s * %s" channel 500))))

(defun erc-cmd-TABLEFLIP ()
  (interactive)
  (insert "(╯°□°)╯︵ ┻━┻"))

(defun erc-cmd-SHRUG ()
  (interactive)
  (insert "¯\_(ツ)_/¯"))

(setq erc-fill-column 120
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)

;; desktop notifications for mentions
(add-to-list 'erc-modules 'notifications)
