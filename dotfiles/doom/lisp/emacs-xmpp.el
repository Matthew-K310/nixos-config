;;; ../../.dotfiles/.config/doom/lisp/emacs-xmpp.el -*- lexical-binding: t; -*-


;; send to unix socket, prompts for jid and message
;; testing only
(defun xmpp-send (jid text)
  (interactive "sJID: \nsMessage: ")
  (let ((socket (make-network-process
                 :name "xmpp-daemon"
                 :buffer "*xmpp-daemon*"
                 :family 'local
                 :service "/tmp/xmpp-daemon.sock")))
    (process-send-string socket (concat jid ":" text "\n"))
    (delete-process socket)))



(provide 'emacs-xmpp)
