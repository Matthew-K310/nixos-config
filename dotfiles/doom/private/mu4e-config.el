;;; ../../.dotfiles/.config/doom/private/mu4e-config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)

(setq
 mu4e-maildir "~/.local/share/mail"
 mu4e-get-mail-command "mbsync --all"
 mu4e-update-interval 300
 mu4e-headers-auto-update t
 mu4e-view-show-images t
 mu4e-compose-signature-auto-include nil
 mu4e-use-fancy-chars t

 ;; Sending mail
 sendmail-program "/usr/bin/msmtp"
 message-send-mail-function 'message-send-mail-with-sendmail
 message-sendmail-extra-arguments '("--read-envelope-from")
 message-sendmail-f-is-evil t

 ;; Context behavior
 mu4e-context-policy 'pick-first
 mu4e-compose-context-policy 'ask

 ;; Quality of life
 mu4e-compose-format-flowed t
 mu4e-view-show-addresses t
 mu4e-change-filenames-when-moving t
 ;; mu4e-attachment-dir "~/cloud/downloads/mail"
 )

;; suppress the personal account warning
(setq mu4e-hide-personal-addresses t)

;; === Contexts ===
(setq mu4e-contexts
      (list
       ;; Personal
       (make-mu4e-context
        :name "Personal"
        :match-func
        (lambda (msg)
          (when msg
            (string-match-p "matthew@matthew-kennedy.com"
                            (concat (or (mu4e-message-field msg :to) "")
                                    (or (mu4e-message-field msg :from) "")))))
        :vars '((user-mail-address . "matthew@matthew-kennedy.com")
                (user-full-name    . "Matthew Kennedy")
                (msmtp-account     . "matthew@matthew-kennedy.com")
                (mu4e-sent-folder   . "/personal/Sent")
                (mu4e-drafts-folder . "/personal/Drafts")
                (mu4e-trash-folder  . "/personal/Trash")
                (mu4e-refile-folder . "/personal/Archive")))

       ;; IT
       (make-mu4e-context
        :name "IT"
        :match-func
        (lambda (msg)
          (when msg
            (string-match-p "it@matthew-kennedy.com"
                            (concat (or (mu4e-message-field msg :to) "")
                                    (or (mu4e-message-field msg :from) "")))))
        :vars '((user-mail-address . "it@matthew-kennedy.com")
                (user-full-name    . "Matthew Kennedy")
                (msmtp-account     . "it@matthew-kennedy.com")
                (mu4e-sent-folder   . "/it/Sent")
                (mu4e-drafts-folder . "/it/Drafts")
                (mu4e-trash-folder  . "/it/Trash")
                (mu4e-refile-folder . "/it/Archive")))

       ;; iCloud
       (make-mu4e-context
        :name "iCloud"
        :match-func
        (lambda (msg)
          (when msg
            (string-match-p "matthew_i_kennedy@icloud.com"
                            (concat (or (mu4e-message-field msg :to) "")
                                    (or (mu4e-message-field msg :from) "")))))
        :vars '((user-mail-address . "matthew_i_kennedy@icloud.com")
                (user-full-name    . "Matthew Kennedy")
                (msmtp-account     . "matthew_i_kennedy@icloud.com")
                (mu4e-sent-folder   . "/icloud/Sent Messages")
                (mu4e-drafts-folder . "/icloud/Drafts")
                (mu4e-trash-folder  . "/icloud/Deleted Messages")
                (mu4e-refile-folder . "/icloud/Archive")))

       ;; Business
       (make-mu4e-context
        :name "Business"
        :match-func
        (lambda (msg)
          (when msg
            (string-match-p "business@matthew-kennedy.com"
                            (concat (or (mu4e-message-field msg :to) "")
                                    (or (mu4e-message-field msg :from) "")))))
        :vars '((user-mail-address . "business@matthew-kennedy.com")
                (user-full-name    . "Matthew Kennedy Productions")
                (msmtp-account     . "business@matthew-kennedy.com")
                (mu4e-sent-folder   . "/business/Sent")
                (mu4e-drafts-folder . "/business/Drafts")
                (mu4e-trash-folder  . "/business/Trash")
                (mu4e-refile-folder . "/business/Archive")))

       ;; Accounts
       (make-mu4e-context
        :name "Accounts"
        :match-func
        (lambda (msg)
          (when msg
            (string-match-p "accounts@matthew-kennedy.com"
                            (concat (or (mu4e-message-field msg :to) "")
                                    (or (mu4e-message-field msg :from) "")))))
        :vars '((user-mail-address . "accounts@matthew-kennedy.com")
                (user-full-name    . "Matthew Kennedy")
                (msmtp-account     . "accounts@matthew-kennedy.com")
                (mu4e-sent-folder   . "/accounts/Sent")
                (mu4e-drafts-folder . "/accounts/Drafts")
                (mu4e-trash-folder  . "/accounts/Trash")
                (mu4e-refile-folder . "/accounts/Archive")))

       ;; Mailing
       (make-mu4e-context
        :name "Mailing"
        :match-func
        (lambda (msg)
          (when msg
            (string-match-p "mailing@matthew-kennedy.com"
                            (concat (or (mu4e-message-field msg :to) "")
                                    (or (mu4e-message-field msg :from) "")))))
        :vars '((user-mail-address . "mailing@matthew-kennedy.com")
                (user-full-name    . "Matthew Kennedy")
                (msmtp-account     . "mailing@matthew-kennedy.com")
                (mu4e-sent-folder   . "/mailing/Sent")
                (mu4e-drafts-folder . "/mailing/Drafts")
                (mu4e-trash-folder  . "/mailing/Trash")
                (mu4e-refile-folder . "/mailing/Archive")))))

(setq mu4e-compose-signature "God bless, \n Matthew Kennedy"
      mu4e-compose-context-policy 'ask)
