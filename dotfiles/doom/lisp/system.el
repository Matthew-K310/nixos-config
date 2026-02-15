;;; ../../.dotfiles/.config/doom/lisp/system.el -*- lexical-binding: t; -*-

(setq display-time-format "%l:%M %p %b %d W%U"
      display-time-load-average-threshold 0.0)

;; -- Tab Bar -----

(use-package tab-bar
  :ensure nil
  :bind (("s-[" . tab-bar-switch-to-prev-tab)
         ("s-]" . tab-bar-switch-to-next-tab)
         ("s-{" . (lambda ()
                    (interactive)
                    (tab-move -1)))
         ("s-}" . (lambda ()
                    (interactive)
                    (tab-move 1))))
  :custom
  (tab-bar-show t)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-menu-bar
                    tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))

  ;; Like winner-mode for tabs
  (tab-bar-history-mode 1)
  (tab-bar-mode 1))

;;; -- Make Help More Helpful -----

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))

(provide 'system)

