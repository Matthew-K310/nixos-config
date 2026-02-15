;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(package! mu4e-compat
  :recipe (:host github :repo "tecosaur/mu4e-compat"))
(when (modulep! +org)
  (package! org-msg))
