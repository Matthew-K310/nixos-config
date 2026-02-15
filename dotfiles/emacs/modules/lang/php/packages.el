;; -*- no-byte-compile: t; -*-
;;; lang/php/packages.el

(package! psysh)
(package! php-mode)
(package! php-refactor-mode)
(package! phpunit)
(package! composer)

(when (modulep! +hack)
  (package! hack-mode
    :recipe (:host github :repo "hhvm/hack-mode")))
