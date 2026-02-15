;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed)
(package! elfeed-goodies)
(when (modulep! +org)
  (package! elfeed-org))
(when (modulep! +youtube)
  (package! elfeed-tube))
