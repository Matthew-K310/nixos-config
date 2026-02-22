;;; ../../nixos-config/dotfiles/doom/lisp/nix-search.el -*- lexical-binding: t; -*-

(defun my/search-nixpkgs-stable (query)
  (interactive "sSearch NixOS packages: ")
  (request
    "https://search.nixos.org/backend/latest-44-nixos-25.11/_search"
    :type "POST"
    :headers '(("Content-Type" . "application/json")
               ("Authorization" . "Basic YVdWU0FMWHBadjpYOGdQSG56TDUyd0ZFZWt1eHNmUTljU2g="))
    :data (json-encode
           `((query . ((multi_match . ((query . ,query)
                                       (fields . ["package_attr_name"
                                                  "package_pname"
                                                  "package_description"])))))
             (size . 10)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((hits (alist-get 'hits (alist-get 'hits data)))
                       (candidates
                        (mapcar (lambda (hit)
                                  (let* ((source (alist-get '_source hit))
                                         (name    (alist-get 'package_attr_name source))
                                         (version (alist-get 'package_pversion source))
                                         (desc    (alist-get 'package_description source)))
                                    (format "%s (%s) - %s" name version desc)))
                                hits)))
                  (let ((selection (completing-read "Select package: " candidates)))
                    (kill-new (car (split-string selection " ")))  ; copy attr name to clipboard
                    (message "Copied %s" selection)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error: %S" error-thrown)))))

(defun my/search-nixpkgs-unstable (query)
  (interactive "sSearch NixOS packages: ")
  (request
    "https://search.nixos.org/backend/latest-44-nixos-unstable/_search"
    :type "POST"
    :headers '(("Content-Type" . "application/json")
               ("Authorization" . "Basic YVdWU0FMWHBadjpYOGdQSG56TDUyd0ZFZWt1eHNmUTljU2g="))
    :data (json-encode
           `((query . ((multi_match . ((query . ,query)
                                       (fields . ["package_attr_name"
                                                  "package_pname"
                                                  "package_description"])))))
             (size . 10)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((hits (alist-get 'hits (alist-get 'hits data)))
                       (candidates
                        (mapcar (lambda (hit)
                                  (let* ((source (alist-get '_source hit))
                                         (name    (alist-get 'package_attr_name source))
                                         (version (alist-get 'package_pversion source))
                                         (desc    (alist-get 'package_description source)))
                                    (format "%s (%s) - %s" name version desc)))
                                hits)))
                  (let ((selection (completing-read "Select package: " candidates)))
                    (kill-new (car (split-string selection " ")))  ; copy attr name to clipboard
                    (message "Copied %s" selection)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error: %S" error-thrown)))))

(global-set-key (kbd "C-x n s s") 'my/search-nixpkgs-stable)
(global-set-key (kbd "C-x n s u") 'my/search-nixpkgs-unstable)
