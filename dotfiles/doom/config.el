;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matthew Kennedy"
      user-mail-address "matthew@matthew-kennedy.com")

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

;; allow unlocking of GPG keys in mini-buffer
(setq epg-pinentry-mode 'loopback)

(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB

(setq doom-inhibit-large-file-detection t)

;; Relative line numbers
(setq display-line-numbers-type `relative)

;; Better readability while scrolling
(setq scroll-margin 30)

;; Speed of which-key popup
(setq which-key-idle-delay 0.1)

;; Set tab width
(setq-default tab-width 4)

;; Set browser for opening links
(setq browse-url-browser-function 'browse-url-generic)
;; (setq browse-url-generic-program "helium-browser")
(setq browse-url-generic-program "chromium")

;; Change focus highlight mode
'((prog-mode . defun) (text-mode . paragraph))

(setq tramp-default-method "ssh")

;; prevent eww popups
(remove-hook 'eww-mode-hook #'+popup-buffer-mode)

;; nov.el settings
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq +latex-viewers '(pdf-tools))

(load! "lisp/audio-record")
(load! "lisp/bases")
(load! "lisp/cursed-text")
(load! "lisp/change-fonts")
(load! "lisp/daily")
(load! "lisp/done-refile")
(load! "lisp/emacs-xmpp")
(load! "lisp/jitsi-meeting")
(load! "lisp/meeting-assistant")
(load! "lisp/mu4e-contact")
(load! "lisp/nm")
(load! "lisp/pg-config")
(load! "lisp/pomodoro")
(load! "lisp/popup-dirvish-browser")
(load! "lisp/popup-scratch")
(load! "lisp/post-to-blog")
(load! "lisp/termux-sms")
(load! "lisp/theo")
(load! "lisp/universal-launcher")
(load! "lisp/weather")
(load! "private/irc-config")
(load! "private/mu4e-config")

;; (add-to-list 'load-path "~/.config/doom/lisp/tui.el/")
;; (require 'tui)

;; Custom keymaps
(map! :leader
      
      (:prefix ("b" . "blog")
       :desc "Create new blog post" "n" #'blog/new-post 
       :desc "Export blog post to Hugo MD" "e" #'org-hugo-export-to-md
       :desc "Run Air dev server" "r" #'blog/site-serve
       :desc "Run Hugo dev server" "h" #'blog/hugo-serve)
      
      (:prefix ("c" . "curse")
       :desc "Enact curses" "t" #'cursed-text-region)
      
      ;; elfeed and erc keybinds
      (:prefix ("e" . "elfeed")
       :desc "Open eshell"         "s" #'eshell
       :desc "Open elfeed"         "e" #'elfeed
       :desc "Update elfeed"       "u" #'elfeed-update
       :desc "Open elfeed-tube-mpv" "t" #'elfeed-tube-mpv
       :desc "Start ERC" "r" #'run-erc
       :desc "Start ERC" "c" #'erc
       :desc "Open EWW" "w" #'eww)

      ;; Magit mode mappings
      (:prefix ("g" . "magit")
       :desc "Stage all files"          "a" #'magit-stage-modified
       :desc "goto function definition" "d" #'evil-goto-definition
       :desc "Push"                     "P" #'magit-push
       :desc "Pull"                     "p" #'magit-pull
       :desc "Merge"                    "m" #'magit-merge
       :desc "Quick commit and push"    "z" #'my/magit-stage-commit-push)
      
      (:prefix ("i" . "inhibit")
       :desc "Inhibit mouse" "m" #'inhibit-mouse-mode)

      (:prefix ("k" . "kill")
       :desc "Kill process" "p" #'kill-process)
      
      (:prefix ("l" . "line")
       :desc "Toggle line numbers" "n" #'display-line-numbers-mode)
      
      (:prefix ("m" . "music/EMMS")
       :desc "Update from MPD" "u" #'my/update-emms-from-mpd
       :desc "Play at directory tree" "d" #'emms-play-directory-tree
       :desc "Go to emms playlist" "p" #'emms-playlist-mode-go
       :desc "Shuffle" "h" #'emms-shuffle
       :desc "Emms pause track" "x" #'emms-pause
       :desc "Emms stop track" "s" #'emms-stop
       :desc "Emms play previous track" "b" #'emms-previous
       :desc "Emms play next track" "n" #'emms-next
       :desc "EMMS Browser" "o" #'emms-browser)

      ;; notes keybinds
      (:prefix ("n" . "notes")
       :desc "Insert Noter Annotation" "i" #'org-noter-insert-note)

      ;; (:prefix ("r" . "roam")
      ;;  :desc "Find Roam Node"    "f" #'org-roam-node-find
      ;;  :desc "Insert Roam Node"  "i" #'org-roam-node-insert
      ;;  :desc "Toggle Roam Buffer" "t" #'org-roam-buffer-toggle)
      
      ;; open calendar
      (:prefix ("o" . "open")
       :desc "Calendar" "c" #'=calendar
       :desc "MU4E" "m" #'mu4e)

      ;; Org mode mappings
      (:prefix ("y" . "org-mode-specifics")
       :desc "Export as markdown"        "e" #'org-md-export-as-markdown
       :desc "Preview markdown file"     "p" #'markdown-preview
       :desc "Export as html"            "h" #'org-html-export-as-html
       :desc "Search dictionary at word" "d" #'dictionary-lookup-definition
       :desc "Powerthesaurus lookup"     "t" #'powerthesaurus-lookup-word-at-point
       :desc "Read Aloud This"           "r" #'read-aloud-this
       :desc "Export to PDF"             "l" #'org-pandoc-export-to-latex-pdf
       :desc "Export to PDF & Open"      "L" #'org-pandoc-export-to-latex-pdf-and-open
       :desc "Spell check"               "z" #'ispell-word
       :desc "Find definition"           "f" #'lsp-find-definition)

      ;; Focus/Zen
      (:prefix ("z" . "focus")
       :desc "Toggle zen mode"   "z" #'+zen/toggle
       :desc "Toggle focus mode" "f" #'focus-mode))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.15) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c p") 'org-capture)
(define-key global-map (kbd "C-c b n") #'blog/new-post)
(define-key global-map (kbd "C-c b h") #'blog/blog-serve-dev) ;; h for hugo
(define-key global-map (kbd "C-c b s") #'blog/site-serve-dev) ;; s for site

;; ;; inbox capture
;; (defun org-capture-inbox ()
;;   (interactive)
;;   (call-interactively 'org-store-link)
;;   (org-capture nil "n"))

;; (define-key global-map (kbd "C-c f") 'org-capture-inbox)

;; random global bind lol
;; (global-set-key (kbd "C-c n f") #'org-roam-node-find)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x C-a") 'my/archive-done-task)
  ;; (define-key org-mode-map (kbd "C-c n l") #'org-roam-buffer-toggle)
  ;; (define-key org-mode-map (kbd "C-c n i") #'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c e") #'org-set-effort)
  (define-key org-mode-map (kbd "C-c i") #'org-clock-in)
  (define-key org-mode-map (kbd "C-c o") #'org-clock-out)
  (define-key org-mode-map (kbd "C-c b e") #'org-hugo-export-to-md))

;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
;;       doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
;;       doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 16)
      doom-big-font (font-spec :family "Iosevka Nerd Font Mono" :size 22))

;; (setq doom-font (font-spec :family "Geist Mono" :size 14)
;;       doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
;;       doom-big-font (font-spec :family "Geist Mono" :size 22))

(custom-theme-set-faces!
  'compline
  '(org-level-8 :inherit outline-3 :height 1.0)
  '(org-level-7 :inherit outline-3 :height 1.0)
  '(org-level-6 :inherit outline-3 :height 1.0)
  '(org-level-5 :inherit outline-3 :height 1.1)
  '(org-level-4 :inherit outline-3 :height 1.2)
  '(org-level-3 :inherit outline-3 :height 1.3)
  '(org-level-2 :inherit outline-3 :height 1.4)
  '(org-level-1 :inherit outline-3 :height 1.5)
  '(org-document-title :height 1.6 :bold t :underline nil))

(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
(load-theme 'compline t)
;; (load-theme 'lauds t)

;; Remove top frame bar in emacs
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(add-to-list 'default-frame-alist '(undecorated . t))

;; Emacs statusline
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)

(setq writeroom-width 80)

;; Setup custom splashscreen
(defun ozymandias ()
  (let* ((banner '(
                   "I met a traveller from an antique land,"
                   "Who said - 'Two vast and trunkless legs of stone"
                   "Stand in the desert. . . . Near them, on the sand,"
                   "Half sunk a shattered visage lies, whose frown,"
                   "And wrinkled lip, and sneer of cold command,"
                   "Tell that its sculptor well those passions read"
                   "Which yet survive, stamped on these lifeless things,"
                   "The hand that mocked them, and the heart that fed;"
                   "And on the pedestal, these words appear:"
                   "My name is Ozymandias, King of Kings;"
                   "Look on my Works, ye Mighty, and despair!'"
                   "Nothing beside remains. Round the decay"
                   "Of that colossal Wreck, boundless and bare"
                   "The lone and level sands stretch far away."
                   ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32))) ; 32 is space character
               "\n"))
     'face '(:foreground "white"))))

(setq +doom-dashboard-ascii-banner-fn #'ozymandias)

(setq +doom-dashboard-functions
      (list #'doom-dashboard-widget-banner))

(after! doom-dashboard
  (assoc-delete-all "Jump to bookmark" +doom-dashboard-menu-sections)
  (assoc-delete-all "Open documentation" +doom-dashboard-menu-sections))

(after! persp-mode
  ;; alternative, non-fancy version which only centers the output of +workspace--tabline
  (defun workspaces-formatted ()
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun hy/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right hy/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore))

;; need to run this later for it to not break frame size for some reason
(run-at-time nil nil (cmd! (tab-bar-mode +1)))

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src")
        (templ      "https://github.com/templ-go/tree-sitter-templ")
        (html      "https://github.com/tree-sitter/tree-sitter-html")
        (css      "https://github.com/tree-sitter/tree-sitter-css")
        (rust      "https://github.com/tree-sitter/tree-sitter-rust")
        (zig      "https://github.com/tree-sitter/zig-tree-sitter")
        (c      "https://github.com/tree-sitter/tree-sitter-c")
        (csharp      "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (php      "https://github.com/tree-sitter/tree-sitter-php")
        (swift      "https://github.com/tree-sitter/swift-tree-sitter")
        (ruby      "https://github.com/tree-sitter/tree-sitter-ruby")
        (haskell      "https://github.com/tree-sitter/tree-sitter-haskell")
        (go      "https://github.com/tree-sitter/tree-sitter-go")))

(after! tree-sitter
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(org-mode . go)))

;; enable all analyzers; not done by default
(after! lsp-mode
  (setq  lsp-go-analyses '((fieldalignment . t)
                           (nilness . t)
                           (shadow . t)
                           (unusedparams . t)
                           (unusedwrite . t)
                           (useany . t)
                           (unusedvariable . t)))
  )

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; rust-analyzer integration
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.3)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(after! lsp-haskell
  (setq lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

(defun my-open-vterm-at-point ()
  "Open vterm in the directory of the currently selected window's buffer.
This function is designed to be called via `emacsclient -e`."
  (interactive)
  (let* ((selected-window (selected-window))
         ;; Ensure selected-window is not nil before trying to get its buffer
         (buffer-in-window (and selected-window (window-buffer selected-window)))
         dir)

    (when buffer-in-window
      (setq dir
            ;; Temporarily switch to the target buffer to evaluate its context
            (with-current-buffer buffer-in-window
              (cond ((buffer-file-name buffer-in-window)
                     (file-name-directory (buffer-file-name buffer-in-window)))
                    ((and (eq major-mode 'dired-mode)
                          (dired-current-directory))
                     (dired-current-directory))
                    (t default-directory)))))

    ;; Fallback to the server's default-directory if no specific directory was found
    (unless dir (setq dir default-directory))

    (message "Opening vterm in directory: %s" dir) ; For debugging, check *Messages* buffer

    ;; Now, crucially, set 'default-directory' for the vterm call itself
    (let ((default-directory dir))
      ;; Call the plain 'vterm' function, which should respect 'default-directory'.
      ;; We are *not* passing 'dir' as an argument to 'vterm' here,
      ;; as it's often designed to pick up the current 'default-directory'.
      (vterm))))

(defun my/new-frame-with-vterm ()
  "Create a new frame and immediately open vterm in it."
  (interactive)
  (require 'vterm)
  (let ((new-frame (make-frame '((explicit-vterm . t)))))
    (select-frame new-frame)
    (delete-other-windows)
    ;; vterm creates and switches to the buffer, but returns nil
    (vterm (format "*vterm-%s*" (frame-parameter new-frame 'name)))
    ;; Now current-buffer is the vterm buffer
    (delete-other-windows)))

(require 'org)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")
(setq org-agenda-diary-file "~/org/agenda.org")

(after! org
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))


(use-package! org-download
  :after org
  :config
  (setq-default org-download-screenshot-method "scrot -s %s"))

;; Keybinds for org mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x C-a") 'my/archive-done-task)
  ;; (define-key org-mode-map (kbd "C-c n l") #'org-roam-buffer-toggle)
  ;; (define-key org-mode-map (kbd "C-c n f") #'org-roam-node-find)
  ;; (define-key org-mode-map (kbd "C-c n i") #'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c e") #'org-set-effort)
  (define-key org-mode-map (kbd "C-c i") #'org-clock-in)
  (define-key org-mode-map (kbd "C-c o") #'org-clock-out))

(defun my/org-checkbox-intermediate ()
  "Set checkbox at point to [-]."
  (interactive)
  (when (org-at-item-checkbox-p)
    (replace-match "-" t t nil 1)))

(map! :map org-mode-map
      :leader
      :desc "Mark checkbox intermediate"
      "m c i" #'my/org-checkbox-intermediate)

(defun my/pandoc-convert-org-to-docx ()
  "Use Pandoc to convert .org to .docx.
Comments:
- The `-N' flag numbers the headers lines.
- Use the `--from org' flag to have this function work on files
  that are in Org syntax but do not have a .org extension"
  (interactive)
  (message "exporting .org to .docx")
  (shell-command
   (concat "pandoc -N --from org " (buffer-file-name)
           " -o "
           (file-name-sans-extension (buffer-file-name))
           (format-time-string "-%Y-%m-%d-%H%M%S") ".docx")))

;; Auto-clock in when state changes to STRT
(defun my/org-clock-in-if-starting ()
  "Clock in when the task state changes to STRT."
  (when (and (string= org-state "STRT")
             (not (org-clock-is-active)))
    (org-clock-in)))

;; Auto-clock out when leaving STRT state (but not to DONE - handle that separately)
(defun my/org-clock-out-if-not-starting ()
  "Clock out when leaving STRT state, unless going to DONE."
  (when (and (org-clock-is-active)
             (not (member org-state '("STRT" "DONE"))))
    (org-clock-out)))

;; Add these functions to org-after-todo-state-change-hook
(add-hook 'org-after-todo-state-change-hook 'my/org-clock-in-if-starting)
(add-hook 'org-after-todo-state-change-hook 'my/org-clock-out-if-not-starting)

;; Prevent clock from stopping when marking subtasks as done
(setq org-clock-out-when-done nil)

(defun my/auto-archive-done-tasks ()
  "Automatically archive task when marked as DONE."
  (when (and (string= org-state "DONE")
             (not (org-is-habit-p)))
    (my/move-to-done-org)))

;; Add frame borders and window dividers
;; Option 1: Per buffer
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Option 2: Globally
(with-eval-after-load 'org (global-org-modern-mode))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(global-org-modern-mode)

;; increase line spacing
(setq-default line-spacing 0.1)

;; Set org-modern to use replace mode
(setq org-modern-star 'replace)
;; Customize the replacement strings to show H1, H2, H3, etc.
(setq org-modern-replace-stars '("H1" "H2" "H3" "H4" "H5" "H6"))

(after! org
  (setq org-log-done 'time)
  ;; Automatically resume previous task when clocking out
  (setq org-clock-out-resume t)
  
  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/org/inbox.org" "Todo")
           "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?" :prepend t)
          ("d" "Denote (New Note)" plain
           (file denote-last-path)
           #'denote-org-capture
           :no-save t
           :immediate-finish nil
           :kill-buffer t
           :jump-to-captured t)
          ("c" "Calendar")
          ("cb" "Time Blocks" entry (file+datetree "~/org/work.org" "Time Blocks")
           "* Work on %?\nSCHEDULED: %T\n" :clock-in t :clock-keep t)
          ("ce" "Event" entry
           (file+headline "~/org/calendar.org" "Events")
           "* %^{Event}\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
          ("cd" "Deadline" entry
           (file+headline "~/org/calendar.org" "Deadlines")
           "* NEXT %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
          ("p" "Project" entry
           (file+headline "~/org/projects.org" "Projects")
           "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:END:\n** TODO %?")
          ("P" "Post Idea" entry
           (file+headline "~/org/denote/20260128T201802--blog-post-backlog__backlog_blog.org" "Capture")
           "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:END:\n** TODO %?")
          ("i" "Inbox Capture" entry
           (file+headline "~/org/inbox.org" "Capture")
           "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?" :prepend t)
          ("b" "Bookmark")
          ("bc" "Bookmark (Clipboard)" entry (file+headline "~/org/bookmarks.org" "Bookmarks")
           "** %(org-web-tools-insert-link-for-clipboard-url)\n:PROPERTIES:\n:CAPTURED: %t\n:END:\n%?" :empty-lines 1 :prepend t)
          ("bp" "Bookmark (Paste)" entry (file+headline "~/org/bookmarks.org" "Bookmarks")
           "** %(org-web-tools-insert-link-for-given-url)\n:PROPERTIES:\n:CAPTURED: %t\n:END:\n%?" :empty-lines 1 :prepend t)
          ("bw" "Bookmark (Watch Later)" entry (file+headline "~/org/bookmarks.org" "Watch Later")
           "** %(org-web-tools-insert-link-for-given-url)\n:PROPERTIES:\n:CAPTURED: %t\n:END:\n%?" :empty-lines 1 :prepend t)
          ("c" "Contact" entry
           (file+headline "~/org/contacts.org" "Contacts")
           "* %^{Name}
:PROPERTIES:
:CREATED: %U
:EMAIL: %^{Email}
:PHONE: %^{Phone}
:BIRTHDAY: %^{Birthday +1y}u
:LOCATION: %^{Address}
:END:
%?")
          )))

(after! org
  (defun org-capture-bookmark-tags ()
    "Get tags from existing bookmarks and prompt for tags with completion."
    (save-window-excursion
      (let ((tags-list '()))
        ;; Collect existing tags
        (with-current-buffer (find-file-noselect "~/org/bookmarks.org")
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^:TAGS:\\s-*\\(.+\\)$" nil t)
              (let ((tag-string (match-string 1)))
                (dolist (tag (split-string tag-string "[,;]" t "[[:space:]]"))
                  (push (string-trim tag) tags-list))))))
        ;; Remove duplicates and sort
        (setq tags-list (sort (delete-dups tags-list) 'string<))
        ;; Prompt user with completion
        (let ((selected-tags (completing-read-multiple "Tags (comma-separated): " tags-list)))
          ;; Return as a comma-separated string
          (mapconcat 'identity selected-tags ", ")))))
  
  ;; Helper function to select and link a contact
  (defun org-capture-ref-link (file)
    "Create a link to a contact in contacts.org"
    (let* ((headlines (org-map-entries
                       (lambda ()
                         (cons (org-get-heading t t t t)
                               (org-id-get-create)))
                       t
                       (list file)))
           (contact (completing-read "Contact: "
                                     (mapcar #'car headlines)))
           (id (cdr (assoc contact headlines))))
      (format "[[id:%s][%s]]" id contact))))

;; Make sure org-web-tools is loaded
(require 'org-web-tools)

;; pulls looks for link from kill ring
  (defun org-web-tools-insert-link-for-url (url)
  "Insert Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'."
  (interactive (list (org-web-tools--get-first-url)))
  (insert (org-web-tools--org-link-for-url url)))

;; Modified functions that explicitly return values
(defun org-web-tools-insert-link-for-clipboard-url ()
  "Return Org link for URL from clipboard or kill-ring"
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

(defun org-web-tools-insert-link-for-given-url ()
  "Return Org link for user-provided URL"
  (let ((url (read-string "Link: ")))
    (org-web-tools--org-link-for-url url)))

;; (setq org-agenda-files
;;       '("~/org/inbox.org"
;;         "~/org/todo.org"
;;         "~/org/habits.org"
;;         "~/org/projects.org"
;;         "~/org/agenda.org"
;;         "~/org/calendar.org"))

;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org"))

(after! org
  (setq org-agenda-files
        '("~/org/todo.org"
          "~/org/calendar.org"
          "~/org/inbox.org"
          "~/org/projects.org"
          "~/org/work.org")))

(setq org-tag-alist '(("inbox" . ?i)))

(setq org-agenda-custom-commands
      '(("a" "Main Agenda View"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)
                   (org-agenda-overriding-header "\nSchedule\n")))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %?-12t% s")
                 (org-agenda-overriding-header "\nUp Next\n")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (agenda ""
                  ((org-agenda-overriding-header "\nDeadlines (next 7 days)\n")
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "%A, %B %d")
                   (org-deadline-warning-days 7)))
          (todo "TODO"
                ((org-agenda-overriding-header "\nBacklog\n")
                 (org-agenda-files
                  (cl-remove-if (lambda (f)
                                  (string-match-p "habits\\.org$" f))
                                org-agenda-files))))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted Today\n")))))))

(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype \"%s\""
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype \"%s\" && wtype -k Return"
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(define-minor-mode thanos/type-mode
  "Minor mode for inserting text via wtype."
  :keymap `((,(kbd "C-c C-c") . ,(lambda () (interactive)
                                   (call-process-shell-command
                                    (thanos/wtype-text (buffer-string))
                                    nil 0)
                                   (delete-frame)))
            (,(kbd "C-c C-k") . ,(lambda () (interactive)
                                   (kill-buffer (current-buffer))))))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (flyspell-mode)
      (thanos/type-mode)
      (setq-local header-line-format
                  (format " %s to insert text or %s to cancel."
                          (propertize "C-c C-c" 'face 'help-key-binding)
			  (propertize "C-c C-k" 'face 'help-key-binding)))
      ;; Make the frame more temporary-like
      (set-frame-parameter frame 'delete-before-kill-buffer t)
      (set-window-dedicated-p (selected-window) t))))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n c" . denote-link-after-creating)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep)
   ("C-c n j" . denote-journal-new-entry)
   ("C-c n j" . denote-journal-new-entry))
  :config
  (setq denote-directory (expand-file-name "~/org/denote/"))
  (setq denote-blog-directory (expand-file-name "~/org/blog/"))
  
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(setq forge-owned-accounts '(("Matthew-K310")))

(use-package inhibit-mouse
  :custom
  ;; Disable highlighting of clickable text such as URLs and hyperlinks when hovered.
  (inhibit-mouse-adjust-mouse-highlight t)

  ;; Disable tooltips (show-help-function) during mouse events.
  (inhibit-mouse-adjust-show-help-function t)

  :config
  ;; Enable in both daemon and normal sessions
  (add-hook 'after-init-hook #'inhibit-mouse-mode)
  (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode))

;; dispatch function
(defun my/dirvish-jump ()
  "Jump to frequently used directories."
  (interactive)
  (let ((dir (completing-read "Jump to: "
               '(("Home" . "~/")
                 ("Dev" . "~/dev")
                 ("Site" . "~/site")
                 ("Projects" . "~/dev/projects")
                 ("Forks" . "~/dev/forks")
                 ("Share" . "~/.local/share")
                 ("Config" . "~/.config")
                 ("Cloud" . "~/cloud")
                 ("Photos" . "~/cloud/photos")
                 ("Screenshots" . "~/cloud/photos/screenshots")))))
    (dirvish (cdr (assoc dir '(("Home" . "~/")
                               ("Dev" . "~/dev")
                               ("Site" . "~/site")
                               ("Projects" . "~/dev/projects")
                               ("Forks" . "~/dev/forks")
                               ("Share" . "~/.local/share")
                               ("Config" . "~/.config")
                               ("Cloud" . "~/cloud")
                               ("Photos" . "~/cloud/photos")
                               ("Screenshots" . "~/cloud/photos/screenshots")))))))

(global-set-key (kbd "C-c j") 'my/dirvish-jump)

(defun my/get-cliphist-entries ()
  "Get the 50 most recent clipboard entries from cliphist, fully decoded."
  (when (executable-find "cliphist")
    (let* ((list-output (shell-command-to-string "cliphist list"))
           (lines (split-string list-output "\n" t))
           ;; Take only first 50 entries (newest first)
           (limited-lines (seq-take lines 50)))
      ;; Decode each entry to get full content
      (delq nil
            (mapcar (lambda (line)
                      (when (string-match "^\\([0-9]+\\)\t" line)
                        (let ((id (match-string 1 line)))
                          (string-trim (shell-command-to-string 
                                        (format "cliphist decode %s" id))))))
                    limited-lines)))))

(defun my/clipboard-manager ()
  "Browse kill ring + system clipboard history, copy selection to clipboard.
The full, untruncated text is always copied - truncation is only for display."
  (interactive)
  (require 'consult)
  (let* ((cliphist-items (my/get-cliphist-entries))
         (kill-ring-items kill-ring)
         (all-items (delete-dups (append cliphist-items kill-ring-items)))
         (candidates (mapcar (lambda (item)
                               (let ((display (truncate-string-to-width
                                             (replace-regexp-in-string "\n" " " item)
                                             80 nil nil "...")))
                                 (cons display item)))
                             all-items))
         (selected (consult--read
                    candidates
                    :prompt "Clipboard history: "
                    :sort nil
                    :require-match t
                    :category 'kill-ring
                    :lookup #'consult--lookup-cdr
                    :history 'consult--yank-history)))
    (when selected
      (with-temp-buffer
        (insert selected)
        (call-process-region (point-min) (point-max) "wl-copy"))
      
      (unless (member selected kill-ring)
        (kill-new selected))
      
      (message "Copied to clipboard: %s" 
               (truncate-string-to-width selected 50 nil nil "...")))))

(map! :leader
      (:prefix "y" 
       :desc "Clipboard manager" "y" #'my/clipboard-manager))

;; Set the graph layout algorithm
(setq denote-explore-network-algorithm 'spring) ; or 'circle, 'grid

;; Customize graph appearance
(setq denote-explore-network-keywords-include t) ; Show keyword connections
(setq denote-explore-network-regex ".*") ; Filter which notes to include

(defun my/denote-todo-template ()
  "Return string for daily tasks heading in `denote-journal' entries."
  (with-temp-buffer
    (org-mode)
    (let ((date-str (format-time-string "%Y-%m-%d (%a)")))
      (insert (format "* Tasks for %s\n** TODO \n** Loafing around\n\n* Notes for today\n\n* Clocktable\n"
                      date-str)))
    (let ((org-clock-clocktable-default-properties
           '(:scope file :maxlevel 3 :link nil :compact t :block today :formula %)))
      (org-clock-report))
    (buffer-string)))

(after! ledger-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ledger . t))))

(require 'company-emoji)
(after! org-mode
  (set-company-backend! org-mode 'company-emoji))

(defun my/keycast-predicate ()
  ;; Don't show keys if we're in the minibuffer
  (not (minibufferp)))

(use-package! keycast
  :commands (keycast-tab-bar-mode)
  :config
  ;; This works with doom-modeline, inspired by this comment:
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    ;; Don't
    (setq keycast-window-predicate #'my/keycast-predicate)
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update)
      (remove-hook 'pre-command-hook 'keycast--update)))

  ;; Make the key face normal-sized
  (set-face-attribute 'keycast-key nil :height 1.0)

  ;; Make sure Keycast gets added at the front of the list
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (add-to-list 'global-mode-string '("" mode-line-keycast "       ")))))

(use-package writeroom-mode
  :defer t
  :commands writeroom-mode
  :config
  (setq writeroom-global-effects (remove 'writeroom-set-fullscreen
                                         writeroom-global-effects)))

(use-package! elfeed
  :commands (elfeed)
  :config
  ;; Where to store the Elfeed database
  (setq elfeed-db-directory (expand-file-name "~/.local/share/elfeed"))
  (make-directory elfeed-db-directory t)

  ;; Update every hour automatically
  (run-at-time nil (* 60 60) #'elfeed-update)

  (use-package! elfeed-org
    :after elfeed
    :config
    (setq rmh-elfeed-org-files
          (list (expand-file-name "~/.config/doom/feeds.org")))
    (elfeed-org)))

(defvar elfeed-download-base-dir (expand-file-name "~/cloud/downloads/elfeed/")
  "Base directory for all Elfeed downloads.")

(defvar elfeed-download-youtube-dir "youtube/"
  "Subdirectory for YouTube videos (relative to base dir).")

(defvar elfeed-download-articles-dir "articles/"
  "Subdirectory for saved articles (relative to base dir).")

(defun elfeed-download--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun elfeed-download--sanitize-filename (filename)
  "Make FILENAME safe for use on the filesystem."
  (replace-regexp-in-string "[^a-zA-Z0-9-_. ]" "_" filename))

(defun elfeed-download--youtube-dir ()
  (expand-file-name elfeed-download-youtube-dir elfeed-download-base-dir))

(defun elfeed-download--articles-dir ()
  (expand-file-name elfeed-download-articles-dir elfeed-download-base-dir))

(defun elfeed-download-youtube (url title)
  "Download a YouTube video at URL with TITLE using yt-dlp."
  (let ((dir (elfeed-download--youtube-dir)))
    (elfeed-download--ensure-directory dir)
    (start-process "yt-dlp" "*yt-dlp*"
                   "yt-dlp"
                   "-o" (concat dir "%(title)s.%(ext)s")
                   url)))

(defun elfeed-download-article (url title)
  "Save article at URL with TITLE as a PDF using Node/Playwright."
  (let* ((dir (elfeed-download--articles-dir))
         (safe-title (elfeed-download--sanitize-filename title))
         (pdf-file (concat dir safe-title ".pdf")))
    (elfeed-download--ensure-directory dir)
    (start-process "playwright-pdf" "*pdf-gen*"
                   "node"
                   elfeed-download-node-script-path
                   url pdf-file)))

(defun elfeed-download-current-entry ()
  "Download the current Elfeed entry (video or article) and mark it read."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (when entries
      (let* ((entry (if (listp entries) (car entries) entries))
             (url (elfeed-entry-link entry))
             (title (elfeed-entry-title entry)))
        (if (string-match-p "youtube\\.com\\|youtu\\.be" url)
            (elfeed-download-youtube url title)
          (elfeed-download-article url title))
        (elfeed-untag entry 'unread)
        (elfeed-tag entry 'read)
        (elfeed-search-update-entry entry)))))

(defun elfeed-download-setup ()
  "Ensure directories exist and add keybindings for Elfeed downloads."
  (elfeed-download--ensure-directory elfeed-download-base-dir)
  (elfeed-download--ensure-directory (elfeed-download--youtube-dir))
  (elfeed-download--ensure-directory (elfeed-download--articles-dir))
  (when (boundp 'elfeed-search-mode-map)
    (map! :map elfeed-search-mode-map
          :n "d" #'elfeed-download-current-entry))
  (message "Elfeed download integration ready."))

(add-hook 'elfeed-search-mode-hook #'elfeed-download-setup)

(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;; EMMS full configuration with Nord theme, centered layout, and swaync notifications
(use-package! emms
  :defer t
  :commands (emms 
             emms-browser 
             emms-playlist-mode-go
             emms-pause
             emms-stop
             emms-next
             emms-previous
             emms-shuffle)
  :init
  ;; Set these early so they're available when EMMS loads
  (setq emms-source-file-default-directory "~/cloud/music"
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t
        emms-browser-default-browse-type 'artist)
  
  :config
  ;; Initialize EMMS - only runs when you actually use it
  (emms-all)
  (emms-default-players)
  (emms-mode-line-mode 1)
  (emms-playing-time-mode 1)

  ;; Basic settings
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

  ;; MPD integration - critical for your workflow
  (require 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory (expand-file-name "~/cloud/music"))

  ;; Connect to MPD and add it to player list
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  
  ;; Connect to MPD with slight delay to avoid blocking
  (run-with-timer 0.1 nil #'emms-player-mpd-connect)

  ;; Ensure players are properly set up
  (setq emms-player-list '(emms-player-mpd
                           emms-player-mplayer
                           emms-player-vlc
                           emms-player-mpg321
                           emms-player-ogg123))

  ;; Info functions
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (add-to-list 'emms-info-functions 'emms-info-tinytag)

  ;; Nord theme colors
  (custom-set-faces
   ;; Nord
   ;; '(emms-browser-artist-face ((t (:foreground "#ECEFF4" :height 1.1))))
   ;; '(emms-browser-album-face ((t (:foreground "#88C0D0" :height 1.0))))
   ;; '(emms-browser-track-face ((t (:foreground "#A3BE8C" :height 1.0))))
   ;; '(emms-playlist-track-face ((t (:foreground "#D8DEE9" :height 1.0))))
   ;; '(emms-playlist-selected-face ((t (:foreground "#BF616A" :weight bold)))))
   
   ;; Nowhere
   '(emms-browser-artist-face ((t (:foreground "#e0dcd4" :height 1.1))))   ; Parchment - most prominent
   '(emms-browser-album-face ((t (:foreground "#b4bec8" :height 1.0))))    ; Steel-blue - secondary accent
   '(emms-browser-track-face ((t (:foreground "#b4beb4" :height 1.0))))    ; Sage-green - individual tracks
   '(emms-playlist-track-face ((t (:foreground "#c0bdb8" :height 1.0))))   ; Muted foreground - neutral
   '(emms-playlist-selected-face ((t (:foreground "#ccc4b0" :weight bold))))) ; Wheat-gold - warm selection

  ;; Browser keybindings
  (define-key emms-browser-mode-map (kbd "RET") 'emms-browser-add-tracks-and-play)
  ;; (define-key emms-browser-mode-map (kbd "SPC") 'emms-pause)

  ;; Add notification hook
  (add-hook 'emms-player-started-hook 'emms-notify-song-change-with-artwork))

;; Helper functions - defined outside use-package so they're always available
(defun my/update-emms-from-mpd ()
  "Update EMMS cache from MPD and refresh browser."
  (interactive)
  (require 'emms)  ; Ensure EMMS is loaded
  (message "Updating EMMS cache from MPD...")
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "EMMS cache updated. Refreshing browser...")
  (when (get-buffer "*EMMS Browser*")
    (with-current-buffer "*EMMS Browser*"
      (emms-browser-refresh))))

(defun emms-center-buffer-in-frame ()
  "Add margins to center the EMMS buffer in the frame."
  (let* ((window-width (window-width))
         (desired-width 80)
         (margin (max 0 (/ (- window-width desired-width) 2))))
    (setq-local left-margin-width margin)
    (setq-local right-margin-width margin)
    (setq-local line-spacing 0.2)
    (set-window-buffer (selected-window) (current-buffer))))

(defun emms-cover-art-path ()
  "Return the path of the cover art for the current track."
  (when (bound-and-true-p emms-playlist-buffer)
    (let* ((track (emms-playlist-current-selected-track))
           (path (emms-track-get track 'name))
           (dir (file-name-directory path))
           (standard-files '("cover.jpg" "cover.png" "folder.jpg" "folder.png"
                             "album.jpg" "album.png" "front.jpg" "front.png"))
           (standard-cover (cl-find-if
                            (lambda (file)
                              (file-exists-p (expand-file-name file dir)))
                            standard-files)))
      (if standard-cover
          (expand-file-name standard-cover dir)
        (let ((cover-files (directory-files dir nil ".*\\(jpg\\|png\\|jpeg\\)$")))
          (when cover-files
            (expand-file-name (car cover-files) dir)))))))

(defun emms-notify-song-change-with-artwork ()
  "Send song change notification with album artwork to swaync via libnotify."
  (when (bound-and-true-p emms-playlist-buffer)
    (let* ((track (emms-playlist-current-selected-track))
           (artist (or (emms-track-get track 'info-artist) "Unknown Artist"))
           (title (or (emms-track-get track 'info-title) "Unknown Title"))
           (album (or (emms-track-get track 'info-album) "Unknown Album"))
           (cover-image (emms-cover-art-path)))
      
      (apply #'start-process
             "emms-notify" nil "notify-send"
             "-a" "EMMS"
             "-c" "music"
             (append
              (when cover-image
                (list "-i" cover-image))
              (list
               (format "Now Playing: %s" title)
               (format "Artist: %s\nAlbum: %s" artist album)))))))

(defun emms-signal-waybar-mpd-update ()
  "Signal waybar to update its MPD widget."
  (start-process "emms-signal-waybar" nil "pkill" "-RTMIN+8" "waybar"))

;; Hooks for EMMS modes - use with-eval-after-load to avoid premature loading
(with-eval-after-load 'emms-browser
  (add-hook 'emms-browser-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:background "#1a1d21"))
              (emms-center-buffer-in-frame))))

(with-eval-after-load 'emms-playlist-mode
  (add-hook 'emms-playlist-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:background "#1a1d21"))
              (emms-center-buffer-in-frame))))

;; Window resize hook - only add when EMMS is actually loaded
(with-eval-after-load 'emms
  (add-hook 'window-size-change-functions
            (lambda (_)
              (when (or (eq major-mode 'emms-browser-mode)
                        (eq major-mode 'emms-playlist-mode))
                (emms-center-buffer-in-frame)))))

(defvar site-serve-process nil)
(defvar blog-serve-process nil)

(defun my/run-dev-server (dir command &optional process-name output-buffer)
  "Run a development server in the specified directory with the given command.
DIR is the directory where the process will be run.
COMMAND is the command to execute (e.g., 'just serve').
PROCESS-NAME is an optional name for the process.
OUTPUT-BUFFER is the buffer to capture the output."
  (interactive "DDirectory: \nsCommand: ")
  (let ((default-directory dir)
        (process-name (or process-name (concat "just-" command)))
        (output-buffer (or output-buffer (concat "*" command "-output*"))))
    (setq site-serve-process
          (start-process process-name output-buffer "just" command))))

(defun blog/site-serve-dev ()
  "Run the site dev server locally."
  (interactive)
  (my/run-dev-server "/home/matthewkennedy/site/" "serve" "site-serve" "*site-serve-output*"))

(defun blog/blog-serve-dev ()
  "Run the blog dev server locally."
  (interactive)
  (my/run-dev-server "/home/matthewkennedy/site/hugo/" "test" "blog-serve" "blog-serve-output"))
