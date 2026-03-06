;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons +dirvish)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       eww
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;; (spell) ; tasing you for misspelling mispelling
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       biblio            ; Writes a PhD for you (citation needed)
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
       ;; editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +dictionary +offline +docsets)              ; navigate your code and its documentation
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pass              ; password manager for nerds
       pdf               ; pdf enhancements
       rgb               ; creating color strings
       tree-sitter       ; syntax and parsing, sitting in a tree...
       upload            ; map local to remote projects via ssh/ftp

       :os
       tty               ; improve the terminal Emacs experience

       :lang
       (cc +lsp +treesitter)         ; C > C++ == 1
       clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       elixir            ; erlang done right
       (elm +lsp +treesitter)               ; care for a cup of TEA?
       (emacs-lisp +lsp +treesitter)        ; drown in parentheses
       erlang            ; an elegant language for a more civilized age
       fsharp            ; ML stands for Microsoft's Language
       gdscript          ; the language you waited for
       (go +tree-sitter +lsp)           ; the hipster dialect
       (graphql +lsp)    ; Give queries a REST
       (haskell +lsp +treesitter)    ; a language that's lazier than I am
       json              ; At least it ain't XML
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       (latex +tree-sitter)             ; writing papers in Emacs has never been so fun
       ledger            ; be audit you can be
       (lua +lsp +treesitter)               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       nix               ; I hereby declare "nix geht mehr!"
       ocaml             ; an objective camel
       odin
       (org                ; organize your plain life in plain text
        +pretty
        +journal
        +hugo
        +noter
        +dragndrop
        +pandoc
        +present)
       php               ; perl's insecure younger brother
       python            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       (ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala             ; java, but good
       (scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       templ             ; no running allowed
       web               ; the tubes
       yaml              ; JSON, but readable
       (zig +lsp)               ; C, but simpler

       :email
       (mu4e +org +gmail)

       :app
       calendar
       emms
       (everywhere +protocol)       ; leave emacs? never.
       irc               ; how neckbeards socialize
       (rss +org +web +search)        ; emacs as an RSS reader

       :config
       literate
       (default +bindings +smartparens))

