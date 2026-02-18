;;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion
 (corfu +icons)    ; Completion backend
 (vertico +icons)  ; the search engine of the future

 :ui
 doom                ; what makes DOOM look the way it does
 doom-dashboard      ; a nifty splash screen for Emacs
 doom-quit           ; DOOM quit-message prompts when you quit Emacs
 (emoji +unicode)    ; 🙂
 hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 ;;indent-guides       ; highlighted indent columns
 (ligatures +fira)   ; ligatures and symbols to make your code pretty again
 ;;minimap             ; show a map of the code on the side
 modeline            ; snazzy, Atom-inspired modeline, plus API
 ophints             ; highlight the region an operation acts on
 (popup +defaults)   ; tame sudden yet inevitable temporary windows
 tabs                ; Tab bar
 (treemacs +lsp)     ; a project drawer, like neotree but cooler
 vi-tilde-fringe     ; fringe tildes to mark beyond EOB
 ;;workspaces          ; tab emulation, persistence & separate workspaces
 (vc-gutter +pretty) ; vcs diff in the fringe

 :editor
 (evil +everywhere); come to the dark side, we have cookies
 file-templates    ; auto-snippets for empty files
 fold              ; (nigh) universal code folding
 (format +lsp)     ; automated prettiness
 snippets          ; my elves. They type so I don't have to

 :emacs
 (dired +icons)    ; making dired pretty [functional]
 electric          ; smarter, keyword-based electric-indent
 (ibuffer +icons)  ; edit me like one of your French buffers
 undo              ; persistent, smarter undo for your inevitable mistakes
 vc                ; version-control and Emacs, sitting in a tree

 :term
 vterm             ; the best terminal emulation in Emacs

 :checkers
 syntax            ; tasing you for every semicolon you forget
 (spell +aspell)   ; tasing you for misspelling mispelling

 :tools
 (debugger +lsp)   ; step through code to help you add bugs
 lookup            ; navigate your code and its documentation
 magit             ; a git porcelain for Emacs
 make              ; run make tasks from Emacs
 pdf               ; pdf enhancements
 editorconfig
 tree-sitter
 lsp (+peek)

 :os
 (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS

 :lang
 emacs-lisp                     ; drown in parentheses
 (sh +lsp)                      ; she sells {ba,z,fi}sh shells on the C xor
 data                           ; config/data formats (XML/CSV)
 (json +lsp)                    ; At least it ain't XML
 yaml                           ; JSON, but readable
 latex                          ; writing papers in Emacs has never been so fun
 markdown                       ; writing docs for people to ignore
 (org +roam +dragndrop +pretty +pandoc +present +gnuplot) ; organize your plain life in plain text
 (cc +lsp +tree-sitter)         ; C > C++ == 1
 (nix +lsp +tree-sitter)        ; I hereby declare "nix geht mehr!"
 ;;(rust +lsp)                    ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 ;;(go +lsp)                      ; the hipster dialect
 ;;(csharp +lsp)                  ; unity, .NET, and mono shenanigans
 ;;(java +lsp)                    ; the poster child for carpal tunnel syndrome
 ;;(kotlin +lsp +tree-sitter)     ; a better, slicker Java(Script)
 ;;(web +lsp)                     ; the tubes
 ;;(javascript +lsp +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
 ;;(lua +lsp +tree-sitter)        ; one-based indices? one-based indices
 ;;(python +lsp +tree-sitter)     ; beautiful is better than ugly
 ;;(gdscript +lsp +tree-sitter)   ; the language you waited for
 ;;(scheme +guile)                ; a fully conniving family of lisps

 :app
 irc                            ; how neckbeards socialize
 (rss +org +youtube)            ; emacs as an RSS reader

 :config
 ;;literate
 (default +bindings +smartparens))

