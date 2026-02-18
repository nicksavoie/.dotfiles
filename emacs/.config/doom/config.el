;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq default-directory "~/Documents/")
(global-auto-revert-mode 1) ;;auto-reload if changed externally
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq display-line-numbers-type t)
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'normal))
(setq doom-variable-pitch-font (font-spec :family "Roboto" :size 14 :weight 'normal))
(custom-set-faces '(org-link ((t (:underline nil)))))
(setq scroll-conservatively 10
      scroll-margin 15
      pixel-scroll-precision-mode t)

;;Keybinds
(cua-mode t) ;;Copy/Cut/Paste
(setq cua-enable-cua-keys t
      cua-keep-region-after-copy t)
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)
(setq-default shift-select-mode t) ;;Shift+arrows select
(setq-default delete-selection-mode t) ;;Typing replaces selection
(map! :nvi
      "C-a" #'mark-whole-buffer
      "C-s" #'save-buffer
      "C-f" #'consult-line
      "C-S-z" #'undo-redo
      "C-S-v" #'evil-visual-block
      "M-<up>" #'drag-stuff-up
      "M-<down>" #'drag-stuff-down)

(use-package! drag-stuff
  :config
  (drag-stuff-global-mode 1))

;; Make shift+arrows work for selection even in evil
(define-key evil-insert-state-map (kbd "S-<left>") nil)
(define-key evil-insert-state-map (kbd "S-<right>") nil)
(define-key evil-insert-state-map (kbd "S-<up>") nil)
(define-key evil-insert-state-map (kbd "S-<down>") nil)

(define-key evil-normal-state-map (kbd "S-<left>") nil)
(define-key evil-normal-state-map (kbd "S-<right>") nil)
(define-key evil-normal-state-map (kbd "S-<up>") nil)
(define-key evil-normal-state-map (kbd "S-<down>") nil)

;;Theme
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'macchiato) ; 'frappe, latte, macchiato, or mocha
(defun my/set-catppuccin (flavor)
  (interactive
   (list (intern
          (completing-read
           "Catppuccin flavor: "
           '(latte frappe macchiato mocha)))))
  (setq catppuccin-flavor flavor)
  (load-theme 'catppuccin t))
(map! :leader
      :desc "Set Catppuccin flavor"
      "t C" #'my/set-catppuccin)


;;Tab bar
(after! centaur-tabs
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 28
        centaur-tabs-set-bar 'under
        centaur-tabs-set-close-button nil
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "●"
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-set-icons t)
  (setq centaur-tabs-buffer-list-function
        #'centaur-tabs-buffer-list)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-headline-match)

  ;; Clean up faces (modern flat look)
  (custom-set-faces!
    '(centaur-tabs-selected
      ((t (:inherit default :weight bold))))
    '(centaur-tabs-unselected
      ((t (:inherit default :foreground "#777777"))))
    '(centaur-tabs-selected-modified
      ((t (:inherit centaur-tabs-selected))))
    '(centaur-tabs-unselected-modified
      ((t (:inherit centaur-tabs-unselected))))))

;;Project explorer
(after! treemacs
  ;; --- Behavior ---
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode -1)

  ;; Don't let treemacs steal focus
  (treemacs-is-never-other-window t)

  ;; Fixed width like VSCode
  (setq treemacs-width 32)

  ;; Always open on project switch (but don't select it)
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (treemacs-add-and-display-current-project-exclusively)
              (other-window 1))))

;;Breadcrumbs
(setq lsp-headerline-breadcrumb-enable t)
(setq which-func-unknown "⊘")
(setq-default header-line-format
              '(:eval
                (when (bound-and-true-p which-function-mode)
                  (format "  %s"
                          (or (which-function) "")))))

;;Nix
(after! nix-mode
  (setq nix-nixfmt-bin "alejandra"))

;;ORG ROAM
(setq org-directory "~/Documents/Notes/")
(setq org-roam-directory "~/Documents/Notes")
(setq org-roam-dailies-directory "~/Documents/Notes/Journal")
(setq org-agenda-files '("~/Documents/Notes/Journal"))
(setq org-roam-completion-everywhere t)
(setq org-startup-with-inline-images t
      org-image-actual-width '(300))

(setq org-roam-capture-templates
       '(("f" "Fleeting" plain (file "./Roaming/Fleeting.org")
           :if-new (file "./Roaming/${title}.org")
           :immediate-finish t
           :jump-to-captured t
           :unnarrowed f)
         ("r" "Reference" plain (file "./References/Reference.org")
           :if-new (file "./References/${title}.org")
           :immediate-finish t
           :jump-to-captured t
           :unnarrowed f)
         ("m" "Map of Content" plain (file "./Concepts/MOP.org")
           :if-new (file "./Concepts/${title}.org")
           :immediate-finish t
           :jump-to-captured t
           :unnarrowed f)
        ))

(setq org-roam-dailies-capture-templates
       '(("j" "Journal" plain " %?"
          :target (file+datetree "%<%Y>.org" day)
          :immediate-finish t
          :jump-to-captured f
          :unnarrowed t)
         ("d" "Dream" entry "%<%d-%b-%Y>\n%?"
          :if-new (file+datetree "Dreams.org" day)
          :immediate-finish t
          :jump-to-captured t
          :unnarrowed f)
        ))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun my/org-wikipedia-link ()
  (interactive)
  (let* ((word (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'word t))))
    (unless word
      (setq word (read-string "Enter word: ")))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert (format "[[https://en.wikipedia.org/wiki/%s][Wikipedia]]"
                    (replace-regexp-in-string " " "_" word)))))

