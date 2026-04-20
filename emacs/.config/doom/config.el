;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq default-directory "~/Documents/")
(global-auto-revert-mode 1) ;;auto-reload if changed externally
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq display-line-numbers-type t)
(setq mouse-highlight nil)

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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'normal))
(setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16 :weight 'normal))

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

;;Tab bar
(after! centaur-tabs
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar 'over
        centaur-tabs-height 28
        centaur-tabs-set-icons t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•")
  (centaur-tabs-headline-match))

;; Stale project files fix
(after! projectile
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil))

;; Tiling window fix
(after! corfu
  (setq corfu-auto t)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  (setq x-gtk-resize-child-frames 'resize-mode))

;; Text Centering
(use-package! visual-fill-column
  :hook (prog-mode . my/auto-center)
  :config
  (defun my/auto-center ()
    (when (> (frame-width) 140)
      (setq visual-fill-column-width 120
            visual-fill-column-center-text t)
      (visual-fill-column-mode 1))))

;;Project explorer
(after! treemacs
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode -1)
  (setq treemacs-width 32)

  (defun my/treemacs-auto-toggle ()
    "Open treemacs when maximized, close when small."
    (let ((wide (> (frame-width) 140)))
      (if wide
          (unless (treemacs-get-local-window)
            (treemacs-add-and-display-current-project-exclusively)
            (other-window 1))
        (when (treemacs-get-local-window)
          (treemacs-quit)))))

  (add-hook 'window-size-change-functions
            (lambda (_frame) (my/treemacs-auto-toggle)))

  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (my/treemacs-auto-toggle)
              (other-window 1))))

;;Breadcrumbs
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-symbol-numbers nil
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode))
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (and (not (bound-and-true-p lsp-mode))
                       (not (bound-and-true-p which-function-mode)))
              (setq-local header-line-format nil))))

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

