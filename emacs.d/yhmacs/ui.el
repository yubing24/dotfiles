;;; pakcage yhmacs/ui - configure the UI of yhmacs
;;; Mostly about 3rd party UI plugins installed for yhmacs.
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Tabs for organizing buffers
(use-package centaur-tabs
  :demand
  :ensure
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "rounded")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t) ; required if spacemacs is not used
  (setq centaur-tabs-close-button "X")
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-modified-marker "*")
  (centaur-tabs-enable-buffer-alphabetical-reordering)
  (setq centaur-tabs-adjust-buffer-order t)
  (setq centaur-tabs-enable-key-bindings t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode))

;; None-essential configurattions
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "")
  (setq dashboard-items '((recents . 10)
			  (projects . 10)
			  (agenda . 10))))

;; Theme - Doom
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-oceanic-next t)) ;; I like it using my high school blackboar color

;; Mode line (doom-theme)
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 32)
  (setq doom-modeline-bar-width 12)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-lsp t))

(provide 'ui)
