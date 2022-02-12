;; ace-window: makes window switching easy
(use-package ace-window
  :diminish
  :init
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package ag
  :diminish
  :ensure t
  :config
  (setq ag-highlight-search t)
  :bind (("C-x C-S-f" . ag-project)
		 ("C-x C-S-r" . ag-project-regexp)))

;; Company - auto-completion
(use-package company
  :diminish
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)) ;; do NOT hint until 2 characters

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Provide additional documation when Ivy is triggered
(use-package counsel
  :diminish
  :ensure t
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))
;; disable default flycheck jslint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
;; use a global eslint
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; use a global eslint for flycheck in web-mode
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package ivy
  :diminish
  :ensure t
  :config
  (ivy-mode 1) ;; ensure ivy always runs
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d ")
  :bind(("C-s" . swiper-isearch)))

;; Additional helpful information when ivy is triggered
(use-package ivy-rich
  :diminish
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :hook (js2-mode . lsp-deferred)
  :hook (json-mode . lsp-deferred)
  :hook (php-mode . lsp-deferred)
  :hook (python-mode . lsp-deferred)
  :hook (tex-mode . lsp-deferred)
  :hook (typescript-mode . lsp-deferred)
  :hook (web-mode . lsp-deferred)
  :config
  (setq lsp-idle-delay 0.5)
  :bind(("C-c C-f C-d" . lsp-find-definition)
		("C-c C-f C-r" . lsp-find-references)
		("C-c C-f C-i" . lsp-find-implementation)
		("C-c C-f C-b" . lsp-format-buffer)))

(use-package lsp-ivy
  :ensure t)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position "top")
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-show-with-cursor t))

;; Git UI
(use-package magit
  :ensure t)

(use-package powerline
  :ensure t)

(use-package projectile
  :diminish
  :ensure t
  :init
  (projectile-mode +1)
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind(("C-c p" . projectile-command-map))
  :custom((projectile-completion-system 'ivy)))

(use-package rainbow-delimiters
  :diminish
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treemacs
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'load-path "~/.emacs.d/yasnippet"))

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(provide 'dev)
