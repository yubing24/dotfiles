;; Garbage collection threshold
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1000 1000)) ;; 1 MB GC threshold, make thing faster
(setq read-process-output-max (* 128 1024 1024)) ;; 128 Mbit

;; Use Emacs Native
(setq package-native-compile t)
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.2.0")
;; Additional configuration for native compile
(when (featurep 'native-compile)
		;; Suppres compiler warnings
		(setq native-compile-async-report-warnings-errors nil)
		;; Make compilation async
		(setq native-comp-deferred-compilation t)
		;; Set directory to store compilation cache
		(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Set up packaging system
(require 'package)

;; Package Management - sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("elpa" . "https://elpa.gnu.org/packages/")
                        ("melpa-stable" . "https://stable.melpa.org/packages/")
                        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;; [note] some packages require melpa-stable, like lsp-ui

;; This line may help with the freezing issue in Mac
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize) ;; initialize package list
(unless package-archive-contents ;; useful when setting up a fresh new computer
  (package-refresh-contents))

;; Use use-package for package management
(unless (package-installed-p 'use-package) ;; if use-package is not installed
  (package-refresh-contents) ;; update package list before install use-package
  (package-install 'use-package)) ;; install use-package
(require 'use-package)
(setq use-package-alaways-ensure t)

;; command action logging
(use-package command-log-mode
  :ensure t)

;; Font-size for each system
(defvar yhou/font-scale 100)

;; MacOS Specific Configurations - since mac are all on retina display
(if (eq system-type 'darwin)
	(progn
	  (setq insert-directory-program "gls" dired-use-ls-dired t) ;; MacOS built-in ls does not support group directory first)
	  (setq yhou/font-scale 140)))

(if (eq system-type 'gnu/linux)
	(setq yhou/font-scale 100))


;; System
;; Fundamentals - Text Encoding
(set-charset-priority        'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; System
;; Window management
(winner-mode 1)

;; Application - Configuration File Locations
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

;; Application - temporary and private file directories
(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir))

;; Application - Binary execution paths
(setq exec-path (append exec-path '("/usr/local/bin"))
      exec-path (append exec-path '("~/go/bin")))
;; $PATH defintions
(use-package exec-path-from-shell
  :ensure t
  :defer nil
  :config
  (exec-path-from-shell-initialize))

;; Application Behavior
(setq confirm-kill-emacs                     'y-or-n-p
      confirm-nonexistent-file-or-buffer     t
      require-final-newline                  t)

;; UI - Disable default start-up message
(setq inhibit-startup-message t) ;; Disable start-up mesasge

;; UI - Disable default start-up scree
(setq inhibit-startup-screen t)

;; UI - Disable menu bar
(menu-bar-mode -1)

;; UI - Disable tool bar
(tool-bar-mode -1)

;; UI - Disable scroll bar
(scroll-bar-mode -1)

;; UI - Make scroling great again!
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)

;; UI - Disable tool tips
(tooltip-mode -1)

;; UI - Enable visible bell
(setq visible-bell t)

;; UI -  Maximize frame on every start up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable Menu Bar
(menu-bar-mode -1)

;; Editor frindge - some margin around text area
(set-fringe-mode 16)

;; Editor - font
(set-face-attribute 'default nil :font "JetBrains Mono" :height yhou/font-scale)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height yhou/font-scale)
(set-face-attribute 'variable-pitch nil :font "Ubuntu" :weight 'medium :height yhou/font-scale)

(setq inhibit-compacting-font-caches t) ;; do not compact font caches during GC

;; Editor - line mode and exceptions
(global-display-line-numbers-mode t) ;; show line number globally
(dolist (mode '(org-mode-hook ;; exceptions where line number shall not show
                term-mode-hook
				sql-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Editor - parenthesis matching
(show-paren-mode 1)

;; Editor - highlight cursor
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

;; Editor - indentation
(setq-default indent-line-function 'insert-tab)

;; Editor - line highlight
(global-hl-line-mode t)

;; Editor - window navigation
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3))

;; Mode bar - show column number
(column-number-mode 1)

;; Prompt - ESC to leave
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Prompt - Use Y or N
(fset 'yes-or-no-p 'y-or-n-p)

;; dired - show less when looking up files, and group directories first
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  ((dired-listing-switches "-agho --group-directories-first")))

;; Replace default emacs help
;; (use-package helpful
;;   :ensure t
;;   :pin melpa-stable)
;; ;; Replace default help keybindings to point to the new helpful
;; (global-set-key (kbd "C-h f") #'helpful-callable)
;; (global-set-key (kbd "C-h v") #'helpful-variable)
;; (global-set-key (kbd "C-h k") #'helpful-key)
;; ;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; ;; for this in lisp modes.
;; (global-set-key (kbd "C-c C-d") #'helpful-at-point)
;; ;; Look up *F*unctions (excludes macros).
;; ;;
;; ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; ;; already links to the manual, if a function is referenced there.
;; (global-set-key (kbd "C-h F") #'helpful-function)
;; ;; Look up *C*ommands.
;; ;;
;; ;; By default, C-h C is bound to describe `describe-coding-system'. I
;; ;; don't find this very useful, but it's frequently useful to only
;; ;; look at interactive functions.
;; (global-set-key (kbd "C-h C") #'helpful-command)
;; ;; This works well with ivy
;; (setq counsel-describe-function-function #'helpful-callable)
;; (setq counsel-describe-variable-function #'helpful-variable)

;; Notifications
(setq ring-bell-function 'ignore) ;; turn off sound notification

;; Bookmark
(setq bookmark-save-flag t ;; persistent bookmarks
      bookmark-default-file (concat temporary-file-directory "/bookmarks"))

;; File backups
(setq history-length            1000
      backup-inhibited          nil
      make-backup-files         t
      auto-save-default         t
      auto-save-list-file-name  (concat temp-dir "/autosave")
      create-lockfiles          nil
      backup-directory-alist    `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))
(unless (file-exists-p (concat temp-dir "/auto-save-list"))
		       (make-directory (concat temp-dir "/auto-save-list") :parents))

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
  (setq ag-highlight-search t))

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
  :hook (json-mode . lsp-deferred)
  :hook (js2-mode . lsp-deferred)
  :hook (php-mode . lsp-deferred)
  :hook (python-mode . lsp-deferred)
  :hook (sql-mode . lsp-deferred)
  :hook (tex-mode . lsp-deferred)
  :hook (typescript-mode . lsp-deferred)
  :hook (web-mode . lsp-deferred)
  :config
  (setq lsp-idle-delay 0.5))

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

;; Text folding
(use-package s
  :ensure t)
(use-package dash
  :ensure t)
(use-package origami
  :ensure t
  :init
  (global-origami-mode t))

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

;; Language specifics
(setq-default js2-basic-offset 2
	      js-indent-level 2)

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


;; my customizations
(add-to-list 'load-path "~/dotfiles/emacs.d/yhmacs/")

;;(require 'custom-org)
;; (require 'custom-docker)
(require 'lang-go)
(require 'lang-javascript)

;; JSON mode, including formatting and some utility function.
;; To format json document, use C-c C-f. Indentation level is set to 2 by default

(defun yhou-json-mode-hook ()
  (setq tab-width 2))

(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'")

(defun format-json-on-save ()
  "Format JSON file on Save"
  (when (eq major-mode 'json-mode)
    (json-pretty-print-buffer)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(add-hook 'after-save-hook 'format-json-on-save)

(require 'lang-php)
(require 'lang-python)
(require 'lang-sql)
(require 'lang-tex)
(require 'lang-typescript)
(require 'lang-yaml)
(require 'lang-web)
(require 'key)
(require 'custom-org)

;; package management setup
;; if use-package is not installed, install use-package. Useful on non-Linux platform
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environments
(use-package no-littering
  :ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq custom-file "~/dotfiles/emacs.d/custom.el")
(load custom-file)
(setq exec-path (append exec-path '("~/go/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; [Editing]  Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; save recent files
(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file
	(recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

;; Garbage collection hack, may help emacs running faster
(use-package gcmh
  :diminish gcmh-mode
  :ensure t)
