;; init.el -- Yubing's Emacs configuration
;;
;; Copyright (c) 2021-2022 Yubing Hou
;;
;; Author: Yubing Hou <houyubing24@gmail.com>
;;
;; Description: personal Emacs configuration. Use this at your own risk.

;; Set customize file location. Do not load it unless it exists
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; Garbage collection threshold (for LSP)
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 MB GC threshold, make thing faster. Suggested by emacs-lsp.github.io
(setq read-process-output-max (* 5 1024 1024)) ;; 5 MB
;; Do not warn about the file size unless it exceeds this amount
(setq large-file-warning-threshold (* 50 1024 1024)) ;; 50 MB

;; Use Emacs Native
(setq package-native-compile t)
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.2.0")
;; Additional configuration for native compile
(when (featurep 'native-compile)
		;; Suppress compiler warnings
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

;; Loading newerly compiled packages
(setq load-prefer-newer t) ;; Call this before package initialize

(package-initialize) ;; initialize package list
(unless package-archive-contents ;; useful when setting up a fresh new computer. Update the package metadata if local cache is missing
  (package-refresh-contents))

;; Make sure packages are automatically compiled so that no outdated byte code is loaded
(unless (package-installed-p 'auto-compile)
  (package-install 'auto-compile))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

;; Use use-package for package management
(unless (package-installed-p 'use-package) ;; if use-package is not installed
  (package-refresh-contents) ;; update package list before install use-package
  (package-install 'use-package)) ;; install use-package
(require 'use-package)

;; Always ensure packages are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Keep packages up to date
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Font-size for each system
(defvar yhou/font-scale 100)

;; MacOS Specific Configurations - since mac are all on retina display
(if (eq system-type 'darwin)
	(progn
	  (setq insert-directory-program "gls" dired-use-ls-dired t) ;; MacOS built-in ls does not support group directory first)
	  (setq yhou/font-scale 140)))

(if (eq system-type 'gnu/linux)
	(setq yhou/font-scale 100))


;; Text Encoding
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

;; Application - temporary and private file directories
(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir))

;; Application - Binary execution paths
(setq exec-path (append exec-path '("/usr/local/bin"))
      exec-path (append exec-path '("~/go/bin")))
;; $PATH defintions
(use-package exec-path-from-shell
  :defer nil
  :config
  (exec-path-from-shell-initialize))

;; Application Behavior
(setq confirm-kill-emacs                     'y-or-n-p
      confirm-nonexistent-file-or-buffer     t
      require-final-newline                  t)

;; UI - Start-up - Disable default start-up message
(setq inhibit-startup-message t) ;; Disable start-up mesasge

;; UI - Start-up - Disable default start-up scree
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
(setq scroll-conservatively 100000)
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
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Editor - parenthesis matching
(show-paren-mode 1)

;; Editor - highlight cursor
(use-package beacon
  :init
  (beacon-mode 1))

;; Editor - indentation
(setq-default indent-line-function 'insert-tab)

;; Editor - line highlight
(global-hl-line-mode +1)

;; Editor - window navigation
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Editor - automatically revert bufer when files are changed by other programs
(global-auto-revert-mode t)

(use-package which-key
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
  :ensure nil ;; use-package doesn't always find it
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

;; Programming language modes
(use-package ng2-mode) ;; Angular 2+
(use-package csharp-mode
  :mode "\\.cs\\'") ;; C#
(use-package dart-mode)

;; Docker
(use-package dockerfile-mode)
(use-package docker)
(use-package docker-compose-mode)
(use-package lsp-docker)

(use-package git-modes)
;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode-hook . gofmt-before-save))
(use-package company-go)

;; GraphQL
(use-package graphql-mode
  :mode "\\.graphql\\'")

;; JavaScript
(use-package prettier-js
  :hook (js2-mode-hook prettier-js-mode)
  :hook (web-mode-hook prettier-js-mode)
  :hook (typescript-mode-hook prettier-js-mode)) ;; JS Code formatter
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode-hook . prettier-js-mode))
(use-package add-node-modules-path
  :hook (flycheck-mode-hook . add-node-modules-path)) ;; Add node_modules to exec path.


;; JSON
(use-package json-mode
  :mode "\\.json\\'")

(use-package rjsx-mode) ;; JSX

;; LaTeX
(use-package latex-preview-pane)
(use-package company-math)
(use-package latex-pretty-symbols)

;; Lisp
(use-package lisp-mode
  :ensure nil)

(use-package markdown-mode)

;; PHP
(use-package php-mode
  :mode "\\.php\\'")
(use-package company-php)

;; Python
(use-package python-mode)
(use-package lsp-python-ms) ;; LSP-mode client for Microsoft Python

(use-package rust-mode)

;; SASS Stylesheet
(use-package sass-mode
  :mode "\\.sass\\'")

;; SCSS Stylesheet
(use-package scss-mode
  :mode "\\.scss\\'")

;; SQL
(use-package sqlformat)

;; TypeScript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode-hook . prettier-js-mode)
  :hook (typescript-mode-hook . (lambda () (setq tab-width 2))))

;; Web development general settings
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :hook (web-mode-hook . prettier-js-mode))
(use-package company-web)

(use-package yaml-mode)

;; Code Editing -- format
(defun prettify-ecmascript-on-save ()
  "Format ECMAScript (JS/TS) using Prettier upon saving files."
  (when (or (eq major-mode 'js2-mode) (eq major-mode 'typescript-mode))
    (prettier-js)))
(add-hook 'after-save-hook 'prettify-ecmascript-on-save)

;; ace-window: makes window switching easy
(use-package ace-window
  :diminish
  :init
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package ag
  :diminish
  :config
  (setq ag-highlight-search t))

;; Company - auto-completion
(use-package company
  :diminish
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)) ;; do NOT hint until 2 characters

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Provide additional documation when Ivy is triggered
(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)))

(use-package flycheck
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
  :config
  (ivy-mode 1) ;; ensure ivy always runs
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d ")
  :bind(("C-s" . swiper-isearch)))

;; Additional helpful information when ivy is triggered
(use-package ivy-rich
  :diminish
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package lsp-mode
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
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)) ;; setting to true will have performance tax

(use-package lsp-ivy)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position "top")
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-show-with-cursor t))

;; Git UI
(use-package magit)

;; Text folding
(use-package s)
(use-package dash)
(use-package origami
  :init
  (global-origami-mode t))

(use-package projectile
  :diminish
  :init
  (projectile-mode +1)
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind(("C-c p" . projectile-command-map))
  :custom((projectile-completion-system 'ivy)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; File browsing
(use-package treemacs)

(use-package yasnippet
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
(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Tabs for organizing buffers
(use-package centaur-tabs
  :demand
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
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-oceanic-next t)) ;; I like it using my high school blackboar color

;; Mode line (doom-theme)
(use-package doom-modeline
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yhmacs/org-custom-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 0))

;; Hitting the RETURN button will follow links in org-mode files
(setq org-return-follows-link t)

(defun yhmacs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.4)
                  (org-level-4 . 1.3)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :pin elpa
  :ensure t
  :hook (org-mode . yhmacs/org-custom-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-confirm-babel-evaluate nil)
  (yhmacs/org-font-setup))

(use-package org-bullets
  :pin melpa
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-babel-python-command "python3")

(defun yhmacs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; copy the previous code block in org mode to avoid typing
(defun yhmacs/org-babel-copy-previous-src-block ()
  "Copy previous src block excluding the content."
  (interactive)
  (let (result)
    (save-excursion
      (org-babel-previous-src-block)
      (let ((element (org-element-at-point)))
        (when (eq (car element) 'src-block)
          (let* ((pl (cadr element))
                 (lang (plist-get pl :language))
                 (switches (plist-get pl :switches))
                 (parms (plist-get pl :parameters)))
            (setq result
                  (format
                   (concat "#+begin_src %s\n"
                           "\n"
                           "#+end_src\n")
                   (mapconcat #'identity
                              (delq nil (list lang switches parms))
                              " ")))))))
    (and result (insert result))
    (previous-line 2)))

(use-package visual-fill-column
  :pin melpa
  :ensure t
  :hook (org-mode . yhmacs/org-mode-visual-fill))

(use-package ob-async
  :ensure t
  :pin melpa)

(use-package org-contrib
  :ensure t
  :pin nongnu)

(use-package ob-http
  :ensure t
  :pin melpa)

(use-package ob-go
  :ensure t
  :pin melpa)

;; Add language-support to list
(org-babel-do-load-languages
 'org-babel-load-languages
 '((http . t)
   (python . t)
;;   (php . t)
   (awk . t)
   (shell . t)
   (C . t)
   (css . t)
   (emacs-lisp . t)
   (sed . t)
   (java . t)
   (latex . t)
   (makefile . t)
   (js . t)
   (org . t)
   (sql . t)
   (sqlite . t)))

;; add templates to use for org mode
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("java" . "src java"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("php" . "src php"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/OrgRoam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(setq find-file-visit-truename t) ;; Make Emacs always resolve symbolic link, this slows down performance
(org-roam-db-autosync-mode) ;; Make Org-roam to cache changes in real time. This makes org-roam available on startup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; JSON mode, including formatting and some utility function.
;; To format json document, use C-c C-f. Indentation level is set to 2 by default

(defun yhou-json-mode-hook ()
  (setq tab-width 2))


(defun format-json-on-save ()
  "Format JSON file on Save"
  (when (eq major-mode 'json-mode)
    (json-pretty-print-buffer)))

(add-hook 'after-save-hook 'format-json-on-save)



;; package management setup
;; if use-package is not installed, install use-package. Useful on non-Linux platform
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environments
(use-package no-littering)
(unless (file-exists-p "~/.emacs.d/auto-save")
  (make-directory "~/.emacs.d/auto-save") :parents)
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto-save/" t)))

(setq exec-path (append exec-path '("~/go/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; [Editing] Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; [Editing] Always delete the selected text with a key stroke, otherwise I'll have to press backspace
;; to delete selected text
(delete-selection-mode t)

;; [Editing] make table always indent or complete
(setq tab-always-indent 'complete)

;; save recent files
(use-package recentf
  :config
  (setq recentf-save-file
	(recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))


;; Key bindings

(global-set-key (kbd "C-S-r") 'revert-buffer)

;; Editing
(define-prefix-command 'edit-key-map)
(global-set-key (kbd "C-c e") 'edit-key-map)

;; Searching
(define-prefix-command 'search-key-map)
(global-set-key (kbd "C-c s") 'search-key-map)
(define-key 'search-key-map (kbd "f") 'ag-project)

;; Viewing
(define-prefix-command 'view-key-map)
(global-set-key (kbd "C-c v") 'view-key-map)
(define-key 'view-key-map (kbd "o n") 'origami-open-node)
(define-key 'view-key-map (kbd "c n") 'origami-close-node)
(define-key 'view-key-map (kbd "O n") 'origami-open-all-nodes)
(define-key 'view-key-map (kbd "C n") 'origami-close-all-nodes)

;; Coding
(define-prefix-command 'code-key-map)
(global-set-key (kbd "C-c c") 'code-key-map)
(define-key 'code-key-map (kbd "f d") 'lsp-find-definition)
(define-key 'code-key-map (kbd "f i") 'lsp-find-implementation)
(define-key 'code-key-map (kbd "f r") 'lsp-find-references)
(define-key 'code-key-map (kbd "f b") 'lsp-format-buffer)

;; Window/Buffer-managing
(define-prefix-command 'window-key-map)
(global-set-key (kbd "C-c w") 'window-key-map)
