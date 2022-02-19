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
                        ("melpa-stable" . "https://stable.melpa.org/packages/")
                        ("elpa" . "https://elpa.gnu.org/packages/")
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

;; my customizations
(add-to-list 'load-path "~/.emacs.d/yhmacs/")

(require 'defaults) ;; base configuration for entire Emacs
(require 'dev) ;; common configuration for all code-writing task
(require 'ui)
(require 'custom-org)
(require 'custom-docker)
(require 'lang-go)
(require 'lang-javascript)
(require 'lang-json)
(require 'lang-php)
(require 'lang-python)
(require 'lang-sql)
(require 'lang-tex)
(require 'lang-typescript)
(require 'lang-yaml)
(require 'lang-web)

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

(setq custom-file "~/.emacs.d/custom.el")
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
