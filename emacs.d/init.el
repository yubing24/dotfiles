
;; init.el - Yubing Hou's Emacs Configuration
;;
;; Author: Yubing Hou <houyubing24@gmail.com>

;; Initialize ELPA packages
(require 'package)

;; Use Emacs native compilation. This should speed up Emacs
(setq package-native-compile t)
(setq load-prefer-newer t) ;; Call this before package initialize

;; Add additional package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) ;; MELPA
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")) ;; MELPA stable versions
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/")) ;; GNU ELPA
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; Non-GNU packages

(package-initialize)

;; Conditionally refresh package archives
(unless package-archive-contents
  (package-refresh-contents))

;; Put custom settings in its own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Make Emacs load the rest of the settings from the settings.org file.
(require 'org)
(setq org-edit-src-content-indentation 0)
(org-babel-load-file
 (expand-file-name "settings.org"
		   user-emacs-directory))
