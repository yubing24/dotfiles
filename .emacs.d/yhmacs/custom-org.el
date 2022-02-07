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
   (php . t)
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

(provide 'custom-org)
