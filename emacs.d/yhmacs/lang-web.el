(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

;; enable syntax highlighting in .js/.jsx files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; set up indentation to 4 in web-mode
(setq tab-width 4)

;; add node_module path
(use-package add-node-modules-path
  :ensure t)
;; use node_modules for syntax check
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;; prettier js
(use-package prettier-js
  :ensure t
  :hook (js2-mode-hook prettier-js-mode)
  :hook (web-mode-hook prettier-js-mode))

(provide 'lang-web)
