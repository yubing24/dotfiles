(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . prettier-js)))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(add-hook 'typescript-mode-hook (lambda () (setq tab-width 2)))

(provide 'lang-typescript)
