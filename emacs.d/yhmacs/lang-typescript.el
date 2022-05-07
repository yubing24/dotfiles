(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(add-hook 'typescript-mode-hook (lambda () (setq tab-width 4)))

(provide 'lang-typescript)
