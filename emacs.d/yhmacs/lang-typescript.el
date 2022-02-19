(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(add-hook 'typescript-mode-hook (lambda () (setq tab-width 4)))

(provide 'lang-typescript)
