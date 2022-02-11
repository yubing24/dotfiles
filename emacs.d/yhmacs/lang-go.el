(use-package company-go
  :ensure t
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-minimal-prefix-length 2)
  (setq company-begin-commands '(self-insert-command)))


;; go-mode related hooks
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode-hook . gofmt-before-save)
  :config
  (setq indent-tabs-mode t)
  (setq tab-width 4))

;; only use company-mode with company-go in go-mode
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(provide 'lang-go)
