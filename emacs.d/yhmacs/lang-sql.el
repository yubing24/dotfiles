(use-package sql
  :ensure t)

;; SQL
(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path "~/dotfiles/sql-workspace/")


(use-package sqlformat
  :ensure t)

; SQL mode configuration
(provide 'lang-sql)
