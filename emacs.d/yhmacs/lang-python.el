(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda()
			 (require 'lsp-pyright)
			 (lsp-deferred))))
(provide 'lang-python)
