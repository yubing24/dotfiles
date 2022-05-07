;; JSON mode, including formatting and some utility function.
;; To format json document, use C-c C-f. Indentation level is set to 2 by default

(defun yhou-json-mode-hook ()
  (setq tab-width 4))

(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(provide 'lang-json)
