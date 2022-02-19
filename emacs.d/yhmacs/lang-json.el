;; JSON mode, including formatting and some utility function.
;; To format json document, use C-c C-f. Indentation level is set to 2 by default
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(provide 'lang-json)
