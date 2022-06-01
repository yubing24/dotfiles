;; JSON mode, including formatting and some utility function.
;; To format json document, use C-c C-f. Indentation level is set to 2 by default

(defun yhou-json-mode-hook ()
  (setq tab-width 4))

(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'")

(defun format-json-on-save ()
  "Format JSON file on Save"
  (when (eq major-mode 'json-mode)
    (json-pretty-print-buffer)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(add-hook 'after-save-hook 'format-json-on-save)

(provide 'lang-json)
