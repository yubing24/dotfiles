(defun yhou-json-mode-hook ()
  (setq tab-width 4))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :hook(json-mode-hook 'yhou-json-mode-hook))


(provide 'lang-json)
