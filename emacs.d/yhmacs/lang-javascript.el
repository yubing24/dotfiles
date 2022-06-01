(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(defun prettify-js-on-save ()
  "Format JavaScript files with Prettier on Save"
  (when (eq major-mode 'js2-mode)
    (prettier-js)))

(add-hook 'after-save-hook 'prettify-js-on-save)
(provide 'lang-javascript)
