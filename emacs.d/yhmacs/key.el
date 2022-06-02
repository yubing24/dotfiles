;; Personal Key Maps (with prefix) to group the use of keys

(global-set-key (kbd "C-S-r") 'revert-buffer)

;; Editing
(define-prefix-command 'edit-key-map)
(global-set-key (kbd "C-c e") 'edit-key-map)
;; (define-key 'edit-key-map (kbd "d") 'crux-duplicate-current-line-or-region) ;; Duplicate selected text
;; (define-key 'edit-key-map (kbd "i") 'crux-smart-open-line) ;; Add a new empty line below
;; (define-key 'edit-key-map (kbd "u") 'crux-upcase-region) ;; make region upper case
;; (define-key 'edit-key-map (kbd "l") 'crux-downcase-region) ;; make region lower case

;; Searching
(define-prefix-command 'search-key-map)
(global-set-key (kbd "C-c s") 'search-key-map)
(define-key 'search-key-map (kbd "f") 'ag-project)

;; Viewing
(define-prefix-command 'view-key-map)
(global-set-key (kbd "C-c v") 'view-key-map)
(define-key 'view-key-map (kbd "o n") 'origami-open-node)
(define-key 'view-key-map (kbd "c n") 'origami-close-node)
(define-key 'view-key-map (kbd "O n") 'origami-open-all-nodes)
(define-key 'view-key-map (kbd "C n") 'origami-close-all-nodes)

;; Coding
(define-prefix-command 'code-key-map)
(global-set-key (kbd "C-c c") 'code-key-map)
(define-key 'code-key-map (kbd "f d") 'lsp-find-definition)
(define-key 'code-key-map (kbd "f i") 'lsp-find-implementation)
(define-key 'code-key-map (kbd "f r") 'lsp-find-references)
(define-key 'code-key-map (kbd "f b") 'lsp-format-buffer)

;; Window/Buffer-managing
(define-prefix-command 'window-key-map)
(global-set-key (kbd "C-c w") 'window-key-map)
;; (define-key 'window-key-map (kbd "s") 'crux-transpose-windows)

(provide 'key)
