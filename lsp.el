;; LSP related setup

;; which-key
;; For key suggestions when typing key commands
(use-package which-key
    :ensure t)

;; treemacs
(use-package treemacs
    :ensure t
    :bind (("M-O" . treemacs-select-window))
    :config
    (treemacs-resize-icons 44)
    (treemacs-follow-mode)
    (treemacs-indent-guide-mode)
    (treemacs-hide-gitignored-files-mode t))

(use-package treemacs-all-the-icons
    :ensure t
    :config
    (treemacs-resize-icons 44)
    (treemacs-load-theme "all-the-icons"))

;; LSP-mode
(use-package lsp-mode
    :ensure t
    :init
    :hook ((c-mode-common . lsp)
	   (lsp-mode . lsp-enable-which-key-integration)
	   (c++-mode . lsp)
           )
    :commands lsp
    :config
    )

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

(which-key-mode)
