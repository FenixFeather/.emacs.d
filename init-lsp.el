;; LSP related setup

;; which-key
;; For key suggestions when typing key commands
(use-package which-key
    :hook ((after-init . which-key-mode))
    :bind (("C-h M" . which-key-show-keymap))
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

(use-package all-the-icons-ivy
    :ensure t
    :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package treemacs-all-the-icons
    :ensure t
    :config
    (treemacs-resize-icons 44)
    (treemacs-load-theme "all-the-icons"))

(use-package frog-jump-buffer
    :ensure t
    :bind (("C-x C-b" . frog-jump-buffer)
           ("C-x b" . (lambda ()
                        (interactive)
                        (let ((frog-jump-buffer-default-filter
                               'frog-jump-buffer-filter-all))
                          (frog-jump-buffer))))
           ("C-<tab>" . frog-jump-buffer)
           ("C-x k" . (lambda ()
                        (interactive)
                        (kill-buffer (current-buffer)))))
    :config
    (setq frog-jump-buffer-use-all-the-icons-ivy t)
    (setq frog-jump-buffer-include-current-buffer nil)
    (setq frog-jump-buffer-default-filter
          'frog-jump-buffer-filter-file-buffers))

;; LSP-mode
(use-package lsp-mode
    :ensure t
    :init
    :hook ((c-mode-common . lsp)
           (lsp-mode . lsp-enable-which-key-integration)
           (c++-mode . lsp))
    :commands lsp
    :config)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; Java
(use-package lsp-java
    :ensure t
    :config (add-hook 'java-mode-hook #'lsp))

(provide 'init-lsp)
