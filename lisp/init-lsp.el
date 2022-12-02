;; LSP related setup

;; which-key
;; For key suggestions when typing key commands
(setq debug-on-error t)
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
    (setq treemacs-default-visit-action 'treemacs-visit-node-in-most-recently-used-window)
    (setq treemacs-collapse-dirs 10)
    (treemacs-hide-gitignored-files-mode t))

(use-package all-the-icons-ivy
    :ensure t
    :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package treemacs-all-the-icons
    :ensure t
    :config
    (treemacs-resize-icons 44)
    (treemacs-load-theme "all-the-icons"))

(use-package solaire-mode
    :ensure t
    :hook ((after-init . solaire-global-mode)))

(use-package perspective
    :ensure t
    :hook ((kill-emacs . persp-state-save))
    :config
    (persp-mode)
    (setq persp-sort 'created)
    (setq persp-state-default-file "~/.emacs.d/.cache/persp-persist"))

(use-package perspective-treemacs
    :after (treemacs perspective))

(use-package frog-jump-buffer
    :ensure t
    :bind (("C-x C-b" . frog-jump-buffer)
           ("C-<tab>" . frog-jump-buffer)
           ("C-x k" . (lambda ()
                        (interactive)
                        (kill-buffer (current-buffer)))))
    :config
    (setq frog-jump-buffer-use-all-the-icons-ivy t)
    (setq frog-jump-buffer-include-current-buffer nil)
    (add-to-list 'frog-jump-buffer-filter-actions '("X" "[persp]" persp-is-current-buffer))
    (setq frog-jump-buffer-default-filter
          'persp-is-current-buffer))

(use-package treemacs-evil
    :after (treemacs evil)
    :ensure t)

;; LSP-mode
(use-package lsp-mode
    :ensure t
    :init
    :hook ((c-mode-common . lsp)
           (lsp-mode . lsp-enable-which-key-integration)
           (c++-mode . lsp)
           (typescript-mode . lsp)
           (ruby-mode . lsp))
    :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; Java
(use-package lsp-java
    :ensure t
    :hook (java-mode . lsp)
    :bind (:map lsp-command-map
                ("j i" . lsp-java-add-import)
                ("j o" . lsp-java-organize-imports))
    :config
    (setq lsp-java-vmargs
          (list
           "-Xmx4G"
           "-XX:+UseG1GC"
           "-XX:+UseStringDeduplication"
           ;; Download lombok.jar from https://projectlombok.org/download and put it at this path
           (concat "-javaagent:" (expand-file-name "~/lombok.jar"))))
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bemol\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "/build/private")
    (add-to-list 'lsp-file-watch-ignored-directories "/build/src-files")
    (add-to-list 'lsp-file-watch-ignored-directories "/build/brazil-documentation")
    (add-to-list 'lsp-file-watch-ignored-directories "/node_modules")
    (add-to-list 'lsp-file-watch-ignored-directories "/dist")
    (add-to-list 'lsp-file-watch-ignored-directories "/.git")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]eclipse-bin")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bemol/.+/javadoc")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]env")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]out")
    (setq lsp-java-project-resource-filters ["node_modules" ".metadata" "archetype-resources" "META-INF/maven" "runtime" "env"]))

;; Kotlin
(use-package kotlin-mode
    :ensure t)


;; Python
(use-package lsp-pyright
    :ensure t
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp))))

;; Groovy
(use-package groovy-mode
    :ensure t)

(provide 'init-lsp)
