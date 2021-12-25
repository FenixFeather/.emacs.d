;; Evil
(use-package evil
    :ensure t
    :commands evil-mode
    :hook ((after-init . evil-mode))
    :bind (:map evil-normal-state-map
                ("u" . undo-tree-undo)
                ("C-r" . undo-tree-redo)
                ("G" . avy-goto-line))
    :config
    (setq evil-default-state 'emacs)
    (setq evil-undo-system 'undo-tree))
