;; Evil
(require 'personal-keys)
(use-package evil
    :ensure t
    :commands evil-mode
    :hook ((after-init . evil-mode))
    :bind (:map evil-normal-state-map
                ("u" . undo-tree-undo)
                ("C-r" . undo-tree-redo)
                ("C-u" . evil-scroll-up))
    :config
    (setq evil-undo-system 'undo-tree)
    (setq evil-emacs-state-cursor '("light slate blue" box))
    (setq evil-normal-state-cursor '("peach puff" box))
    (personal-keys-setup))

(use-package evil-commentary
    :ensure t
    :hook ((after-init . evil-commentary-mode)))
(provide 'init-evil)
