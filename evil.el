;; Evil
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
    (bind-keys
     :map evil-normal-state-map
     :prefix-map space-map
     :prefix "SPC"
     ("g g" . avy-goto-line)
     ("g c" . avy-goto-char)
     ("o" . ace-window)
     ("0" . treemacs-select-window)
     ("b" . frog-jump-buffer)
     ("u" . pop-global-mark)
     ("q" . kill-buffer-and-window)
     ("3" . split-window-right)
     ("2" . split-window-below)
     ("1" . delete-other-windows)
     ("m" . magit-status)
     ("k" . (lambda () (interactive) (kill-buffer (current-buffer))))
     ("C-g" . keyboard-quit))
    (add-hook 'lsp-mode-hook
              '(lambda ()
                (bind-key "l" lsp-command-map space-map)
                (bind-key "." 'lsp-find-definition space-map)))
    (bind-key "s-SPC" space-map global-map))
