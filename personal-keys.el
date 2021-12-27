(defun define-two-prefix-keymaps (keymap keymap-to-add)
  (evil-define-key '(normal visual) keymap (kbd "SPC") keymap-to-add)
  (evil-define-key 'emacs keymap (kbd "s-SPC") keymap-to-add))

(defun personal-keys-setup ()
  "Set up evil keybindings"
  (let ((space-map (make-sparse-keymap))
        (lsp-space-map (make-sparse-keymap))
        (treemacs-space-map (make-sparse-keymap)))

    ;; Global space map
    (bind-keys
     :map space-map
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
     ("C-g" . keyboard-quit)
     ("e n" . flycheck-next-error)
     ("r" . revert-buffer)
     ("e N" . flycheck-previous-error))
    (evil-define-key '(normal visual) 'global (kbd "SPC") space-map)
    (evil-define-key 'emacs 'global (kbd "s-SPC") space-map)

    ;; lsp-space-map
    (bind-keys
     :map lsp-space-map
     ("." . lsp-find-definition))
    (bind-key "l" lsp-command-map lsp-space-map)
    (define-two-prefix-keymaps lsp-mode-map lsp-space-map)

    ;; treemacs-space-map
    (bind-key "w" treemacs-project-map treemacs-space-map)
    (bind-key "p" treemacs-workspace-map treemacs-space-map)
    (define-two-prefix-keymaps treemacs-mode-map treemacs-space-map)
 ))

(provide 'personal-keys)
