(defun define-two-prefix-keymaps (keymap keymap-to-add)
  (evil-define-key '(normal visual) keymap (kbd "SPC") keymap-to-add)
  (evil-define-key 'emacs keymap (kbd "s-SPC") keymap-to-add))

(defun split-window-ultrawide ()
  (interactive)
  (split-window-right 100)
  (call-interactively 'other-window)
  (split-window-right -100))

(defun personal-keys-setup ()
  "Set up evil keybindings"
  (let ((space-map (make-sparse-keymap))
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

     ("5 5" . query-replace)
     ("5 r" . vr/query-replace)

     ("4" . split-window-ultrawide)
     ("3" . split-window-right)
     ("2" . split-window-below)
     ("1" . delete-other-windows)
     ("m" . magit-status)
     ("k" . (lambda () (interactive) (kill-buffer (current-buffer))))
     ("C-g" . keyboard-quit)
     ("e n" . flycheck-next-error)
     ("r" . revert-buffer)
     ("e N" . flycheck-previous-error))
    (bind-key "P" perspective-map space-map)
    (evil-define-key '(normal visual) 'global (kbd "SPC") space-map)
    (evil-define-key 'emacs 'global (kbd "s-SPC") space-map)

    ;; lsp-space-map
    (with-eval-after-load 'lsp-mode
      (let ((lsp-space-map (make-sparse-keymap)))
        (bind-keys
         :map lsp-space-map
         ("." . lsp-find-definition))
        (bind-key "l" lsp-command-map lsp-space-map)
        (define-two-prefix-keymaps lsp-mode-map lsp-space-map)))

    ;; magit
    (with-eval-after-load 'magit (bind-key "SPC" space-map magit-mode-map))

    ;; treemacs-space-map
    (bind-key "p" treemacs-project-map treemacs-space-map)
    (bind-key "w" treemacs-workspace-map treemacs-space-map)
    (bind-keys
     :map treemacs-space-map
     ("r" . treemacs-refresh))
    (set-keymap-parent treemacs-space-map space-map)
    (evil-define-key '(normal visual motion treemacs) treemacs-mode-map (kbd "SPC") treemacs-space-map)))

(provide 'personal-keys)
