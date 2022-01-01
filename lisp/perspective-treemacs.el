;;; perspective-treemacs.el --- Remember selected treemacs workspace per perspective -*- lexical-binding: t; -*-

;; Commentary:

;; The difference with treemacs-perspective is that there is no need
;; for 1-1 relationship between treemacs ws and perspective, enabling
;; per feature perspectives rather than per workspace perspectives

(require 'perspective)
(require 'treemacs)

(setq debug-on-error t)
;; remove base compatibility hook
(remove-hook 'perspective-activated-functions #'treemacs--remove-treemacs-window-in-new-frames)

(defmacro dm (&rest args)
  (when nil
    `(apply 'message (list ,@args))))

(defvar persp-treemacs--current-ws-name (treemacs-workspace->name (treemacs-current-workspace)))
(defvar persp-treemacs--current-visibility (treemacs-current-visibility))
(persp-make-variable-persp-local 'persp-treemacs--current-ws-name)
(persp-make-variable-persp-local 'persp-treemacs--current-visibility)

(defun perspective-treemacs--on-persp-mode ()
  (perspective-treemacs--update-persp-ws-name))

(defun perspective-treemacs--update-persp-ws-name (&rest _)
  (dm "Current perspective:")
  (dm (persp-current-name))
  (unless persp-treemacs--current-visibility (dm "Uh oh"))
  (setq persp-treemacs--current-visibility (treemacs-current-visibility))
  (setq persp-treemacs--current-ws-name (treemacs-workspace->name (treemacs-current-workspace)))
  (dm (string-join (list "Update to " persp-treemacs--current-ws-name)))
  (dm "Update Visibility:")
  (dm (symbol-name persp-treemacs--current-visibility)))

(defun perspective-treemacs--on-persp-switch ()
  "Switch to stored treemacs WS"
  (dm (string-join (list "switch to " persp-treemacs--current-ws-name)))
  (let ((treemacs-switch-workspace-hook nil))
    (treemacs-do-switch-workspace persp-treemacs--current-ws-name))
  (dm "Found visibility:")
  (dm (symbol-name persp-treemacs--current-visibility))
  (when (eq persp-treemacs--current-visibility 'visible)
    (dm "start treemacs")
    (treemacs)
    (when (eq treemacs-select-when-already-in-treemacs
              'move-back)
      (call-interactively 'treemacs-select-window)))
  (perspective-treemacs--update-persp-ws-name))

(defun perspective-treemacs ()
  (when (bound-and-true-p persp-mode)
    (perspective-treemacs--on-persp-mode))
  (add-hook 'persp-mode-hook #'perspective-treemacs--on-persp-mode)
  (add-hook 'treemacs-switch-workspace-hook #'perspective-treemacs--update-persp-ws-name)
  (add-hook 'persp-before-switch-hook #'perspective-treemacs--update-persp-ws-name)
  (add-hook 'persp-switch-hook #'perspective-treemacs--on-persp-switch)
  (let ((treemacs-hooks `(treemacs-switch-workspace-hook
                          treemacs-select-functions
                          treemacs-rename-workspace-functions
                          treemacs-create-workspace-functions
                          treemacs-workspace-edit-hook)))
    (mapcar (lambda (target)
              (add-hook target #'perspective-treemacs--update-persp-ws-name))
            treemacs-hooks)))

(perspective-treemacs)
(provide 'perspective-treemacs)

;;; perspective-treemacs.el ends here
