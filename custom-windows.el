;; ssh-agency
(use-package ssh-agency
  :ensure t
  :config
  (unless (file-exists-p "~/.ssh/id_rsa.pub")
    (remove-hook 'magit-credential-hook 'ssh-agency-ensure))
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

(setq w32-lwindow-modifier 'super)
(w32-register-hot-key [s-])

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(doc-view-continuous t)
 '(doc-view-dvipdf-program "dvipdfm")
 '(doc-view-ghostscript-program "gswin64c")
 '(doc-view-resolution 300)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-pair-mode t)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(inferior-julia-program-name "julia")
 '(longlines-wrap-follows-window-size t)
 '(org-indent-mode-turns-off-org-adapt-indentation nil)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(preview-gs-command "GSWIN64C.EXE")
 '(reftex-cite-prompt-optional-args 'maybe)
 '(show-paren-mode t)
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t)
 '(whitespace-line-column 120))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(fixed-pitch ((t (:family "Iosevka Fixed")))))
(put 'downcase-region 'disabled nil)
