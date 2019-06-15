;; use-package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;; Ace-window
(use-package ace-window
  :ensure t
  :bind (("M-[" . ace-window)
	 ("C-x o" . ace-window)))

;; Anaconda
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

;; Auctex
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

;; Avy
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g g" . avy-goto-line)
	 ("C-M-g" . avy-goto-line)
	 ("C-," . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0))
  :config
  (avy-setup-default))
;; cdlatex
(use-package cdlatex-mode
  :ensure cdlatex
  :defer t)

;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0))

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-auctex
  :ensure t
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook #'company-auctex-init))

(use-package company-bibtex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-bibtex))

;; csv-mode
(use-package csv-mode
  :mode "\\.csv\\'"
  :ensure t)

;; Dockerfile
(use-package dockerfile-mode
  :defer t
  :ensure t)

;; dtrt
(use-package dtrt-indent
  :mode "\\.js\\'"
  :ensure t)

;; JavaScript
(use-package js2-mode
  :defer t
  :ensure t)

(use-package json-mode
  :mode ("\\.eslintrc.*$" "\\.babelrc$")
  :ensure t)

(use-package rjsx-mode
  :mode "\\.js\\'"
  :ensure t)

;; Julia
(use-package julia-mode
  :mode "\\.jl\\'"
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (require 'flycheck)
  ;; disable jshint since we prefer eslint checking
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint json-python-json javascript-jshint
					    javascript-gjslint javascript-jscs)))

  (defun my-js2-mode-hook ()
    (flycheck-mode)
    (setq js2-basic-offset 2)
    )
  (add-hook 'js2-mode-hook 'my-js2-mode-hook)

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

;; Haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

;; Magit
(use-package magit
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'" "\\.mkdn\\'"))

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (defun neobush ()
    "Hide NeoTree's scrollbar"
    (interactive)
    (neotree)
    (set-window-scroll-bars nil nil))
  :bind ("C-c C-n" . neobush)
  )

;; Org mode
(use-package org
  :config
  (defun my-org-mode-hook ()
    (visual-line-mode 1)
    (flyspell-mode))
  (add-hook 'org-mode-hook 'my-org-mode-hook))

;; Paredit
(use-package paredit
  :ensure t)

;; Powershell
(use-package powershell
  :defer t
  :ensure t)

;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

;; smart-line-mode
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

;; Smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

;; ssh-agency
(use-package ssh-agency
  :ensure t
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; toml
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;; visual-regexp
(use-package visual-regexp
  :ensure t)

;; visual-regexp-steroids
(use-package visual-regexp-steroids
  :ensure t
  :after visual-regexp
  :bind (("C-M-$" . vr/replace)
	 ("C-M-^" . vr/query-replace)))

;; vue
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))

;; with-editor
(use-package with-editor
  :ensure t)

;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.php\\'"))

;; yaml
(use-package yaml-mode
  :defer t
  :ensure t)

;; theme
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config (load-theme 'spacemacs-dark t))

;; Non-package options
(setq visible-bell 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(require 'ido)
(ido-mode t)
(setq-default ispell-program-name "aspell")
(setq scroll-preserve-screen-position 'always)

(setq c-default-style "k&r"
          c-basic-offset 4)

(defun my-c++-mode-hook ()
  (c-set-style "k&r")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1)
  (electric-pair-mode 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;;Windows backup
(setq version-control t ;; Use version numbers for backups.
      kept-new-versions 10 ;; Number of newest versions to keep.
      kept-old-versions 0 ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t) ;; Copy all files, don't rename them.
  ;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
	  (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(custom-safe-themes
   (quote
    ("bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(doc-view-continuous t)
 '(doc-view-dvipdf-program "dvipdfm")
 '(doc-view-ghostscript-program "gswin64c")
 '(doc-view-resolution 300)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(inferior-julia-program-name "julia")
 '(longlines-wrap-follows-window-size t)
 '(org-indent-mode-turns-off-org-adapt-indentation nil)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (smart-mode-line-atom-one-dark-theme smart-mode-line spacemacs-theme neotree zenburn-theme yaml-mode web-mode vue-mode visual-regexp-steroids use-package toml-mode ssh-agency smex rjsx-mode powershell paredit markdown-mode julia-mode json-mode haskell-mode flycheck dtrt-indent dockerfile-mode csv-mode company-bibtex company-auctex company-anaconda cdlatex ace-window)))
 '(preview-gs-command "GSWIN64C.EXE")
 '(reftex-cite-prompt-optional-args (quote maybe))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
(put 'downcase-region 'disabled nil)
