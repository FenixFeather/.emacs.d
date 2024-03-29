;; Top level stuff
;; On Windows, add 'emacsclientw -n -a ""' for client
;; 'runemacs.exe --daemon' on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; use-package setup
(require 'package)
(setq package-enable-at-startup nil)

;; Disable package-selected-packages
(defun package--save-selected-packages (&rest opt) nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

(unless (executable-find "python")
      (setq python-shell-interpreter "python3"))

(use-package diminish
  :ensure t)

;; Set env vars properly
(use-package exec-path-from-shell
    :ensure t
    :hook ((after-init . exec-path-from-shell-initialize))
    :config
    (setq exec-path-from-shell-variables '("PATH" "NVM_BIN")))

;; Ace-window
(use-package ace-window
    :ensure t
    :bind (("M-[" . ace-window)
           ("M-o" . ace-window)
           ("C-x o" . ace-window)))

;; amx
(use-package amx
    :ensure t)

;; Auctex
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

;; Astyle
(use-package astyle
  :ensure t
  :when (executable-find "astyle")
  :hook (c-mode-common . astyle-on-save-mode))

;; Avy
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("C-M-g" . avy-goto-line)
         ("C-," . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("M-i" . avy-copy-line))
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
  (setq company-idle-delay 0)
  (setq company-dabbrev-downcase nil))

(use-package yasnippet
    :ensure t
    :commands yas-minor-mode
    :hook ((prog-mode . yas-minor-mode)))

(use-package yasnippet-snippets
    :ensure t)

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

;; dirvish
(use-package dirvish
    :ensure t
    :config
    (dirvish-override-dired-mode))

;; JavaScript
(use-package js2-mode
    :defer t
    :ensure t)

(use-package json-mode
  :mode ("\\.eslintrc.*$" "\\.babelrc$")
  :config
  (setq js-indent-level 2)
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

;; ggtags
(use-package ggtags
  :ensure t
  :hook ((c-mode . ggtags-mode)
         (c++-mode . ggtags-mode)
         (java-mode . ggtags-mode)))

;; Haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

;; Highlight indent guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'text-mode-hook 'highlight-indent-guides-mode))

;; Ivy, Swiper, and Counsel
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . 'ivy-resume)
         :map ivy-minibuffer-map
         ("C-j" . ivy-done)
         ("RET" . ivy-alt-done))
  :config
  (setq ivy-count-format "%d/%d ")
  (setq ivy-extra-directories nil)
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . 'swiper)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :demand
  :config
  (counsel-mode 1)
  :bind (("M-x" . counsel-M-x)
         ("M-<left>" . 'pop-to-mark-command)))

(use-package counsel-tramp
  :ensure t
  )

;; Jinja
(use-package mmm-jinja2
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml.jinja\\'" . yaml-mode))
  (mmm-add-mode-ext-class 'yaml-mode "\\.yml.jinja\\'" 'jinja2))

;; Magit
(use-package magit
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'" "\\.mkdn\\'"))

;; Org mode
(use-package org
  :config
  (defun my-org-mode-hook ()
    (visual-line-mode 1)
    (flyspell-mode))
  (add-hook 'org-mode-hook 'my-org-mode-hook))

;; Powershell
(use-package powershell
  :defer t
  :ensure t)

;; Rainbow
(use-package rainbow-delimiters
    :ensure t
    :hook ((prog-mode . rainbow-delimiters-mode)))

;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

;; smart-line-mode
(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :config
    (setq doom-modeline-hud t))

;; toml
(use-package toml-mode
    :ensure t
    :mode "\\.toml\\'")

;; typescript
(use-package typescript-mode
    :ensure t
    :config
    (setq typescript-indent-level 2))

;; visual-regexp-steroids
(use-package visual-regexp-steroids
    :ensure t
    :demand
    :bind (("C-M-$" . vr/replace)
           ("C-M-%" . vr/query-replace))
    :config
    (unless (executable-find "python")
      (setq vr/command-python
            (string-join
             (cons "python3" (cdr (split-string vr/command-python " ")))
             " "))))

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
(defmacro maybe-daemon (&rest args)
  "Run in both daemon and non-daemon mode"
  (if (daemonp)
      `(add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  ,@args))
    `(progn ,@args)))

(when (eq system-type 'darwin)
  (maybe-daemon
   (load-file "~/.emacs.d/lisp/mac-os.el")))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (maybe-daemon (load-theme 'spacemacs-dark t)))

;; undo-tree
(use-package undo-tree
    :ensure t
    :bind (("M-/" . undo-tree-undo)
           ("C-r" . undo-tree-redo))
    :hook ((after-init . global-undo-tree-mode)))

(use-package xonsh-mode
    :ensure t)

;; Lisp
(setq lisp-indent-function 'common-lisp-indent-function)

(use-package lispy
    :ensure t
    :hook ((emacs-lisp-mode . lispy-mode)))

(use-package lispyville
    :ensure t
    :after (lispy)
    :hook ((lispy-mode . lispyville-mode))
    :config
    (lispyville-set-key-theme '(operators c-w additional additional-insert)))

;; Misc
(setq visible-bell 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq-default ispell-program-name "aspell")
(setq scroll-preserve-screen-position 'always)
(setq comint-prompt-read-only t)
(global-hl-line-mode +1)
(setq-default indent-tabs-mode nil)

(setq c-default-style "k&r"
      c-basic-offset 4)

(defun my-c++-mode-hook ()
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1)
  (electric-pair-mode 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)

;; Whitespace
(setq whitespace-style '(face trailing lines-tail tabs big-indent empty))
(add-hook 'prog-mode-hook 'whitespace-mode)

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

;; Custom
(if (eq system-type 'windows-nt)
    (setq custom-file "~/.emacs.d/custom-windows.el")
  (setq custom-file "~/.emacs.d/custom.el"))

(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

;; avoid resizing
(customize-set-variable 'even-window-sizes nil)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((debug-on-error t))
 (mapcar 'require (list
                   'init-evil
		   'init-lsp)))

(load custom-file :noerror)
