;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:


;; Leave this here, or package.el will just add it again.
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

;(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;(show-paren-mode 1)
;(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;(setq-default left-fringe-width nil)
;(setq-default indent-tabs-mode nil)
;(eval-after-load "vc" '(setq vc-handled-backends nil))
;(setq vc-follow-symlinks t)
;(setq large-file-warning-threshold nil)
;(setq split-width-threshold nil)
;(setq custom-safe-themes t)
;(put 'narrow-to-region 'disabled nil)

;(defun my-minibuffer-setup-hook ()
  ;"Increase GC cons threshold."
  ;(setq gc-cons-threshold most-positive-fixnum))

;(defun my-minibuffer-exit-hook ()
  ;"Set GC cons threshold to its default value."
  ;(setq gc-cons-threshold 1000000))

;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files t)

; backup files under version control
(setq vc-make-backup-files t)

(setq version-control t ;; Use version numbers for backups.
    kept-new-versions 10 ;; Number of newest versions to keep.
    kept-old-versions 0 ;; Number of oldest versions to keep.
    delete-old-versions t ;; Don't ask to delete excess backup versions.
    backup-by-copying t) ;; Copy all files, don't rename them.

;;; File type overrides.
;(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;; My own configurations, which are bundled in my dotfiles.
;(require 'init-platform)
(require 'init-global-functions)

;(require 'diminish)
;(require 'init-fonts)
;(require 'init-gtags)
(require 'init-evil)
;(require 'init-twitter)
;(require 'init-maps)
;(require 'init-w3m)
;(require 'init-php)
;(require 'init-powerline)
;(require 'init-flycheck)
;(require 'init-tmux)

;(require 'markdown-preview-mode)
;(add-hook 'markdown-preview-mode-hook
          ;(lambda ()
            ;(setq markdown-preview-template
                  ;(expand-file-name "~/.emacs.d/markdown-preview.html" user-emacs-directory))
            ;(setq markdown-preview-style
                  ;"http://aaronbieber.com/assets/styles/github-markdown.css")))

;(add-to-list 'load-path (expand-file-name "fence-edit" user-emacs-directory))
;(require 'fence-edit)

;; Utilities
;(use-package s
  ;:ensure t
  ;:defer 1)
;(use-package dash :ensure t)

;; Org Mode
(require 'init-org)


;(use-package exec-path-from-shell
  ;:ensure t
  ;:config
  ;(exec-path-from-shell-initialize))

;(use-package elpy
  ;:ensure t
  ;:mode ("\\.py\\'" . elpy-mode))

;(use-package groovy-mode
  ;:ensure t
  ;:mode "\\.groovy\\'"
  ;:config
  ;(c-set-offset 'label 4))

;(use-package rainbow-mode
  ;:ensure t
  ;:commands rainbow-mode)

;(use-package wgrep
  ;:ensure t
  ;:config
  ;(setq wgrep-auto-save-buffer t)
  ;(defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    ;(if (fboundp 'evil-normal-state)
        ;(evil-normal-state)))
  ;(ad-activate 'wgrep-change-to-wgrep-mode)

  ;(defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    ;(if (fboundp 'evil-motion-state)
        ;(evil-motion-state)))
  ;(ad-activate 'wgrep-finish-edit))

;(use-package wgrep-ag
  ;:ensure t
  ;:commands (wgrep-ag-setup))

;(use-package ag
  ;:ensure t
  ;:commands (ag ag-project)
  ;:config
  ;(add-hook 'ag-mode-hook
            ;(lambda ()
              ;(wgrep-ag-setup)
              ;(define-key ag-mode-map (kbd "n") 'evil-search-next)
              ;(define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  ;(setq ag-executable "/usr/local/bin/ag")
  ;(setq ag-highlight-search t)
  ;(setq ag-reuse-buffers t)
  ;(setq ag-reuse-window t))

;(use-package js2-mode
  ;:ensure t
  ;:mode "\\.js\\'")

;(use-package exec-path-from-shell
  ;:ensure t
  ;:defer t
  ;:config
  ;(when (memq window-system '(mac ns))
    ;(exec-path-from-shell-initialize)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-bibliography '("~/git/thesis/references-biblatex.bib"))
  (setq bibtex-completion-library-path '("~/Dropbox/paper-collection/pdfs"))
  (setq bibtex-completion-notes-path "~/Dropbox/paper-collection/notes.org"))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/paper-collection/notes.org"
        org-ref-default-bibliography '("~/git/thesis/references-biblatex.bib")
        org-ref-pdf-directory "~/Dropbox/paper-collection/pdfs/")


    (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
	    (key (car results))
	    (pdf-file (car (bibtex-completion-find-pdf key))))
	(if (file-exists-p pdf-file)
	    (org-open-file pdf-file)
	(message "No PDF found for %s" key))))

    (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
  )

;(use-package company
  ;:ensure t
  ;:defer t
  ;:init
  ;(global-company-mode)
  ;:config
  ;;(setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
  ;;(setq company-tooltip-selection ((t (:background "yellow2"))))
  ;(setq company-idle-delay 0.2)
  ;(setq company-selection-wrap-around t)
  ;(define-key company-active-map [tab] 'company-complete)
  ;(define-key company-active-map (kbd "C-n") 'company-select-next)
  ;(define-key company-active-map (kbd "C-p") 'company-select-previous))

;(use-package swiper
  ;:ensure t
  ;:commands swiper
  ;:bind ("C-s" . counsel-grep-or-swiper)
  ;:config
  ;(require 'counsel)
  ;(setq counsel-grep-base-command "grep -niE \"%s\" %s")
  ;(setq ivy-height 20))

;(use-package dictionary :ensure t)

;(use-package emmet-mode
  ;:ensure t
  ;:commands emmet-mode)

;(use-package flycheck
  ;:ensure t
  ;:commands flycheck-mode)

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t)

;(use-package markdown-mode
  ;:ensure t
  ;:mode "\\.md\\'"
  ;:config
  ;(setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  ;(define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  ;(define-key markdown-mode-map (kbd "C-c '") 'fence-edit-code-at-point)
  ;(define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  ;(define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  ;(define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  ;(define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  ;(define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  ;(define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6))

;(use-package php-extras :ensure t :defer t)
;(use-package sublime-themes :ensure t)
;(use-package sunshine
  ;:ensure t
  ;:commands sunshine-forecast
  ;:config
  ;(defun get-string-from-file (file-path)
    ;"Return FILE-PATH's contents."
    ;(with-temp-buffer
      ;(insert-file-contents file-path)
      ;(buffer-string)))
  ;(setq sunshine-appid (get-string-from-file
                        ;(expand-file-name "sunshine-appid" user-emacs-directory)))
  ;(setq sunshine-location "Brookline, MA")
  ;(setq sunshine-show-icons t))

;(use-package web-mode
  ;:ensure t
  ;:defer t
  ;:config
  ;(setq web-mode-attr-indent-offset 2)
  ;(setq web-mode-code-indent-offset 2)
  ;(setq web-mode-css-indent-offset 2)
  ;(setq web-mode-indent-style 2)
  ;(setq web-mode-markup-indent-offset 2)
  ;(setq web-mode-sql-indent-offset 2))

;(use-package zenburn-theme :ensure t :defer t)
;(use-package mmm-mode :ensure t :defer t)
;(use-package yaml-mode :ensure t :defer t)

;(use-package yasnippet
  ;:ensure t
  ;:defer t
  ;:config
  ;(yas-reload-all)
  ;(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           ;"~/.emacs.d/remote-snippets"))
  ;(setq tab-always-indent 'complete)
  ;(setq yas-prompt-functions '(yas-completing-prompt
                               ;yas-ido-prompt
                               ;yas-dropdown-prompt))
  ;(define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

;(use-package which-key
  ;:ensure t
  ;:diminish ""
  ;:config
  ;(which-key-mode t))

(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;(use-package highlight-symbol
  ;:ensure t
  ;:defer t
  ;:diminish ""
  ;:config
  ;(setq-default highlight-symbol-idle-delay 1.5))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;(use-package mmm-mode
  ;:ensure t
  ;:defer t
  ;:config
  ;(setq mmm-global-mode 'maybe)
  ;(mmm-add-classes
   ;'((markdown-cl
      ;:submode emacs-lisp-mode
      ;:face mmm-declaration-submode-face
      ;:front "^~~~cl[\n\r]+"
      ;:back "^~~~$")
     ;(markdown-php
      ;:submode php-mode
      ;:face mmm-declaration-submode-face
      ;:front "^```php[\n\r]+"
      ;:back "^```$")))
  ;(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)
  ;(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-php))

;(use-package sublime-themes :ensure t)
(use-package gruvbox-theme :ensure t)
;(use-package color-theme-sanityinc-tomorrow :ensure t)

;(use-package undo-tree
  ;:ensure t
  ;:diminish t
  ;:config
  ;(setq undo-tree-auto-save-history t)
  ;(setq undo-tree-history-directory-alist
        ;(list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

;;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
;(require 'epa-file)
;(epa-file-enable)
;(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

;(defvar show-paren-delay 0
  ;"Delay (in seconds) before matching paren is highlighted.")

;;; Flycheck mode:
;(add-hook 'flycheck-mode-hook
          ;(lambda ()
            ;(evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            ;(evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
;(add-hook 'lisp-interaction-mode-hook
          ;(lambda ()
            ;(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; All programming modes
;(defun air--set-up-prog-mode ()
  ;"Configure global prog-mode."
  ;(setq-local comment-auto-fill-only-comments t)
  ;(electric-pair-local-mode))
;(add-hook 'prog-mode-hook 'air--set-up-prog-mode)

;(use-package nlinum-relative
  ;:ensure t
  ;:config
  ;(nlinum-relative-setup-evil)
  ;(setq nlinum-relative-redisplay-delay 0)
  ;(add-hook 'prog-mode-hook #'nlinum-relative-mode))

;;; Python mode:
;(use-package virtualenvwrapper
  ;:ensure t
  ;:config
  ;(venv-initialize-interactive-shells)
  ;(venv-initialize-eshell)
  ;(setq venv-location
        ;(expand-file-name "~/Projects/virtualenvs/")))

;(add-hook 'python-mode-hook
          ;(lambda ()
            ;;; I'm rudely redefining this function to do a comparison of `point'
            ;;; to the end marker of the `comint-last-prompt' because the original
            ;;; method of using `looking-back' to match the prompt was never
            ;;; matching, which hangs the shell startup forever.
            ;(defun python-shell-accept-process-output (process &optional timeout regexp)
              ;"Redefined to actually work."
              ;(let ((regexp (or regexp comint-prompt-regexp)))
                ;(catch 'found
                  ;(while t
                    ;(when (not (accept-process-output process timeout))
                      ;(throw 'found nil))
                    ;(when (= (point) (cdr (python-util-comint-last-prompt)))
                      ;(throw 'found t))))))

            ;;; Additional settings follow.
            ;(add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;;; The Emacs Shell
;(defun company-eshell-history (command &optional arg &rest ignored)
  ;"Complete from shell history when starting a new line.

;Provide COMMAND and ARG in keeping with the Company Mode backend spec.
;The IGNORED argument is... Ignored."
  ;(interactive (list 'interactive))
  ;(cl-case command
    ;(interactive (company-begin-backend 'company-eshell-history))
    ;(prefix (and (eq major-mode 'eshell-mode)
                 ;(let ((word (company-grab-word)))
                   ;(save-excursion
                     ;(eshell-bol)
                     ;(and (looking-at-p (s-concat word "$")) word)))))
    ;(candidates (remove-duplicates
                 ;(->> (ring-elements eshell-history-ring)
                      ;(remove-if-not (lambda (item) (s-prefix-p arg item)))
                      ;(mapcar 's-trim))
                 ;:test 'string=))
    ;(sorted t)))

;(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  ;"Kill term buffer when term is ended."
  ;(if (memq (process-status proc) '(signal exit))
      ;(let ((buffer (process-buffer proc)))
        ;ad-do-it
        ;(kill-buffer buffer))
    ;ad-do-it))
;(ad-activate 'term-sentinel)

;(add-hook 'eshell-mode-hook
          ;(lambda ()
            ;(set (make-local-variable 'pcomplete-ignore-case) t)
            ;(set (make-local-variable 'company-backends)
                 ;'((company-shell company-eshell-history)))))

;;; Magit mode (which does not open in evil-mode):
;(add-hook 'magit-mode-hook
          ;(lambda ()
            ;(define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;;; Git Commit Mode (a Magit minor mode):
;(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;;; Emmet mode:
;(add-hook 'emmet-mode-hook
          ;(lambda ()
            ;(evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
            ;(evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))

;;;; Web mode:
;(add-hook 'web-mode-hook
          ;(lambda ()
            ;(setq web-mode-style-padding 2)
            ;(yas-minor-mode t)
            ;(emmet-mode)
            ;(flycheck-add-mode 'html-tidy 'web-mode)
            ;(flycheck-mode)))

;(setq web-mode-ac-sources-alist
      ;'(("php" . (ac-source-php-extras ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ;("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

;(add-hook 'web-mode-before-auto-complete-hooks
          ;'(lambda ()
             ;(let ((web-mode-cur-language (web-mode-language-at-pos)))
               ;(if (string= web-mode-cur-language "php")
                   ;(yas-activate-extra-mode 'php-mode)
                 ;(yas-deactivate-extra-mode 'php-mode))
               ;(if (string= web-mode-cur-language "css")
                   ;(setq emmet-use-css-transform t)
                 ;(setq emmet-use-css-transform nil)))))

;;;; Emacs Lisp mode:
;(add-hook 'emacs-lisp-mode-hook
          ;(lambda ()
            ;(yas-minor-mode t)
            ;(eldoc-mode)
            ;(highlight-symbol-mode)
            ;(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;;; SH mode:
;(add-hook 'sh-mode-hook (lambda ()
                          ;(setq sh-basic-offset 2)
                          ;(setq sh-indentation 2)))

;;;; Twittering mode:
;(setq twittering-use-master-password t)
;(add-hook 'twittering-mode-hook (lambda ()
                                  ;(define-key twittering-mode-map (kbd "C-c C-a") 'twittering-favorite)
                                  ;(define-key twittering-mode-map (kbd ",b") 'helm-mini)))

;(add-hook 'twittering-edit-mode-hook (lambda ()
                                       ;(flyspell-mode)))

;;;; Javascript mode:
;(add-hook 'javascript-mode-hook (lambda ()
                                  ;(set-fill-column 120)
                                  ;(turn-on-auto-fill)
                                  ;(setq js-indent-level 2)))

;;;; Markdown mode:
;(add-hook 'markdown-mode-hook (lambda ()
                                ;(set-fill-column 80)
                                ;(turn-on-auto-fill)
                                ;(flyspell-mode)))

;;;; HTML mode:
;(add-hook 'html-mode-hook (lambda ()
                            ;(setq sgml-basic-offset 2)
                            ;(setq indent-tabs-mode nil)))

;(put 'narrow-to-region 'disabled nil)

;;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;;; used in OS X. Disable sRGB before setting up Powerline.
;(when (memq window-system '(mac ns))
  ;(setq ns-use-srgb-colorspace nil))

(load-theme 'gruvbox)

(provide 'init)
;;; init.el ends here
