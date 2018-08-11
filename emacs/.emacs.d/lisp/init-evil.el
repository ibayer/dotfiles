;;; init-evil.el -- My evil mode configuration.
;; Commentary:
;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    ;"aa" 'align-regexp
    ;"a=" 'my-align-single-equals
    ;"b"  'helm-mini             ;; Switch to another buffer
    ;"B"  'magit-blame-toggle
    "c"  'comment-dwim	   ;
   ;"d"  'kill-this-buffer
    ;"D"  'open-current-line-in-codebase-search
    ;"f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    ;"h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    ;"l"  'whitespace-mode       ;; Show invisible characters
    ;"nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    ;"nw" 'widen
    "o"  'delete-other-windows  ;; C-w o
    "p"  'helm-show-kill-ring
    ;"s"  'ag-project            ;; Ag search from project's root
    ;"r"  'chrome-reload
    ;"R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    ;"S"  'delete-trailing-whitespace
    ;"t"  'gtags-reindex
    ;"T"  'gtags-find-tag
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard)

  )

(defun air--config-evil ()
  "Configure evil mode."

  ; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "<down>")  'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>")    'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-")       'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (evil-define-key 'normal global-map (kbd "C-p")   'helm-projectile)
)

;(use-package evil
; :ensure t
; ;:commands (evil-mode evil-define-key)
; :config
; (add-hook 'evil-mode-hook 'air--config-evil)
; (evil-mode 1)



(setq evil-want-integration nil)
(use-package evil
  :ensure t
  ;:after evil-leader
  :init
  (setq evil-want-integration nil)
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)
  )

(use-package evil-leader
  :after evil
  :ensure t
  :config
  (global-evil-leader-mode)
  (air--config-evil-leader))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-magit
  :after evil
  :ensure t)

(use-package key-chord
  :after evil
  :ensure t
  :config
    ;;Exit insert mode by pressing j and then j quickly
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
