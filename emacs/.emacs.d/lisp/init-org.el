;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;; Helper functions

(defun air-org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header))

(defun air-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header t))

(defun air--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))


(defun air-org-skip-if-not-closed-today (&optional subtree)
  "Skip entries that were not closed today.

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
               (save-excursion (progn (outline-next-heading) (1- (point))))))
        (today-prefix (format-time-string "%Y-%m-%d")))
    (if (save-excursion
          (and (re-search-forward org-closed-time-regexp end t)
               (string= (substring (match-string-no-properties 1) 0 10) today-prefix)))
        nil
      end)))

(defun air-org-skip-if-habit (&optional subtree)
  "Skip an agenda entry if it has a STYLE property equal to \"habit\".

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
                (save-excursion (progn (outline-next-heading) (1- (point)))))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        end
      nil)))

(defun air-org-skip-if-priority (priority &optional subtree)
  "Skip an agenda item if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C.

Skips the current entry unless SUBTREE is not nil."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
                (save-excursion (progn (outline-next-heading) (1- (point))))))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        end
      nil)))

(defun air-org-task-capture (&optional vanilla)
  "Capture a task with my default template.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "a")))

(defun air-org-agenda-capture (&optional vanilla)
  "Capture a task in agenda mode, using the date at point.
   If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "a"))))

(defun air-org-agenda-toggle-date (current-line)
  "Toggle `SCHEDULED' and `DEADLINE' tag in the capture buffer."
  (interactive "P")
  (save-excursion
    (let ((search-limit (if current-line
                            (line-end-position)
                          (point-max))))

      (if current-line (beginning-of-line)
        (beginning-of-buffer))
      (if (search-forward "DEADLINE:" search-limit t)
          (replace-match "SCHEDULED:")
        (and (search-forward "SCHEDULED:" search-limit t)
             (replace-match "DEADLINE:"))))))

(defun air-pop-to-org-todo (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/git/org/todo.org" split))

(defun air-pop-to-org-inbox (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/git/org/inbox.org" split))

(defun air-pop-to-org-projects (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/git/org/projects/active.org" split))

(defun air-pop-to-org-knowledge (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/git/org/knowledge-vault/knowledge.org" split))

;(defun air-pop-to-org-vault (split)
  ;"Visit my encrypted vault file, in the current window or a SPLIT."
  ;(interactive "P")
  ;(air--pop-to-file "~/Dropbox/org/vault.gpg" split))

(defun air-pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  ;; (org-agenda nil "d")
  (org-agenda-list)
  (when (not split)
    (delete-other-windows)))

;(defun air--org-insert-list-leader-or-self (char)
  ;"If on column 0, insert space-padded CHAR; otherwise insert CHAR.

;This has the effect of automatically creating a properly indented list
;leader; like hyphen, asterisk, or plus sign; without having to use
;list-specific key maps."
  ;(if (= (current-column) 0)
      ;(insert (concat " " char " "))
    ;(insert char)))

(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let ((old-tags (org-get-tags-string))
        (tags (if tags
                  (concat " " tags)
                "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position) t)
      (replace-match tags)
      (org-set-tags t))))

;(defun air-org-goto-first-child ()
  ;"Goto the first child, even if it is invisible.

;Return t when a child was found.  Otherwise don't move point and
;return nil."
  ;(interactive)
  ;(let ((pos (point))
        ;(re (concat "^" outline-regexp))
        ;level)
    ;(when (condition-case nil (org-back-to-heading t) (error nil))
      ;(setq level (outline-level))
      ;(forward-char 1)
      ;(if (and (re-search-forward re nil t) (> (outline-level) level))
          ;(progn (goto-char (match-beginning 0)) t)
        ;(goto-char pos) nil))))


(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (completing-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (let* ((cur-list (org-get-tags))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (air--org-swap-tags new)))



;;; Code:
(use-package org
  :ensure org-plus-contrib
  :defer t
  :commands (org-capture)
  :bind (("C-c c" .   air-org-task-capture)
         ;; ("C-c l" .   org-store-link)
         ("C-c t i" . air-pop-to-org-inbox)
         ("C-c t p" . air-pop-to-org-projects)
         ("C-c t k" . air-pop-to-org-knowledge)
         ("C-c t t" . air-pop-to-org-todo)
         ;; ("C-c t v" . air-pop-to-org-vault)
         ("C-c t a" . air-pop-to-org-agenda)
         ("C-c t A" . org-agenda)
         ("C-c f k" . org-search-view)
         ("C-c f t" . org-tags-view)
	    ;("C-c f i" . air-org-goto-custom-id)
	 )
  :config
  ;(require 'org-protocol)
  ;(server-start)
  ;(setq org-hide-emphasis-markers t)
  ;; (setq org-modules
  ;;       '(org-bbdb org-bibtex org-docview org-habit org-info org-w3m))


  (require 'org-contacts)
  (setq org-contacts-files '("~/git/org/contacts.org"))

  (require 'org-habit)
  (setq org-modules
	'(org-bibtex org-docview org-habit org-info org-w3m org-drill))

  ; cl is required for org-drill
  (require 'cl)
  (require 'org-collector)

  (defun my-save-final-report ()
    ;; calls my-add-stats-to-table from org-drill buffer
    (org-babel-execute-buffer))
     
  ;; find way to only execute individual source blocks
  ;; (avice-add 'org-drill-final-report :before 'my-save-final-report)

  (defun my-add-stats-to-table ()
   (defun get-last-quality ()
        (org-entry-get (point) "DRILL_LAST_QUALITY"))

   ;; count occurance of value in collected-quantities list
   (defun my-count(my-value)
	(reduce (lambda (acc x) (if (= my-value (string-to-number x))
	        (1+ acc) acc)) collected-qualities :initial-value 0))

   (setq collected-qualities (remove nil (org-map-entries 'get-last-quality "+drill")))
   (setq quality-counts (cl-mapcar 'my-count '(1 2 3 4 5)))

   (add-to-list 'quality-counts (format-time-string "%d/%m/%Y\t%H:%M"))
   (add-to-list 'table quality-counts))


  (defun my-save-drill-stats ()
    ;; calls my-add-stats-to-table from org-drill buffer

    (save-excursion
    (goto-char
      (org-babel-find-named-block "agg-drill-stats"))
      (org-babel-execute-src-block-maybe)))
     
  (advice-add 'org-drill-final-report :before 'my-save-drill-stats)

(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-image-actual-width nil)

(require 'ox-latex)
(eval-after-load "ox-latex"

  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 (setq org-latex-listings t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t) ; this is the entry to activate LaTeX
   (python . t)
   (shell . t)
   (sql . t)
   (C . t)
   (sqlite . nil)))

(defun babel-confirm (flag)
  "Report the setting of org-confirm-babel-evaluate.
If invoked with C-u, toggle the setting"
  (interactive "P")
  (if (equal flag '(4))
      (setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate)))
  (message "Babel evaluation confirmation is %s"
           (if org-confirm-babel-evaluate "on" "off")))

  (setq org-todo-keywords
        '((sequence "NEXT" "TODO" "WAITING" "|" "DONE" "CANCELED")))
  ;(setq org-blank-before-new-entry '((heading . t)
                                     ;(plain-list-item . t)))
  (setq org-capture-templates
        '(("a" "My TODO task format." entry
           (file "todo.org")
           "* TODO %?")

          ("n" "A (work-related) note." entry
           (file+headline "notes.org" "Work")
           "* %?\n%u\n\n"
           :jump-to-captured t)

          ("l" "A link, for reading later." entry
           (file+headline "notes.org" "Reading List")
           "* %:description\n%u\n\n%c\n\n%i"
           :empty-lines 1)

          ;; ("w" "Nine Minutes on Monday weekly agenda." entry
          ;;  (id "9A6DDE04-90B8-49ED-90B9-A55A0D1E7B28")
          ;;  (function air-org-nmom-capture-template))
	  ))
  (setq org-default-notes-file "~/git/org/todo.org")
  (setq org-directory "~/git/org")




  ;; ;;; Logging of state changes
  (setq org-log-done (quote time))
  ;; (setq org-log-redeadline (quote time))
  ;; ;(setq org-log-into-drawer t)

  ;; ;(setq org-insert-heading-respect-content t)
  ;; ;(setq org-ellipsis " …")
  ;; ;(setq org-startup-with-inline-images t)
  ;; ;(setq org-export-initial-scope 'subtree)
  ;; ;(setq org-use-tag-inheritance nil) ;; Use the list form, which happens to be blank

  ;; ;;; Agenda configuration
 ;; ;(setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/git/org/" "~/git/org/projects" "~/git/org/knowledge-vault" "~/git/collabo" "~/git/org/calendar"))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-use-property-inheritance t)
  (setq org-agenda-overriding-columns-format " %8TODO %PRIORITY %10Project %60ITEM ")
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           (
	    (tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda ""
                    ((org-agenda-span 'day)
		     (org-agenda-sorting-strategy
		     '(time-up habit-down priority-down category-keep))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'any))))
            (agenda ""
		    ((org-agenda-tag-filter-preset '("-drill"))
                     (org-agenda-span 2)

                     (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'any )
						    (org-agenda-skip-entry-if 'regexp ":drill:")))
                     (org-agenda-overriding-header "Reminders for today:")))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-if-habit)
                                                     (air-org-skip-if-priority ?A)
						     (org-agenda-skip-entry-if 'todo '("○ NEXT" "NEXT" "WAITING"))
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:")))

            (todo "DONE"
                     ((org-agenda-skip-function 'air-org-skip-if-not-closed-today)
                      (org-agenda-overriding-header "Closed today:"))
                     )
            )
           ;; ((org-agenda-compact-blocks t))
	   )))

  ;(set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")

  (evil-leader/set-key-for-mode 'org-mode
    "$"  'org-archive-subtree
    "a"  'org-agenda
    "l"  'org-toggle-latex-fragment
    ;"c"  'air-org-set-category-property
    "d"  'org-deadline
    ;"ns" 'org-narrow-to-subtree
    ;"p"  'org-set-property
    "p" 'org-pomodoro
    "r"   'org-babel-execute-src-block'
    "s"  'org-schedule
    "t"  'air-org-set-tags
)

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-habit-graph-column 50)
              ;(define-key org-agenda-mode-map "H"          'beginning-of-buffer)
              (define-key org-agenda-mode-map "j"          'org-agenda-next-item)
              (define-key org-agenda-mode-map "k"          'org-agenda-previous-item)
              (define-key org-agenda-mode-map "J"          'air-org-agenda-next-header)
              (define-key org-agenda-mode-map "K"          'air-org-agenda-previous-header)
              ;(define-key org-agenda-mode-map "n"          'org-agenda-next-date-line)
              ;(define-key org-agenda-mode-map "p"          'org-agenda-previous-date-line)
              (define-key org-agenda-mode-map "p"          'org-pomodoro)
              (define-key org-agenda-mode-map "c"          'air-org-agenda-capture)
              ;(define-key org-agenda-mode-map "R"          'org-revert-all-org-buffers)
              ;(define-key org-agenda-mode-map "y"          'air-org-bulk-copy-headlines)
              ;(define-key org-agenda-mode-map "/"          'counsel-grep-or-swiper)
              ;(define-key org-agenda-mode-map (kbd "RET")  'org-agenda-switch-to)

              ;(define-prefix-command 'air-org-run-shortcuts)
              ;(define-key air-org-run-shortcuts "f" (tiny-menu-run-item "org-files"))
              ;(define-key air-org-run-shortcuts "t" (tiny-menu-run-item "org-things"))
              ;(define-key air-org-run-shortcuts "c" (tiny-menu-run-item "org-captures"))
              ;(define-key air-org-run-shortcuts "l" (tiny-menu-run-item "org-links"))
              ;(define-key org-agenda-mode-map (kbd "\\") air-org-run-shortcuts))
              ))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-define-key 'normal org-capture-mode-map "+" 'org-priority-up)
              (evil-define-key 'normal org-capture-mode-map "-" 'org-priority-down)
              ;(evil-define-key '(normal insert) org-capture-mode-map (kbd "C-=" ) 'org-priority-up)
              ;(evil-define-key '(normal insert) org-capture-mode-map (kbd "C--" ) 'org-priority-down)
              ;;; TODO this seems like a hack
              (evil-insert-state)))

  ;(add-hook 'org-mode-hook
            ;(lambda ()
              ;;; Special plain list leader inserts
              ;(dolist (char '("+" "-"))
                ;(define-key org-mode-map (kbd char)
                  ;`(lambda ()
                    ;(interactive)
                    ;(air--org-insert-list-leader-or-self ,char))))

              ;;; Normal maps
              ;(define-key org-mode-map (kbd "C-c d")   (lambda ()
                                                         ;(interactive) (air-org-agenda-toggle-date t)))
              ;(define-key org-mode-map (kbd "C-c ,")   'org-time-stamp-inactive)
              ;(define-key org-mode-map (kbd "C-|")     'air-org-insert-scheduled-heading)
              ;(define-key org-mode-map (kbd "C-\\")    'air-org-insert-heading)
              ;(define-key org-mode-map (kbd "s-r")     'org-revert-all-org-buffers)
              ;(define-key org-mode-map (kbd "C-c C-l") (tiny-menu-run-item "org-links"))

              ;(define-key org-mode-map (kbd "C-<")                'org-shiftmetaleft)
              ;(define-key org-mode-map (kbd "C->")                'org-shiftmetaright)

              ;;; These are set as evil keys because they conflict with
              ;;; existing commands I don't use, or are superseded by
              ;;; some evil function that org-mode-map is shadowed by.
              ;(evil-define-key 'normal org-mode-map (kbd "TAB")   'org-cycle)

              ;(evil-define-key 'normal org-mode-map (kbd "C-,")   'org-metaleft)
              ;(evil-define-key 'normal org-mode-map (kbd "C-.")   'org-metaright)

              ;(evil-define-key 'insert org-mode-map (kbd "C-,")   'org-metaleft)
              ;(evil-define-key 'insert org-mode-map (kbd "C-.")   'org-metaright)

              ;(evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
              ;(evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)

              ;(evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
              ;(evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

              ;;; Navigation
              ;(define-key org-mode-map (kbd "M-h") 'org-up-element)
              ;(define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
              ;(define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
              ;(define-key org-mode-map (kbd "M-l") 'air-org-goto-first-child)

              ;;; Use fill column, but not in agenda
              ;(setq fill-column 100)
              ;(when (not (eq major-mode 'org-agenda-mode))
                ;(visual-line-mode)
                ;(visual-fill-column-mode))
              ;(flyspell-mode)
              ;(org-indent-mode)))
)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

(use-package gnuplot
  :commands gnuplot-mode
  :defer t
  :ensure t)

(use-package org-pomodoro
  :ensure t
  :defer t
  :config
  (setq org-pomodoro-start-sound-p t)
  (setq org-pomodoro-length 25)

    (setq org-pomodoro-keep-killed-pomodoro-time nil)
    (setq org-pomodoro-finished-sound-p t)
    (setq org-pomodoro-short-break-sound-p t)
    (setq org-pomodoro-long-break-sound-p t)
    (setq org-pomodoro-ticking-sound-p nil))

(use-package sound-wav
  :ensure t)

(use-package powershell
  :if window-system
  :ensure t)

(use-package org-caldav
  :after (org org-agenda)
  :ensure t
  :config
  (setq
   org-caldav-calendars
   `((:calendar-id "org" ; name of calendar in nextcloud
      :inbox "~/git/org/calendar/office.org"
      :url "https://nextcloud.palaimon.io/remote.php/dav/calendars/Immanuel/"
      ;; :files ,(org-agenda-files)	;
      :files
      ;; :select-tags '("shared")
      )
     (:calendar-id "palaimon" ; name of calendar in nextcloud
      :inbox "~/git/org/calendar/palaimon.org"
      :url "https://nextcloud.palaimon.io/remote.php/dav/calendars/Immanuel/"
      :files
      )
     )
   ;; org-caldav-delete-org-entries 'ask
   ;; org-caldav-save-directory (concat eqyiel/xdg-cache-home "/emacs")
   org-icalendar-include-todo t
   org-icalendar-timezone "Europe/Berlin"
   org-icalendar-use-scheduled '(event-if-todo)
   ))


(provide 'init-org)
;;; init-org.el ends here
