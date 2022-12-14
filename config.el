;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Vikas Mishra"
      user-mail-address "vikas.mishra@hey.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 18)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 18)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 ))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-ir-black)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/OrgNotes/")
(setq org-roam-directory "~/Documents/OrgNotes/roam/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Some basic configurations
(setq-default delete-by-moving-to-trash t               ; Delete files to trash
              window-combination-resize t)              ; take new window space from all other windows (not just current)

(setq truncate-string-ellipsis "…"                      ; Unicode ellipsis looks better
      auto-save-default t                               ; Save files by default
      scroll-margin  2                                  ; Save some margin while scrolling up/down.
)

(display-time-mode 1)                                   ; Show time in the modeline
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))

;; Directory for my customizations
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use C-x C-m as M-x command
(map! "C-x C-m" #'execute-extended-command)

;; Unmap some of doom's keysmaps. I use S-l for LSP
(map! "s-l" nil)

;; LSP configuration and Debugger configuration
;; Use debugpy for python debugging
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package lsp-mode
  :defer t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "s-l"))
                        (lsp-enable-which-key-integration))))
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
        lsp-client-packages nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-eldoc-enable-hover t ; Disable eldoc displays in minibuffer
        lsp-ui-imenu-enable t
        lsp-ui-peek-always-show t
        lsp-ui-sideline-ignore-duplicate t
        lsp-headerline-breadcrumb-enable t)
  :config
  (define-key lsp-mode-map (kbd "s-l") lsp-command-map))

(after! lsp-ui
  (setq lsp-ui-doc-enable t))

;; I want to add tabnine to the company backend.
;; Enable tabnine for company
(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  )

;; Set which-key-idle-delay
(setq which-key-idle-delay 0.4)
(setq which-key-idle-secondary-delay 0.01)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Winum for switching between windows easily
(with-eval-after-load 'winum
  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
  (define-key winum-keymap (kbd "C-`") 'winum-select-window-by-number)
  )

;; Treesitter makes the packages look much better
(use-package tree-sitter
  :after python-mode
  :defer t
  :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  )

;; Enable tree-sitter
(use-package! tree-sitter
  :hook
  (prog-mode . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; =================================================
;; Configuration for org mode and other allied modes
;; =================================================
(after! org
  (setq org-image-actual-width 1500)
  ;; I want to open org link in other windows - not the same window.
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)

  ;; I want to log when I mark a task as done
  (setq org-log-done 'time)

  ;; Set my sequence of todo things
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; Task that's ready to be next. No dependencies
           "NEXT(n)"  ; A task that needs doing & is ready to do
           "PROG(p)"  ; A project, which usually contains other tasks
           "WAIT(w)"  ; Something external is holding up this task
           "INTR(i)"  ; Interrupt
           "SOMEDAY(s)" ; Someday
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")))

  ;; Org Tags
  (setq org-tag-alist '(
                        ;; Meeting tags
                        ("Team" . ?t)
                        ("Laguna" . ?l)
                        ("Redondo" . ?r)
                        ("Newport" . ?n)
                        ("Meeting". ?m)
                        ("Planning" . ?p)

                        ;; Work Log Tags
                        ("accomplishment" . ?a)
                        ))

  ;; Tag colors
  (setq org-tag-faces
        '(
          ("Team"  . (:foreground "mediumPurple1" :weight bold))
          ("Laguna"   . (:foreground "royalblue1"    :weight bold))
          ("Redondo"  . (:foreground "light green"  :weight bold))
          ("Newport"        . (:foreground "lightSalmon1"        :weight bold))
          ("Meeting"   . (:foreground "yellow1"       :weight bold))
          ("Planning" . (:foreground "Orange" :weight bold))
          ("CRITICAL"  . (:foreground "red1"          :weight bold))
          )
        )
  ;; My org-capture templates
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("l" "Laguna Todos" entry
           (file+headline "~/Documents/OrgNotes/Laguna_TODO.org" "Laguna TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("r" "Redondo Todos" entry
           (file+headline "~/Documents/OrgNotes/Redondo_TODO.org" "Redondo TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Newport Todos" entry
           (file+headline "~/Documents/OrgNotes/Newport_TODO.org" "Newport TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("i" "IP Team Todos" entry
           (file+headline "~/Documents/OrgNotes/Team_TODO.org" "Team TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("m" "Meeting"
           entry (file+datetree "~/Documents/OrgNotes/Meetings.org")
           "* %? :meeting:%^g \n:Created: %T\n** Attendees\n+ \n** Notes\n+ \n** Action Items\n*** TODO [#A] "
           :tree-type week
           :clock-in t
           :clock-resume t
           :empty-lines 0)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)))

  ;; Enable global org-modern-mode
  (setq
   ;; Edit settings
   org-auto-align-tags t
   org-tags-column 0
   org-modern-tag nil
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis " ▼ "
   org-modern-star '("◉" "○" "◈" "◇" "◇" "◇" "*")
   org-modern-priority nil
   org-modern-todo nil
   org-modern-timestamp nil

   ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-sorting-strategy '(deadline-up)
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode))


;; Set some registers for critical files
(set-register ?t (cons 'file "~/Documents/OrgNotes/todo.org"))
(set-register ?l (cons 'file "~/Documents/OrgNotes/Laguna_TODO.org"))
(set-register ?r (cons 'file "~/Documents/OrgNotes/Redondo_TODO.org"))
(set-register ?n (cons 'file "~/Documents/OrgNotes/Newport_TODO.org"))
(set-register ?i (cons 'file "~/Documents/OrgNotes/Team_TODO.org"))

;; Org Roam Custom Setup
(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil
        org-id-link-to-org-use-id t
        org-roam-mode-section-functions (list #'org-roam-backlinks-section
                                              #'org-roam-reflinks-section
                                              #'org-roam-unlinked-references-section))

  ;; Org roam immediate insert node
  ;; Insert node without prompting a capture buffer
  ;; Bind this to C-c n r I (think C-c n r i -> insert node with capture buffer)
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  (map! "C-c n r I" :desc "Insert Node immediate" 'org-roam-node-insert-immediate)

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))

  )

;; Org Roam Dailies
(after! org-roam-dailies
  (setq org-roam-dailies-directory "daily")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "** %<%I:%M %p>: %?"
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: daily\n\n* Focus\n\n* Tasks\n\n* Journal"
                                  ("Journal"))
           :unnarrowed t))))

;;
;; Enable org-roam-ui
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(use-package! consult-org-roam
  :after org-roam
  :init
  ;; Require consult-org-roam
  (require 'consult-org-roam)
  ;; Activate the minor mode.
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n g" . consult-org-roam-search))


;; Location for my custom emacs files.
(use-package! vm-agenda
  :load-path "/Users/vikmishra/.doom.d/lisp")

;; Super agenda - this looks neat.
;; Can be optimized further. But we will live with this for the moment.
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        ;; org-agenda-span week
        org-agenda-start-on-weekday 1)
  (setq org-agenda-custom-commands
        '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
          ("n" "Agenda / INTR / PROG / NEXT /TODO /SOMEDAY"
           ((agenda "" nil)
            (todo "INTR" nil)
            (todo "PROG" nil)
            (todo "NEXT" nil)
            (todo "TODO" nil)
            (todo "SOMEDAY" nil)
            )
           nil)
          ("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                             :file-path "refile\\.org")
                            (:name "Next to do"
                             :todo "NEXT"
                             :order 1)
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Important"
                             :priority "A"
                             :order 3)
                            (:name "Overdue"
                             :deadline past
                             :order 4)
                            (:name "Due Soon"
                             :deadline future
                             :order 5)
                            (:name "Scheduled Soon"
                             :scheduled future
                             :order 6)
                            (:name "Laguna"
                             :tag "Laguna"
                             :order 7)
                            (:name "Newport"
                             :tag "Newport"
                             :order 8)
                            (:name "Meeting"
                             :tag "Meeting"
                             :order 9)
                            (:name "Redondo"
                             :tag "Redondo"
                             :order 10)
                            (:name "IP Team"
                             :tag "Team"
                             :order 11)
                            (:name "Meetings"
                             :and (:todo "MEET" :scheduled future)
                             :order 15)
                            (:discard (:not (:todo "TODO")))))))))
          ("w" "My Workday"
	   ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (todo "" ((org-agenda-overriding-header "Overview")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "NEXT ___"
                                   :todo "NEXT"
				   :face (:background "chocolate3")
                                   :order 1)
    			    (:name "Due Today ___"
                                   :deadline today
                                   :order 2)
			    (:name "Due this week ___"
                                   :deadline 7
			           :order 3)
			    (:name "Ongoing ___"
                                   :scheduled t
                                   :order 4)
                            (:name "Overdue ___"
                                   :deadline past
                                   :order 5)
			    (:name "Tasks with Due Dates ___"
                                   :deadline future
                                   :order 6)
			    (:name "Tasks without Due Dates ___"
                                   :deadline nil
				   :order 7)
			    (:discard (:anything t))))))))))
  :config
  (org-super-agenda-mode))

(use-package! consult-notes
  :commands (
             consult-notes-search-in-all-notes
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-sources
        '(("Org"             ?o "~/Documents/OrgNotes")))
  ;; set org-roam integration
  (consult-notes-org-roam-mode))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-appear-autoentities t
           org-appear-autosubmarkers t ))


(use-package! ox-pandoc
  :config
  (setq org-pandoc-options-for-html5 '((template . "Github.html5"))))

;; Show the TOC on the side.
(use-package! org-ol-tree
  :after org
  :commands org-ol-tree
  :hook (org-ol-tree-mode . visual-line-mode)
  :config
  (setq org-ol-tree-ui-window-auto-resize nil
        org-ol-tree-ui-window-max-width 0.3))
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)


(use-package! vm-custom-functions
  :load-path "/Users/vikmishra/.doom.d/lisp")

(map! "C-c n C-d" 'insert-current-date-time)
(map! "C-c n C-t" 'insert-current-time-for-journal)


;; Make some keys equivalent to the evil mode.
;; C-c SPC to open file in project
(map! :leader
      :desc "Switch buffer within project"      ","     #'persp-switch-to-buffer
      :desc "Switch buffer"                     "<"     #'switch-to-buffer
      :desc "Search project"                    "/"     #'+default/search-project
      :desc "Search for Symbol in project"      "*"     #'+default/search-project-for-symbol-at-point
      :desc "Find File"                         "."     #'find-file
      :desc "Org Capture"                       "x"     #'org-capture
      :desc "Jump to Bookmark"                  "RET"   #'bookmark-jump
      :desc "Pop up scratch buffer"             "X"     #'doom/open-scratch-buffer
      :desc "Find file in project"              "SPC"   #'projectile-find-file
      :desc "Resume last search"                "'"     #'vertico-repeat
      ;; Create ID for the current entry
      :desc "Create ID for current entry"       "n r o" #'org-id-get-create
      ;; Add a tag
      :desc "Add tags to the Node"              "n r A" #'org-roam-tag-add
      :desc "Use avy-goto-char-2 to jump"       ";"     #'avy-goto-char-2
      )

(map! "s-t" 'org-roam-dailies-goto-today)
(map! "s-d" 'org-roam-dailies-find-date)
(map! "s-u" 'consult-notes-search-in-all-notes)

;; Add icon to doom modeline
(use-package! doom-modeline
  :config
  (setq doom-modeline-major-mode-icon t)
  )

;; Configure good-scroll
(use-package! good-scroll
  :init (good-scroll-mode 1))
(global-set-key (kbd "M-v") 'good-scroll-down-full-screen)
(global-set-key (kbd "C-v") 'good-scroll-up-full-screen)


;; Goto line preview
(global-set-key [remap goto-line] 'goto-line-preview)

;; Configure zop-to-char
(global-set-key [remap zap-to-char] 'zop-to-char)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.0 :box (:line-width 10 :color "red"))
    )

;; Setup dirvish
(use-package! dirvish
  :config
  (setq dirvish-hide-details nil)
  (dirvish-override-dired-mode)
  )

(add-hook 'org-mode-hook (lambda ()
                           (company-mode -1)
                           (setq fill-column 150)
                           (visual-fill-column-mode)
                           (setq display-line-numbers nil)))


(setq line-spacing 10)
