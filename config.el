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
(setq doom-font (font-spec :family "CaskaydiaCove NF" :size 19 )
      doom-unicode-font (font-spec :family "CaskaydiaCove NF" :size 19 )
      doom-variable-pitch-font (font-spec :family "Bear Sans UI" :size 18))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-day)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Doom's scratch buffer mode
(setq-default doom-scratch-initial-major-mode 'lisp-interaction-mode)


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
(setq-default line-spacing 5)
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
;; Enable tree-sitter
(use-package! tree-sitter
  :hook
  (prog-mode . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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
      :desc "Use avy-goto-char-2 to jump"       "'"     #'avy-goto-char-2
      )

(map! "s-t" 'org-roam-dailies-goto-today)
(map! "s-d" 'org-roam-dailies-find-date)
(map! "s-u" 'consult-org-roam-search)

;; Add icon to doom modeline
(use-package! doom-modeline
  :config
  (setq doom-modeline-major-mode-icon t)
  )

;; Goto line preview
(global-set-key [remap goto-line] 'goto-line-preview)

;; Configure zop-to-char
(global-set-key [remap zap-to-char] 'zop-to-char)

;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
(pixel-scroll-precision-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.


;; Try a better search package
(use-package! ctrlf
  :hook
  (after-init . ctrlf-mode))

;; Create google link for what I need.
(defalias 'linkify
  (kmacro "C-w [ [ h t t p : C-y C-/ / / C-y C-f [ C-y C-e SPC"))

;; My custom keyboard shortcuts
(map! :leader
      (:prefix-map ("m" . "My Custom Shortcuts")
       :desc "Add to personal directory" "a" #'+spell/add-word
       :desc "Google Linkify" "l" #'linkify))



;; ===================================================
;; Configuration for org mode and other allied modes
;; ===================================================
(setq ispell-personal-dictionary "~/.doom.d/vikas.pws")

(defun vm-org-faces ()
  (set-face-attribute 'org-level-1 nil :height 1.25)
  (set-face-attribute 'org-level-2 nil :height 1.15)
  (set-face-attribute 'org-level-3 nil :height 1.1))

;; Use electric pair mode for Org mode
(electric-pair-mode 1)
; (defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=)
;                             (?\_ . ?\_) (?~ . ?~) (?+ . ?+)) "Electric pairs for org-mode.")

;(defun org-add-electric-pairs ()
;  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
;  (setq-local electric-pair-text-pairs electric-pair-pairs))
;
(require 'wrap-region)
(add-hook 'org-mode-hook #'wrap-region-mode)

(wrap-region-add-wrapper "=" "=" nil 'org-mode) ; select region, hit = then region -> =region= in org-mode
(wrap-region-add-wrapper "~" "~" nil 'org-mode) ; select region, hit ~ then region -> ~region~ in org-mode
(wrap-region-add-wrapper "*" "*" nil 'org-mode) ; select region, hit * then region -> *region* in org-mode
(wrap-region-add-wrapper "/" "/" nil 'org-mode) ; select region, hit / then region -> /region/ in org-mode
(wrap-region-add-wrapper "_" "_" nil 'org-mode) ; select region, hit _ then region -> _region_ in org-mode
(wrap-region-add-wrapper "+" "+" nil 'org-mode) ; select region, hit + then region -> +region+ in org-mode

(add-hook 'org-mode-hook (lambda ()
                           (setq fill-column 120)
                           (visual-fill-column-mode)
                           ;(org-add-electric-pairs)
                           (vm-org-faces)
                           (setq display-line-numbers nil)))

(defvar +org-roam-open-buffer-on-find-file t)

(after! org
  ;; I want to open org link in other windows - not the same window.
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)

  ;; I want to log when I mark a task as done
  (setq org-log-done 'time)

  ;; Set my sequence of todo things
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; Input todo
           "NEXT(n)"  ; Task that I can start on next. No dependencies
           "WAIT(w)"  ; Something external is holding up this task
           "PROG(p)"  ; Work in progress.
           "SOMEDAY(s)" ; Someday
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          ))

  ;; Org Tags
  (setq org-tag-alist '(
                        ;; Meeting tags
                        ("Team" . ?t)
                        ("Laguna" . ?l)
                        ("Malibu" . ?n)
                        ("Meeting". ?m)
                        ("Personal" . ?p)

                        ;; Work Log Tags
                        ("accomplishment" . ?a)
                        ))

  ;; Tag colors
  (setq org-tag-faces
        '(
          ("Team"  . (:foreground "mediumPurple1" :weight bold))
          ("Laguna"   . (:foreground "royalblue1"    :weight bold))
          ("Redondo"  . (:foreground "Dark green"  :weight bold))
          ("Malibu"        . (:foreground "lightSalmon1"        :weight bold))
          ("Meeting"   . (:foreground "yellow1"       :weight bold))
          ("Personal" . (:foreground "Orange" :weight bold))
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
          ("n" "Malibu Todos" entry
           (file+headline "~/Documents/OrgNotes/Malibu_TODO.org" "Malibu TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("i" "IP Team Todos" entry
           (file+headline "~/Documents/OrgNotes/Team_TODO.org" "Team TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("m" "Meeting"
           entry (file+datetree "~/Documents/OrgNotes/Meetings.org")
           "* %? :meeting:%^g \n:Created: %T\n** Attendees\n+ \n** Notes\n+ \n** Action Items\n*** TODO "
           :tree-type week
           :clock-in t
           :clock-resume t
           :empty-lines 0)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t))))

(map! :after org :map org-mode-map "M-n" #'org-forward-heading-same-level)
(map! :after org :map org-mode-map "M-p" #'org-backward-heading-same-level)



;; Set some registers for critical files
(set-register ?t (cons 'file "~/Documents/OrgNotes/todo.org"))
(set-register ?l (cons 'file "~/Documents/OrgNotes/Laguna_TODO.org"))
(set-register ?r (cons 'file "~/Documents/OrgNotes/Redondo_TODO.org"))
(set-register ?m (cons 'file "~/Documents/OrgNotes/Malibu_TODO.org"))
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

  (defun my/preview-fetcher ()
    (let* ((elem (org-element-context))
           (parent (org-element-property :parent elem)))
      ;; TODO: alt handling for non-paragraph elements
      (string-trim-right (buffer-substring-no-properties
                          (org-element-property :begin parent)
                          (org-element-property :end parent)))))

  (setq org-roam-preview-function #'my/preview-fetcher)

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))

  )

;; Org Roam Dailies
(after! org-roam-dailies
  (setq org-roam-dailies-directory "")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n** %<%I:%M %p> - %?"
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
;; Add journal to agenda
(use-package! vm-agenda
  :load-path "/Users/vikmishra/.doom.d/lisp")
(use-package! gogolink
  :load-path "/Users/vikmishra/.doom.d/lisp")
;(setq org-agenda-files (append '("~/Documents/OrgNotes/" "~/Documents/OrgNotes/roam/" "~/Documents/OrgNotes/journal/")))

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
        '(;; other commands here
          ("l" "PROG / TODO / WAIT /SOMEDAY"
           ((todo "PROG" nil)
            (todo "TODO" nil)
            (todo "WAIT" nil)
            (todo "SOMEDAY" nil)
            )
           nil)
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
                         (:name "NEXT \n ===="
                          :todo "NEXT"
                          :face (:background "chocolate3")
                          :order 1)
                         (:name "Due Today \n ========="
                          :deadline today
                          :order 2)
                         (:name "Due this week \n ============="
                          :deadline 7
                          :order 3)
                         (:name "Ongoing \n ======="
                          :scheduled t
                          :order 4)
                         (:name "Overdue \n ======="
                          :deadline past
                          :order 5)
                         (:name "Tasks with Due Dates \n ===================="
                          :deadline future
                          :order 6)
                         (:name "Tasks without Due Dates \n ========================"
                          :deadline nil
                          :order 7)
                         (:name "Laguna \n ======"
                          :tag "Laguna"
                          :order 8)
                         (:name "Malibu \n ======"
                          :tag "Malibu"
                          :order 9)
                         (:name "SSWRP and IP Team \n ================="
                          :tag "Team"
                          :order 10)
                         (:name "My ToDos \n ========"
                          :tag "Personal"
                          :order 11)
                         (:discard (:anything t))))))))
          ("k" "Kategories"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (todo "" ((org-agenda-overriding-header "Overview")
                      (org-super-agenda-groups
                       '((:log t)
                         (:name "Laguna \n ======"
                          :tag "Laguna"
                          :order 1)
                         (:name "Malibu \n ======"
                          :tag "Malibu"
                          :order 2)
                         (:name "SSWRP and IP Team \n ================="
                          :tag "Team"
                          :order 3)
                         (:name "My ToDos \n ========"
                          :tag "Personal"
                          :order 4)
                         (:discard (:anything t))))))))))
  :config
  (org-super-agenda-mode))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-hide-emphasis-markers t
           org-appear-autoentities t
           org-appear-autosubmarkers t ))

(use-package! ox-pandoc
  :config
  (setq org-pandoc-options-for-html5 '((template . "Github.html5"))))

;; Org LaTeX export template
;; Material and configuration shamelessly copied from - https://so.nwalsh.com/2020/01/05-latex
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      (list (concat "latexmk -"
                    org-latex-compiler
                    " -recorder -synctex=1 -bibtex-cond %b")))
(setq org-latex-listings t)
(setq org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))

(setq org-latex-classes
      '(("article"
         "\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{ETBembo RomanOSF}
\\setsansfont[Scale=MatchLowercase]{Raleway}
\\setmonofont[Scale=MatchLowercase]{OperatorMonoSSm Nerd Font}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
        framesep=.5em,backgroundcolor=\\color{AliceBlue},
        rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{literate=
            {§}{{\\S}}1
            {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
            {«}{{\\guillemotleft}}1
            {»}{{\\guillemotright}}1
            {Á}{{\\'A}}1
            {Ä}{{\\\"A}}1
            {É}{{\\'E}}1
            {Í}{{\\'I}}1
            {Ó}{{\\'O}}1
            {Ö}{{\\\"O}}1
            {Ú}{{\\'U}}1
            {Ü}{{\\\"U}}1
            {ß}{{\\ss}}2
            {à}{{\\`a}}1
            {á}{{\\'a}}1
            {ä}{{\\\"a}}1
            {é}{{\\'e}}1
            {í}{{\\'i}}1
            {ó}{{\\'o}}1
            {ö}{{\\\"o}}1
            {ú}{{\\'u}}1
            {ü}{{\\\"u}}1
            {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
            {ı}{{\\i}}1
            {—}{{---}}1
            {’}{{'}}1
            {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
            {␣}{{\\textvisiblespace}}1,
            keywordstyle=\\color{DarkGreen}\\bfseries,
            identifierstyle=\\color{DarkRed},
            commentstyle=\\color{Gray}\\upshape,
            stringstyle=\\color{DarkBlue}\\upshape,
            emphstyle=\\color{Chocolate}\\upshape,
            showstringspaces=false,
            columns=fullflexible,
            keepspaces=true}
\\usepackage[a4paper,margin=1in,left=1.5in]{geometry}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

        ("report" "\\documentclass[11pt]{report}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

        ("book" "\\documentclass[11pt]{book}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package! vm-custom-functions
  :load-path "/Users/vikmishra/.doom.d/lisp")

(map! "C-c n C-d" 'insert-current-date-time)
(map! "C-c n C-t" 'insert-current-time-for-journal)

;; Dired Dotfiles hide
(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode))

;; To toggle hiding
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(use-package! embark)
(after! embark
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (embark-open-externally (dired-get-filename)) arg)))

(map! (:after dired
              (:map dired-mode-map
               :desc "Open File Externally" "E" #'dired-open-externally)))

;; Use bash for the emacs commands.
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Set the default apps for opening type of files.
(setq org-file-apps
      '((remote . emacs)
        (auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.png\\'" . default)
        ("\\.jpg\\'" . default)
        ("\\.pdf\\'" . default)))

(after! unicode-fonts
  (push "Symbola" (cadr (assoc "Miscellaneous Symbols" unicode-fonts-block-font-mapping))))

(setq deft-directory "~/Documents/OrgNotes/")
