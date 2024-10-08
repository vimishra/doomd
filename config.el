;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "Vikas Mishra"
      user-mail-address "vikas.mishra@hey.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
      doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "Google Sans" :size 25 :weight 'regular))

;; Setup the doom modeline
(setq doom-modeline-hud nil)
(setq doom-modeline-support-imenu t)

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vikas)
;; Uncomment the below two lines after commenting the one above if you want to
;; use the modus theme
;; (use-package! vm-modus-custom
;; 	:load-path "~/.doom.d/lisp")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Doom's scratch buffer mode
(setq-default doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/OrgNotes")
(setq org-roam-directory "~/Documents/OrgNotes/roam")

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

;; My configuration starts
(setq-default delete-by-moving-to-trash t               ; Delete files to trash
              window-combination-resize t)              ; take new window space from all other windows (not just current)

(setq truncate-string-ellipsis "…"                      ; Unicode ellipsis looks better
      auto-save-default t                               ; Save files by default
      scroll-margin  2)                                  ; Save some margin while scrolling up/down.

(setq-default line-spacing 5)
(display-time-mode 1)                                   ; Show time in the modeline


;; Directory for my customizations
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use C-x C-m as M-x command
(map! "C-x C-m" #'execute-extended-command)

; LSP configuration and Debugger configuration
; Need to figure out how to debug pythonPath
; Use debugpy for python debugging
; (after! dap-mode
;  (setq dap-python-debugger 'debugpy))

;; Set which-key-idle-delay. Don't take a lot of time before you pop up the
;; keymap
(setq which-key-idle-delay 0.4)
(setq which-key-idle-secondary-delay 0.01)

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
  (define-key winum-keymap (kbd "C-`") 'winum-select-window-by-number))


;; Treesitter makes the packages look much better
;; Enable tree-sitter
(use-package! tree-sitter
  :hook
  (prog-mode . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Custom Keyboard commands
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
      (:prefix-map ("m" . "My Custom Shortcuts")
       :desc "Add to personal directory" "a" #'+spell/add-word
       :desc "Google Linkify" "l" #'linkify))

(setq byte-compile-warnings nil)

;; Add icon to doom modeline
(use-package! doom-modeline
  :config
  (setq doom-modeline-major-mode-icon t))

;; Goto line preview
(global-set-key [remap goto-line] 'goto-line-preview)
;; Configure zop-to-char
(global-set-key [remap zap-to-char] 'zop-to-char)

;; Try a better search package
(use-package! ctrlf
  :hook
  (after-init . ctrlf-mode))


;; ===================================================
;; Configuration for org mode and other allied modes
;; ===================================================
(setq ispell-personal-dictionary "~/.doom.d/vikas.pws")

;; Use electric pair mode for Org mode
(electric-pair-mode 1)

(require 'wrap-region)
(add-hook 'org-mode-hook #'wrap-region-mode)
(add-hook 'org-mode-hook #'org-make-toc-mode)


;; Wrap the region in markup chars
(wrap-region-add-wrapper "=" "=" nil 'org-mode) ; select region, hit = then region -> =region= in org-mode
(wrap-region-add-wrapper "~" "~" nil 'org-mode) ; select region, hit ~ then region -> ~region~ in org-mode
(wrap-region-add-wrapper "*" "*" nil 'org-mode) ; select region, hit * then region -> *region* in org-mode
(wrap-region-add-wrapper "/" "/" nil 'org-mode) ; select region, hit / then region -> /region/ in org-mode
(wrap-region-add-wrapper "_" "_" nil 'org-mode) ; select region, hit _ then region -> _region_ in org-mode
(wrap-region-add-wrapper "+" "+" nil 'org-mode) ; select region, hit + then region -> +region+ in org-mode

;; Prettify symbols list
(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        '(("lambda" . "λ")
          ("|>" . "▷")
          ("<|" . "◁")
          ("->>" . "↠")
          ("->" . "→")
          ("<-" . "←")
          ("=>" . "⇒")
          ("<=" . "≤")
          (">=" . "≥")))
  (prettify-symbols-mode 1))
                                        ; Org mode hooks
(add-hook 'org-mode-hook (lambda ()
                           (setq fill-column 140)
                           (visual-fill-column-mode)
                           (my/org-mode/load-prettify-symbols)
                           (setq display-line-numbers nil)))

(defvar +org-roam-open-buffer-on-find-file t)

(after! org
  ;; I want to open org link in other windows - not the same window.
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)

  ;; I want to log when I mark a task as done
  (setq org-log-done 'time)

  ;; I don't want to export the below while exporting
  ;; While exporting org-roam, we need to use org-roam-export
  (setq org-export-with-tags nil)
  (setq org-export-with-author nil)
  (setq org-export-with-title nil)
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts "{}")
  (setq org-image-actual-width t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-image-max-width 800)
  (setq org-M-RET-may-split-line '((default . t)))
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
           "KILL(k)"))) ; Task was cancelled, aborted or is no longer applicable


  ;; Org Tags
  (setq org-tag-alist '(
                        ;; Meeting tags
                        ("Team" . ?t)
                        ("Laguna" . ?l)
                        ("Malibu" . ?n)
                        ("Meeting". ?m)
                        ("Personal" . ?p)
                        ("LaJolla" . ?j)

                        ;; Work Log Tags
                        ("accomplishment" . ?a)))


  ;; Tag colors
  (setq org-tag-faces
        '(
          ("Team"  . (:foreground "mediumPurple1" :weight bold))
          ("Laguna"   . (:foreground "royalblue1"    :weight bold))
          ("LaJolla"  . (:foreground "Dark green"  :weight bold))
          ("Malibu"        . (:foreground "lightSalmon1"        :weight bold))
          ("Meeting"   . (:foreground "yellow1"       :weight bold))
          ("Personal" . (:foreground "Orange" :weight bold))
          ("CRITICAL"  . (:foreground "red1"          :weight bold))))


  (setq +org-capture-todo-file "~/Documents/OrgNotes/roam/20240728160335-my_todos.org")
  (setq org-capture-templates
        '(("i" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("l" "LaJolla Todos" entry
           (file+headline "~/Documents/OrgNotes/roam/20240728150920-lajolla_todo.org" "LaJolla TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("m" "Malibu Todos" entry
           (file+headline "~/Documents/OrgNotes/roam/20240728151300-malibu_todos.org" "Malibu TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("t" "Team Todos" entry
           (file+headline "~/Documents/OrgNotes/roam/20240729153639-team_todos.org" "Team TODOs")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Meeting"
           entry (file+datetree "~/Documents/OrgNotes/roam/20240728151824-meetings.org")
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
(set-register ?t (cons 'file "~/Documents/OrgNotes/roam/20240729153639-team_todos.org"))
(set-register ?l (cons 'file "~/Documents/OrgNotes/roam/20240728150920-lajolla_todo.org"))
(set-register ?m (cons 'file "~/Documents/OrgNotes/roam/20240728151300-malibu_todos.org"))
(set-register ?i (cons 'file "~/Documents/OrgNotes/roam/20240728160335-my_todos.org"))

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
                   (my/org-roam-copy-todo-to-today)))))



;; Org Roam Dailies
(after! org-roam-dailies
  (setq org-roam-dailies-directory "")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+datetree "journal_2024.org" week)))))

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

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-hide-emphasis-markers t
           org-appear-autoentities t
           org-appear-autosubmarkers t))

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


(map! "C-c n C-d" 'insert-current-date-time)
(map! "C-c n C-t" 'insert-current-time-for-journal)

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

;; Org modern setup
(use-package! org-modern
  :ensure t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "*"))
  (org-modern-block-fringe 10)
  (org-modern-table nil)
  (org-modern-timestamp nil)
  (org-modern-todo nil)
  (org-ellipsis "↴")
  :custom-face
  (org-modern-label
   ((t :height 1.0 :weight semi-bold
       :underline nil :inherit default))))
(global-org-modern-mode)

(defun date-one-week-from-today ()
  (interactive)
  "Returns the date one week from today as a string."
  (let ((one-week-from-today (time-add (current-time) (days-to-time 7))))
    (format-time-string "%Y-%m-%d" one-week-from-today)))

;; Org sorting priorities
(setq org-agenda-sorting-strategy
      '((agenda time-up category-keep priority-down)
        (todo priority-down scheduled-up)
        (tags priority-down)))

;; Custom color for PROG status
(setq org-todo-keyword-faces
      '(("PROG" . (:foreground "plum" :weight bold))))
;; Hide the deadline prewarning prior to scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

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
          ("w" "My Workday"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-agenda-overriding-columns-format "%25ITEM %SCHEDULED %TAGS")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (todo "" ((org-agenda-overriding-header "Overview")
                      (org-agenda-overriding-columns-format "%25ITEM %SCHEDULED %TAGS")
                      (org-super-agenda-groups
                       '((:log t)
                         (:name "Overdue \n ======="
                          :deadline past
                          :order 1)
                         (:name "NEXT \n ===="
                          :todo "NEXT"
                          :face (:background "chocolate3")
                          :order 2)
                         (:name "Due Today \n ========="
                          :deadline today
                          :order 3)
                         ;; (:name "Due this week \n ============="
                         ;;  :deadline (before "2024-04-02")
                         ;;  :order 3)
                         (:name "Tasks with Due Dates \n ===================="
                          :deadline future
                          :order 4)                         ;;
                         (:name "Scheduled Soon \n =============="
                          :scheduled future
                          :order 5)
                         (:name "Tasks without Due Dates \n ========================"
                          :deadline nil
                          :scheduled nil
                          :order 7)
                         (:discard (:anything t))))))))
          ("n" "My Weekly Agenda"
           ((agenda "" nil)
            (todo "PROG" nil)
            (todo "TODO" nil)
            (todo "DONE" nil))
           nil)
          ("q" "Quick Agenda"
           ((todo "PROG" nil)
            (todo "TODO" nil)
            (todo "DONE" nil))
           nil)
          ("l" "Task List"
           ((todo "" ((org-agenda-overriding-header "Overview")
                      (org-agenda-overriding-columns-format "%25ITEM %SCHEDULED %TAGS")
                      (org-super-agenda-groups
                       '((:log t)
                         (:name "Overdue"
                          :deadline past
                          :face (:background "DarkOrchid4" :foregroun "black")
                          :order 1)
                         (:name "NEXT"
                          :todo "NEXT"
                          :face (:background "chocolate3")
                          :order 2)
                         (:name "Due Today"
                          :deadline today
                          :order 3)
                         ;; (:name "Due this week \n ============="
                         ;;  :deadline (before "2024-04-02")
                         ;;  :order 3)
                         (:name "Tasks with Due Dates"
                          :deadline future
                          :order 4)                         ;;
                         (:name "Scheduled Soon"
                          :scheduled future
                          :order 5)
                         (:name "Tasks without Due Dates"
                          :deadline nil
                          :scheduled nil
                          :order 7)
                         (:discard (:anything t))))))))
          ("p" "Project View"
           ((todo "" ((org-agenda-overriding-header "Overview")
                      (org-agenda-overriding-columns-format "%25ITEM %SCHEDULED %TAGS")
                      (org-agenda-sorting-strategy '(priority-down scheduled-up))
                      (org-super-agenda-groups
                       '((:auto-group t)))))))))

  :config
  (org-super-agenda-mode))

;; ob-mermaid
(use-package! ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc"))

(use-package! org-roam-export)


;; Location for my custom emacs files.
;; Add journal to agenda
(use-package! vm-agenda
  :load-path "~/.doom.d/lisp")
(use-package! gogolink
  :load-path "~/.doom.d/lisp")
(use-package! bsv-mode
  :load-path "~/.doom.d/lisp")
(use-package! vm-custom-functions
  :load-path "~/.doom.d/lisp")

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
(setq vterm-buffer-name-string "vterm: %s")
(setq-default explicit-shell-file-name (executable-find "fish"))
(exec-path-from-shell-initialize)

;; SLIME for Common Lisp
(setq inferior-lisp-program "sbcl")

(custom-theme-set-faces!
  'doom-vikas
  '(org-level-1 :inherit outline-1 :height 1.25)
  '(org-level-2 :inherit outline-2 :height 1.15 :foreground "#D2B57F")
  '(org-level-3 :inherit outline-3 :height 1.1 :weight normal :foreground "#82B0EC")
  '(org-level-4 :inherit outline-4 :height 1.0 :weight normal :foreground "#FEABCF")
  '(org-document-title  :height 1.25 :underline nil)
  '(default :background "#101010" :foreground "#d5d8d6")
  '(ctrlf-highlight-passive :background "#383838")
  '(bold :foreground "#FF8058" :weight bold)
  '(italic :foreground "light green" :slant italic)
  '(underline :foreground "bisque1" :underline t))

;; (custom-theme-set-faces!
;;    'modus-vivendi
;;    '(default :background "#101010" :foreground "#d5d8d6")
;;    '(git-gutter-fr:added  :foreground ,green-fringe-bg)
;;    '(git-gutter-fr:deleted :foreground ,red-fringe-intense)
;;    '(git-gutter-fr:modified :foreground ,yellow-fringe-intense))

;; Dired Filter
(use-package! dired-filter)

;; In corfu, use M-Shift-SPC to do orderless completion
(map! :after corfu :map corfu-map "M-S-SPC" #'corfu-insert-separator)

;; Configure magit-delta
(use-package! magit-delta
  :defer t
  :hook (magit-mode . (lambda () (magit-delta-mode +1))))

;; Set up avy
(map! "M-g f" 'avy-goto-line)
(map! "M-g w" 'avy-goto-word-1)

;; For fixing the very poor experience on GCD
                                        ; (setq mac-command-modifier 'meta)
                                        ; (setq mac-option-modifier 'super)
                                        ; (setq  x-meta-keysym 'super
                                        ;       x-super-keysym 'meta)
                                        ; (map! "C-c C-e" 'end-of-buffer)
                                        ; (map! "C-c C-a" 'beginning-of-buffer)
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package! lsp-mode
  :defer t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "s-l"))
                        (lsp-enable-which-key-integration))))
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        ;lsp-client-packages nil
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

;; Enable more rememberable and easy keys for undo.
(global-unset-key (kbd "C-z"))
(map! "C-z" 'undo-fu-only-undo :desc "Undo the last change")
(map! "C-S-z" 'undo-fu-only-redo :desc "Redo the last change")

;; Use org-make-toc
(use-package! org-make-toc)

;; Add info path
(add-to-list 'Info-directory-list "/opt/homebrew/share/info/emacs")
