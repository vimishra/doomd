;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Vikas Mishra"
      user-mail-address "vikas.mishra@hey.com"
      doom-scratch-initial-major-mode 'lisp-interaction-mode
      org-ellipsis " ▼ "
      )


(cond ((eq system-type 'gnu/linux)
       ;; Linux - Especially Manjaro KDE
       ;; On Manjaro for some reason I need to setup a stupidly large font
       ;; Only then it is readable.
       (setq doom-font (font-spec :family "Cascadia Code " :size 30))
       (setq doom-variable-pitch-font (font-spec :family "Calibri" :size 30))
       )
      ((eq system-type 'darwin)
       ;; MacOS specific stuff
       (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 17))
       (setq doom-variable-pitch-font (font-spec :family "Helvetica" :size 24))
       (setq line-spacing 3)
       ))

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


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(custom-theme-set-faces! 'doom-one
  '(default :background "#101114"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/OrgNotes/")

;; Auto enable auto-fill mode in org mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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



(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0)))))

;; Use C-x C-m as M-x command
(map! "C-x C-m" #'execute-extended-command)

;; Use debugpy for python debugging
(after! dap-mode
  (setq dap-python-debugger 'debugpy))


(after! lsp-mode
  (lsp-treemacs-sync-mode 1)

  ;; Unmap some of doom's keysmaps. I use S-l for LSP
  (map! "s-l" nil)
  ;; Configure LSP Mode the way I like
  ;;
  (setq lsp-ui-sideline-enable t
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

  (setq lsp-keymap-prefix "s-l")
  (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  ;; Don't show hidden files in treemacs.
  (setq treemacs-show-hidden-files nil))


;; Enable tabnine for company
(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  )



;; Enable windmove for quickly moving across windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

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

;; Treemacs width
(setq treemacs--width-is-locked nil)

(use-package tree-sitter
  :after python-mode
  :defer t
  :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  )

;; A better search interface
;; (use-package! ctrlf
;;   :hook
;;   (after-init . ctrlf-mode))

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
  (setq org-image-actual-width 800)
  ;; I want to open org link in other windows - not the same window.
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)

  ;; Set my sequence of todo things
  (setq org-todo-keywords
        '((sequence
           "NEXT(N)"  ; Task that's ready to be next. No dependencies
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
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
           "NO(n)"))
        ))


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
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: daily")))))

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

;; Location for my custom emacs files.
(use-package vm-agenda
  :load-path "lisp")


;; Deft for org mode
(setq deft-directory org-directory)
(setq deft-recursive t
      deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
      deft-use-filter-string-for-filename t)

;; Consult notes for org-roam

(use-package! consult-notes
  :commands (
  consult-notes-search-all
  consult-notes-org-roam-find-node
  consult-notes-org-roam-find-node-relation)
:config
;; set org-roam integration
(consult-notes-org-roam-mode))


;; My Helper functions
;; Open an Eshell in the current directory.
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun split-and-follow-horizontally ()
  "Function splits the current window horizontally and jumps to
new window.

Normally Emacs split (horizontal or vertical) splits the
window but doesn't move to it. This doesn't make any sense. By
default, I am splitting because, I want to do something in the
new window.

These two functions split-and-follow-horizontally and
split-and-follow-vertically fix this issue. They also bind the
keys appropriately."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "Function splits the current window horizontally and jumps to
new window.

Normally Emacs split (horizontal or vertical) splits the window
but doesn't move to it. This doesn't make any sense. By
default, I am splitting because, I want to do something in the
new window.

These two functions split-and-follow-horizontally and
split-and-follow-vertically fix this issue. They also bind the
keys appropriately."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; My function to enable swapping from vertical split to horizontal split
(defun toggle-window-split ()
  "Toggle the vertical and horizontal split.

If the windows are split vertically, they will be replaced with a
horizontal split and if they are split horizontally, will be
replaced with a vertical split.

Works with exactly 2 windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Functions to insert date and time in a custom format

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-time-format-journal "%H:%M"
  "Format of date to insert with `insert-current-time' func. This
is specifically for adding the time stamp in the interstitial
journal. Note the weekly scope of the command's precision.")


(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
                                        ;       (insert (let () (comment-start)))
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert " - ")
  )

(defun insert-current-time-for-journal ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format-journal (current-time)))
  (insert " - ")
  )

(map! "C-c n C-d" 'insert-current-date-time)
(map! "C-c n C-t" 'insert-current-time-for-journal)

;; Use w3m to browse basic web.
(use-package! w3m
  :commands (w3m)
  :config
  (setq w3m-use-tab-line nil)
  )

;; Winner mode
(winner-mode +1)
