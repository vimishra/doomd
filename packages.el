;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

                                        ;
;;; Tabnine based AI completion backend for company
(package! company-tabnine)

;; I like tree-sitter
(package! tree-sitter)
(package! tree-sitter-langs)

;; Org and Org related packages to make org mode more effective.
;; Enable org-roam-ui, this needs the latest version of org-roam and so we need to unpin the version
;; that doom has chosen
(package! org-roam-ui)
;; Consult notes
(package! consult-notes
  :recipe (:host github :repo "mclear-tools/consult-notes"))
(package! consult-org-roam)
; Use org-modern package to make the Org mode look a little modern
(package! org-modern)
;; I need org-appear
(package! org-appear)
;; Enable visual-fill mode
;; Better agenda
(package! org-super-agenda)
(package! visual-fill-column)

;; I want smooth scrolling
(package! good-scroll)

;; all-the-icons
(package! all-the-icons)

;; Tomorrow Bright Color them - my favorite color theme
(package! color-theme-sanityinc-tomorrow)

;; Show a preview of line to move to
(package! goto-line-preview)

;; zzz-to-char
(package! zop-to-char)


(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el")))

;; Enable dirvish
(package! dirvish)
