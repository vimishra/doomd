;;; lisp/vm-modus-custom.el -*- lexical-binding: t; -*-
(setq modus-themes-mode-line '(accented))
;; Make the Org agenda use faint colors.
(setq modus-themes-common-palette-overrides
      '((date-common cyan-faint) ; for timestamps and more
        (date-deadline red-faint)
        (date-event fg-alt) ; default
        (date-holiday magenta) ; default (for M-x calendar)
        (date-now fg-main) ; default
        (date-scheduled yellow-faint)
        (date-weekday fg-alt)
        (date-weekend fg-dim)
        (fg-heading-1 blue-warmer)
        (bg-region bg-active)
        (fg-region unspecified)
        ))
(defface my-org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "My bold emphasis for Org.")
(defface my-org-emphasis-italic
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "My italic emphasis for Org.")
(defface my-org-emphasis-underline
  '((default :inherit underline)
    (((class color) (min-colors 88) (background light))
     :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0bc00"))
  "My underline emphasis for Org.")
(defface my-org-emphasis-strike-through
  '((((class color) (min-colors 88) (background light))
     :strike-through "#972500" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :strike-through "#ef8b50" :foreground "#a8a8a8"))
  "My strike-through emphasis for Org.")
(setq org-emphasis-alist
      '(("*" my-org-emphasis-bold)
        ("/" my-org-emphasis-italic)
        ("_" my-org-emphasis-underline)
        ("=" org-verbatim verbatim)
        ("~" org-code verbatim)
        ("+" my-org-emphasis-strike-through)))
(provide 'vm-modus-custom)
