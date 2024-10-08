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
        (fg-region unspecified)))

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
(setq modus-themes-headings
      '((1 . (rainbow overline background 1.2))
        (2 . (rainbow background 1.15))
        (3 . (rainbow bold 1.1))
        (t . (semilight 1.0))))
;; Keep the border but make it the same color as the background of the
;; mode line (thus appearing borderless).  The difference with the
;; above is that this version is a bit thicker because the border are
;; still there.
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)))
;; Like the above, but the current tab has a colorful background and
;; the inactive tabs have a slightly more noticeable gray background.
(setq modus-themes-common-palette-overrides
      '((bg-tab-bar bg-main)
        (bg-tab-current bg-cyan-intense)
        (bg-tab-other bg-inactive)))
;; Make the fringe invisible
(setq modus-themes-common-palette-overrides
      '((fringe unspecified)))


(defun my-modus-themes-custom-faces (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     ;; Make foreground the same as background for a uniform bar on
     ;; Doom Emacs.
     ;;
     ;; Doom should not be implementing such hacks because themes
     ;; cannot support them:
     ;; <https://protesilaos.com/codelog/2022-08-04-doom-git-gutter-modus-themes/>.
     `(git-gutter-fr:added ((,c :foreground ,green-fringe-bg)))
     `(git-gutter-fr:deleted ((,c :foreground ,red-fringe-bg)))
     `(git-gutter-fr:modified ((,c :foreground ,yellow-fringe-bg))))))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

(provide 'vm-modus-custom)
;;; vm-modus-custom.el ends here
