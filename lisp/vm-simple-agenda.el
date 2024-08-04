;;; agenda.el -*- lexical-binding: t; -*-
;;; Stuff in this file is stolen shamelessly from many of the places from the web. It works,
;;; don't break it
;(require 'cl-lib)
;; This is for setting the agenda to look much better than how it does.
;; It removes the ridiculously long file name that org-roam uses by default and uses the title.
;; In addition, it truncates it to 30 chars. Which ought to be more than enough to get an idea of the title
;; as well as look pretty.
;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
 (setq org-agenda-prefix-format
      '((agenda . " %i %-12(vm-agenda-category 20)%?-12t% s")
        (todo . " %i %-12(vm-agenda-category 20) %-22(org-entry-get nil \"SCHEDULED\") %-22(org-entry-get nil \"DEADLINE\") % s ")
        (tags . " %i %-12(vm-agenda-category 20) % s")
        (search . " %i %-12(vm-agenda-category 20) % s")))

(defun vm-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun vm-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vm-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vm-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(provide 'vm-simple-agenda)
