;;; lisp/org-roam-filter-entries.el -*- lexical-binding: t; -*-

 ;; org-roam-filter-entries.el

(require 'org-roam)

;;; 0. Variables
;;  --------------------------------------------------
(defcustom org-roam-filter-by-entries '("journal" "notes")
  "Entries (tags or directories) to be excluded for Org-roam node filtering."
  :type '(repeat string)
  :group 'org-roam)
;;  ---

;;; 1. Helper Functions
;;  --------------------------------------------------

;; Helper function to filter org-roam-node by entries (tags or directories).
(defun custom/org-roam-node--filter-by-entries (node &optional included-entries excluded-entries)
  "Filter org-roam-node by entries (tags or directories)."
  (let* ((entries (append (org-roam-node-tags node)
                          (butlast (f-split (f-relative (org-roam-node-file node) org-roam-directory)))))
         (included (and included-entries (not (seq-some (lambda (entry) (member entry entries)) included-entries))))
         (excluded (and excluded-entries (seq-some (lambda (entry) (member entry entries)) excluded-entries))))
    (if (or included excluded)
        nil t)))

;; Helper function to filter org-roam-node to show only nodes with entries in `org-roam-filter-by-entries`.
(defun custom/org-roam-node--filter-excluded (node)
  "Filter org-roam-node to show only nodes with entries in `org-roam-filter-by-entries`."
  (custom/org-roam-node--filter-by-entries node nil org-roam-filter-by-entries))
;;  ---

;;; 2. Find Functions
;;  --------------------------------------------------

;; Modded org-roam-node-find which filters nodes using entries (tags or directories).
(defun custom/org-roam-node-find ()
  "Show Org-roam nodes, filtering by entries (tags or directories)."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node) (custom/org-roam-node--filter-by-entries node nil org-roam-filter-by-entries))))

;; Show only Org-roam nodes that match entries in `org-roam-filter-by-entries`.
(defun custom/org-roam-node-find-only ()
  "Show only Org-roam nodes that match entries in `org-roam-filter-by-entries`."
  (interactive)
  (org-roam-node-find nil nil (lambda (node) (not (custom/org-roam-node--filter-excluded node)))))

(defun custom/org-roam-node-find-entry (&optional new-entries)
  "Show only Org-roam nodes matching the specified entries interactively.
If NEW-ENTRIES are provided, use them as the entries to match by (seperate entries by ,).
Otherwise, prompt the user to select from existing entries."
  (interactive)
  (if new-entries
      (let ((org-roam-filter-by-entries new-entries))
        (custom/org-roam-node-find-only))
    (let ((selected-entries (completing-read-multiple "Select entries to filter by (seperate by ,): " org-roam-filter-by-entries)))
      (let ((org-roam-filter-by-entries selected-entries))
        (custom/org-roam-node-find-only)))))

;; Perform find actions on Org-roam nodes based on the prefix argument.
(defun custom/org-roam-node-find-actions (arg)
  "Perform find actions on Org-roam nodes.

With no prefix argument, this function invokes `org-roam-node-find`,
to find nodes from the unfiltered list

With a prefix argument of C-u 1, it calls `custom/org-roam-node-find`,
to find Org-roam nodes from a filtered list excluding entries specified in `org-roam-filter-by-entries`

With a prefix argument of C-u 2, it calls `custom/org-roam-node-find-only`,
to find only nodes that match entries specified in `org-roam-filter-by-entries`.

With a prefix argument of C-u 3, it calls `custom/org-roam-node-find-entry`
allowing the user to interactively choose entries or specify on-the-fly new ones (seperated by ,) and to find nodes matching the entries.
"
  (interactive "P")
  (let ((numeric-arg (prefix-numeric-value arg)))
    (cond
     ((null arg) (org-roam-node-find))
     ((= numeric-arg 1) (custom/org-roam-node-find))
     ((= numeric-arg 2) (custom/org-roam-node-find-only))
     ((= numeric-arg 3) (custom/org-roam-node-find-entry))
     (t (custom/org-roam-node-find)))))
;;  ---

;;; 3. Input Functions
;;  --------------------------------------------------

;; Modded org-roam-node-insert which filters nodes using entries (tags or directories).
(defun custom/org-roam-node-insert ()
  "Insert Org-roam node, filtering by entries (tags or directories)."
  (interactive)
  (org-roam-node-insert
   (lambda (node) (custom/org-roam-node--filter-by-entries node nil org-roam-filter-by-entries))))

;; Show only Org-roam nodes that have entries in `org-roam-filter-by-entries`.
(defun custom/org-roam-node-insert-only ()
  "Insert Org-roam node, showing only nodes that match entries in `org-roam-filter-by-entries`."
  (interactive)
  (org-roam-node-insert (lambda (node) (not (custom/org-roam-node--filter-excluded node)))))

(defun custom/org-roam-node-insert-entry (&optional new-entries)
  "Insert Org-roam node, showing only nodes matching the specified entries interactively.
If NEW-ENTRIES are provided, use them as the entries to match by (seperate entries by ,).
Otherwise, prompt the user to select from existing entries."
  (interactive)
  (if new-entries
      (let ((org-roam-filter-by-entries new-entries))
        (custom/org-roam-node-insert-only))
    (let ((selected-entries (completing-read-multiple "Select entries to filter by (seperate by ,): " org-roam-filter-by-entries)))
      (let ((org-roam-filter-by-entries selected-entries))
        (custom/org-roam-node-insert-only)))))

;; Perform actions on Org-roam nodes during insertion based on the prefix argument.
(defun custom/org-roam-node-insert-actions (arg)
  "Perform insert actions on Org-roam nodes.

With no prefix argument, this function invokes `org-roam-node-insert`,
to insert nodes from the unfiltered list

With a prefix argument of C-u 1, it calls `custom/org-roam-node-insert`,
to insert Org-roam nodes from a filtered list excluding entries specified in `org-roam-filter-by-entries`

With a prefix argument of C-u 2, it calls `custom/org-roam-node-insert-only`,
to insert only nodes that match entries specified in `org-roam-filter-by-entries`.

With a prefix argument of C-u 3, it calls `custom/org-roam-node-insert-entry`
allowing the user to interactively choose entries or specify on-the-fly new ones (seperated by ,) and to insert from nodes matching the entries.
"
  (interactive "P")
  (let ((numeric-arg (prefix-numeric-value arg)))
    (cond
     ((null arg) (org-roam-node-insert))
     ((= numeric-arg 1) (custom/org-roam-node-insert))
     ((= numeric-arg 2) (custom/org-roam-node-insert-only))
     ((= numeric-arg 3) (custom/org-roam-node-insert-entry))
     (t (custom/org-roam-node-insert)))))
;;  ---

;; Key bindings
(global-set-key (kbd "C-c f f") #'custom/org-roam-node-find-actions)
(global-set-key (kbd "C-c f i") #'custom/org-roam-node-insert-actions)

(provide 'org-roam-filter-entries)
;;;
