;; -*- lexical-binding: t; -*-
;; local-popper.el
(require 'popper)

(defun a-popper-group-by-directory-home-default ()
  "Returns an identifier to group popups, defaulting to the project root
(according to `project.el') if found, with `default-directory' as fallback. In
case `default-directory' is the home directory, return `nil' to assign to the
default group."
  (or (and (fboundp 'project-root)
           (when-let* ((project (project-current)))
             (project-root project)))
      (unless (file-equal-p (expand-file-name "~/")
                            (expand-file-name default-directory))
        default-directory)))

(defun a-popper-toggle-next (&optional arg)
  "Toggle next popup in group without burying current one through
 providing `popper-toggle', which see, a single prefix argument (by
default). With prefix argument ARG, calls `popper-toggle' with an
additional prefix argument."
  (interactive "p")
  (popper-toggle (* 4 arg)))

(defun a-popper-cycle-default-group ()
  "Cycle to next popup in default group by calling `popper-cycle',
which see, with `0' as argument."
  (interactive)
  (popper-cycle 0))


;;; Keymaps
(defvar-keymap a-popper-map
  :doc "Keymap for popper (global)"
  :prefix 'a-popper-map-prefix
  "k" #'popper-kill-latest-popup
  "l" #'popper-lower-to-popup
  "t" #'popper-toggle
  "T" #'popper-toggle-type
  "r" #'popper-raise-popup
  "^" #'popper-raise-popup
  "_" #'popper-lower-to-popup
  "<left>" #'popper-cycle-backwards
  "<right>" #'popper-cycle)

(defvar-keymap a-popper-cycle-repeat-map
  :doc "Keymap (repeatable) for popper cycling"
  :repeat t
  "<left>" #'popper-cycle-backwards
  "<right>" #'popper-cycle)

(provide 'local-popper)
;;; local-popper.el ends here
