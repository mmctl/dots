;; -*- lexical-binding: t; -*-
;; local-ace-window.el
(require 'ace-window)

(defun an-ace-window-prefix ()
    "Sets `ace-window' as the function to choose window for displaying the
buffer of the next command.

The next buffer is the buffer displayed by the next command invoked
immediately after this command, ignoring reading from the minibuffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let ((window (aw-select (propertize " ACE" 'face 'mode-line-highlight)))
             (type 'reuse))
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

(provide 'local-ace-window)
;;; local-ace-window.el
