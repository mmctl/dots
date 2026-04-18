;; -*- lexical-binding: t -*-
;; local-embark.el

(require 'embark)

;;; Avy
(require 'avy)

;;;###autoload
(defun avy-action-an-embark-select (pt)
  "Executes `embark-select' at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-select))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-an-embark-act (pt)
  "Executes `embark-act' at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-an-embark-dwim (pt)
  "Executes `embark-dwim' at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)


;;; Ace-Window
(require 'ace-window)

;; Embark with Ace Window prefix
(defun an-embark-ace-window-action (fun)
  "Select and switch to window with `ace-window', always dispatching,
before calling FUN interactively."
  (with-demoted-errors "%s"
    (let* ((aw-dispatch-always t))
      (aw-switch-to-window (aw-select nil))
      (call-interactively fun))))

;;;###autoload
(defun an-embark-ace-window-find-file ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `find-file' interactively."
  (interactive)
  (an-embark-ace-window-action #'find-file))

;;;###autoload
(defun an-embark-ace-window-pop-to-buffer ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `pop-to-buffer-same-window' interactively."
  (interactive)
  (an-embark-ace-window-action #'pop-to-buffer-same-window))

;;;###autoload
(defun an-embark-ace-window-bookmark-jump ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `bookmark-jump' interactively."
  (interactive)
  (an-embark-ace-window-action #'bookmark-jump))

;;;###autoload
(defun an-embark-ace-window-find-library ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `find-library' interactively."
  (interactive)
  (an-embark-ace-window-action #'find-library))

;;;###autoload
(defun an-embark-ace-window-xref-find-definitions ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `xref-find-definitions' interactively."
  (interactive)
  (an-embark-ace-window-action #'xref-find-definitions))


;;; General
(defvar-keymap an-embark-completing-read-prompter-map
      :doc "Keymap for Embark's completing read prompter"
      "<backtab>" #'abort-recursive-edit)

(defmacro an-around-advice-with-minibuffer-keymap (keymap)
  "Expands to a lambda (taking a function and arguments) usable as around
advice that applies the provided function to its arguments inside a
minibuffer for which the local keymap is composed with KEYMAP."
  `(lambda (fun &rest args)
     (minibuffer-with-setup-hook
         (lambda ()
           (use-local-map
            (make-composed-keymap ,keymap (current-local-map))))
       (apply fun args))))

;;;###autoload
(defun an-embark-act-with-completing-read (&optional arg)
  "Calls `embark-act' with its completing read prompter
and minimal indicators."
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (embark-indicators '(embark-minimal-indicator)))
    (embark-act arg)))


(provide 'local-embark)

;;; local-embark.el ends here
