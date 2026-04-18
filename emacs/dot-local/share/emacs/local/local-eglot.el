;; -*- lexical-binding: t -*-
;; local-eglot.el

(require 'eglot)


;;; Keybindings
(defvar-keymap an-eglot-prog-map
    :doc "Keymap for eglot programming bindings"
    :prefix 'an-eglot-prog-map-prefix
    "C-`" #'flymake-goto-next-error
    "M-`" #'flymake-show-buffer-diagnostics
    "C-a a" #'eglot-code-actions
    "C-a x" #'eglot-code-action-extract
    "C-a i" #'eglot-code-action-inline
    "C-a o" #'eglot-code-action-organize-imports
    "C-a r" #'eglot-code-action-rewrite
    "C-a f" #'eglot-code-action-quickfix
    "C-f ." #'eldoc-print-current-symbol-info
    "C-b" #'eldoc-doc-buffer
    "C-f i" #'eglot-find-implementation
    "C-f t" #'eglot-find-typeDefinition
    "C-f c" #'eglot-find-declaration
    "C-f d" #'xref-find-definitions
    "C-f D" #'xref-find-definitions-other-window
    "C-f C-d" #'xref-find-definitions-other-frame
    "C-f r" #'xref-find-references
    "C-r" #'eglot-rename)

(defun a-setup-eglot-prog-map-local ()
  "Binds `an-eglot-prog-map' to `C-c' in the local keymap if
current buffer is managed by eglot, or unsets the local `C-c` binding otherwise.

Meant as (buffer-local) hook for `eglot-managed-mode-hook' in
programming major-mode with sparse keymaps (so the Eglot functionality
makes up for the lack of direct major-mode functionality)."
  (if (eglot-managed-p)
      (keymap-local-set "C-c" 'an-eglot-prog-map-prefix)
    (keymap-local-unset "C-c")))

(defun a-setup-eglot-prog-map-local-hook ()
  "Adds `a-setup-eglot-prog-map-local' (buffer-local)
hook to `eglot-managed-mode-hook' (after `eglot' is loaded).

Meant as hook in programming major-modes with sparse keymaps (so the
Eglot functionality makes up for the lack of direct major-mode
functionality)"
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'a-setup-eglot-prog-map-local nil t)))


(provide 'local-eglot)

;;; local-eglot.el ends here
