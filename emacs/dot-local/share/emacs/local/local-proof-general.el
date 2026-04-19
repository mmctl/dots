;; -*- lexical-binding: t -*-
;; local-proof-general.el

;; (require 'proof-general)
(require 'proof)


;;;###autoload
(defun a-setup-proof-response-mode ()
  (toggle-truncate-lines -1)
  (toggle-word-wrap 1))

;;;###autoload
(defun a-setup-proof-goals-mode ()
  (toggle-truncate-lines -1)
  (toggle-word-wrap -1))

(defun an-advice-override-silence-bufhist-insert-buttons (&rest args)
  (setq-local bufhist-top-point (point-min)))


;; Keymaps
(defvar-keymap a-proof-mode-process-repeat-map
  :doc "Keymap (repeatable) for processing proof commands"
  :repeat (:hints ((proof-undo-last-successful-command . "p/u: Undo last succesful command")
                   (proof-assert-next-command-interactive . "n: Assert next command")
                   (proof-undo-and-delete-last-successful-command . "d: Undo and delete last successful command")))
  "p" #'proof-undo-last-successful-command
  "u" #'proof-undo-last-successful-command
  "n" #'proof-assert-next-command-interactive
  "d" #'proof-undo-and-delete-last-successful-command)

(defvar-keymap a-bufhist-repeat-map
  :doc "Keymap (repeatable) for browsing and managing buffer history"
  :repeat (:hints ((bufhist-prev . "p: Go to previous history element")
                   (bufhist-next . "n: Go to next history element")
                   (bufhist-first . "<: Go to first history element")
                   (bufhist-last . ">: Go to last history element")
                   (bufhist-delete . "d: Delete current history element")))
  "p" #'bufhist-prev
  "n" #'bufhist-next
  "<" #'bufhist-first
  ">" #'bufhist-last
  "d" #'bufhist-delete)

;;;###autoload
(defun a-setup-bufhist-map ()
  (keymap-set bufhist-mode-map "p" #'bufhist-prev)
  (keymap-set bufhist-mode-map "n" #'bufhist-next)
  (keymap-set bufhist-mode-map "<" #'bufhist-first)
  (keymap-set bufhist-mode-map ">" #'bufhist-last)
  (keymap-set bufhist-mode-map "c" #'bufhist-clear)
  (keymap-set bufhist-mode-map "d" #'bufhist-delete))

;;;###autoload
(defun a-setup-proof-mode-map ()
  (keymap-unset proof-mode-map "M-<up>")
  (keymap-unset proof-mode-map "M-<down>")
  (keymap-unset proof-mode-map "C-M-<up>")
  (keymap-unset proof-mode-map "C-M-<down>")
  (keymap-unset proof-mode-map "C-c v")
  (keymap-set proof-mode-map "C-S-u" #'proof-undo-last-successful-command)
  (keymap-set proof-mode-map "C-S-p" #'proof-undo-last-successful-command)
  (keymap-set proof-mode-map "C-S-n" #'proof-assert-next-command-interactive)
  (keymap-set proof-mode-map "C-c C-v" #'proof-goto-point)
  (keymap-set proof-mode-map "C-c C-d" #'proof-undo-and-delete-last-successful-command)
  (keymap-set proof-mode-map "C-c C-a" #'proof-goto-command-start)
  (keymap-set proof-mode-map "C-c C-e" #'proof-goto-command-end)
  (keymap-set proof-mode-map "C-c C-l" #'proof-goto-end-of-locked)
  (keymap-set proof-mode-map "C-c C-w" #'proof-layout-windows)
  (keymap-set proof-mode-map "C-c C-o" #'proof-display-some-buffers)
  (keymap-set proof-mode-map "C-c C-k" #'pg-response-clear-displays)
  (keymap-set proof-mode-map "C-c C-x" #'proof-minibuffer-cmd)
  (keymap-set proof-mode-map "C-c C-q" #'proof-shell-exit)
  (keymap-set proof-mode-map "M-P" #'pg-previous-matching-input-from-input)
  (keymap-set proof-mode-map "M-N" #'pg-next-matching-input-from-input)
  (keymap-set proof-mode-map "C-M-p" #'pg-previous-input)
  (keymap-set proof-mode-map "C-M-n" #'pg-next-input)
  (keymap-set proof-mode-map "C-M-S-p" #'pg-previous-matching-input)
  (keymap-set proof-mode-map "C-M-S-n" #'pg-next-matching-input)
  (keymap-set proof-mode-map "C-c M-v" #'pg-toggle-visibility))

;;;###autoload
(defun a-setup-proof-response-mode-map ()
  (keymap-set proof-response-mode-map "C-q" #'bury-buffer)
  (keymap-set proof-response-mode-map "C-c C-d" #'proof-undo-and-delete-last-successful-command)
  (keymap-set proof-response-mode-map "C-c C-e" #'proof-next-error)
  (keymap-set proof-response-mode-map "C-c C-w" #'proof-layout-windows)
  (keymap-set proof-response-mode-map "C-c C-o" #'proof-display-some-buffers)
  (keymap-set proof-response-mode-map "C-c C-k" #'pg-response-clear-displays)
  (keymap-set proof-response-mode-map "C-c C-x" #'proof-minibuffer-cmd)
  (keymap-set proof-response-mode-map "C-c C-q" #'proof-shell-exit))

;;;###autoload
(defun a-setup-proof-goals-mode-map ()
  (keymap-set proof-goals-mode-map "C-q" #'bury-buffer)
  (keymap-set proof-goals-mode-map "C-c C-d" #'proof-undo-and-delete-last-successful-command)
  (keymap-set proof-goals-mode-map "C-c C-e" #'proof-next-error)
  (keymap-set proof-goals-mode-map "C-c C-w" #'proof-layout-windows)
  (keymap-set proof-goals-mode-map "C-c C-o" #'proof-display-some-buffers)
  (keymap-set proof-goals-mode-map "C-c C-k" #'pg-response-clear-displays)
  (keymap-set proof-goals-mode-map "C-c C-x" #'proof-minibuffer-cmd)
  (keymap-set proof-goals-mode-map "C-c C-q" #'proof-shell-exit))


(provide 'local-proof-general)

;; local-proof-general.el ends here
