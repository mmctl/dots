;; -*- lexical-binding: t; -*-
;; local-dirvish.el
;; Add, load, and compile extensions (seems due to bug (?))
(eval-when-compile
  (when-let* ((libdir (locate-library "dirvish"))
              (extdir (expand-file-name "extensions/" (file-name-parent-directory libdir)))
              ((file-directory-p extdir))
              (alfile (expand-file-name "dirvish-extensions-autoloads.el" extdir)))
    (add-to-list 'load-path extdir)
    (unless (file-exists-p alfile)
      (loaddefs-generate extdir alfile))
    (load alfile)))

(require 'dirvish)
(require 'dirvish-fd)
(require 'dirvish-side)
(require 'dirvish-quick-access)


;;;###autoload
(defun a-dirvish-fd-default-directory (pattern)
  "Simple wrapper around `dirvish-fd', with target directory fixed to
`default-directory'"
  (interactive (list (completing-read-multiple "Pattern: " nil)))
  (dirvish-fd default-directory pattern))

;;;###autoload
(defun a-dirvish-fd-full ()
  "Simple wrapper around `dirvish-fd', with `current-prefix-arg'
set to \='(16) (so it asks to provide both arguments)."
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively #'dirvish-fd)))

;;;###autoload
(defun a-dirvish-side-quit ()
  "Quits/kills `dirvish-side' session/window if it is visible (else does
nothing)."
  (interactive)
  (when-let* ((viswin (dirvish-side--session-visible-p)))
    (with-selected-window viswin
      (dirvish-quit))))


;;; Keymaps
(defvar-keymap a-dirvish-map
  :doc "Keymap for dirvish (global)"
  :prefix 'a-dirvish-map-prefix
  "d" #'dirvish-dwim
  "D" #'dirvish
  "j" #'dirvish-quick-access
  "s" #'dirvish-side
  "S" #'a-dirvish-side-quit
  "f" #'a-dirvish-fd-default-directory
  "F" #'a-dirvish-fd-full
  "C-f" #'dirvish-fd)


(provide 'local-dirvish)
;;; local-dirvish.el ends here
