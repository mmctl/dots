;; -*- lexical-binding: t -*-
;; local-pdf-tools.el

(require 'pdf-tools)


(defun an-around-advice-display-synctex (syncfun &rest args)
    "Around advice that (locally) adds an entry to `display-buffer-alist'
to reuse windows containing buffers with modes derived from
TeX-mode (for opening other such buffers).

Meant to be used with `synctex' functionality, so as to not pop up a new
window when syncing to a location in a project TeX file that is not yet
opened."
    (let* ((display-buffer-alist (cons '((derived-mode . TeX-mode)
                                         (display-buffer-reuse-window display-buffer-reuse-mode-window)
                                         (reusable-frames . visible))
                                       display-buffer-alist)))
      (apply syncfun args)))


(provide 'local-pdf-tools)

;;; local-pdf-tools.el ends here
