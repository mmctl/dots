;; -*- lexical-binding: t -*-
;; local-math-delimiters-pre.el


;; Swap to \( and \) instead of $ and $ (when using LaTeX instead of TeX)
;;;###autoload
(defun a-setup-latex-mode-math-delimiters ()
  (setq-local math-delimiters-inline '("\\(" . "\\)")))


(provide 'local-math-delimiters-pre)

;;; local-math-delimiters-pre.el ends here
