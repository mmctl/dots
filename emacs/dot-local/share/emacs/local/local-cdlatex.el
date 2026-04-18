;; -*- lexical-binding: t -*-
;; local-cdlatex.el

(require 'cdlatex)


;; Ensure Corfu is not in automatic mode, as to not interfere with templates
;;;###autoload
(defun a-setup-cdlatex-corfu-mode ()
  (with-eval-after-load 'corfu
    (setq-local corfu-auto nil)))

;; Swap to \( and \) instead of $ and $ (when using LaTeX instead of TeX)
;;;###autoload
(defun a-setup-latex-mode-not-use-dollar ()
  (setq-local cdlatex-use-dollar-to-ensure-math nil))


(provide 'local-cdlatex)

;;; local-cdlatex.el ends here
