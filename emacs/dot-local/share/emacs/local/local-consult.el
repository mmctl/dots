;; -*- lexical-binding: t; -*-
;; local-consult.el

(require 'consult)


(defun filter-return-advice-preview-buffer-no-obey-display-actions (ret)
  "Filter return advice for Consult's preview buffer function
(`consult--buffer-preview') to execute it in environment where
`switch-to-buffer' does not obey display actions and typically
uses window unless, e.g., dedicated."
  (lambda (action cand)
    (let* ((switch-to-buffer-obey-display-actions nil))
      (funcall ret action cand))))


(provide 'local-consult)
;;; local-consult.el ends here
