;; -*- lexical-binding: t -*-
;; local-go-ts-mode.el

(require 'go-ts-mode)


(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))


(provide 'local-go-ts-mode)

;;; local-go-ts-mode.el ends here
