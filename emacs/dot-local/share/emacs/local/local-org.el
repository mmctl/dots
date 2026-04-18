;; -*- lexical-binding: t -*-
;; local-org.el

(require 'org)
(require 'org-agenda)

;;;###autoload
(defun an-org-todo-manipulate-time (&optional arg)
  "As `org-todo'/`org-agenda-todo', but with the
date/time set to that entered by the user through `org-read-date'."
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (datetime (org-read-date t t nil "Manipulate --"))
             ((symbol-function 'current-time) #'(lambda () datetime))
             ((symbol-function 'org-current-effective-time) #'(lambda () datetime))
             ((symbol-function 'org-today) #'(lambda () (time-to-days datetime))))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-todo arg)
      (org-todo arg))))



(provide 'local-org)

;;; local-org.el ends here
