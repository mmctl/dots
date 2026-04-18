;; -*- lexical-binding: t -*-
;; local-org.el

(require 'org)
(require 'org-agenda)
(require 'org-modern)
(require 'org-modern-indent)


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

;;;###autoload
(defun a-setup-org-modern-indent-mode ()
    (org-indent-mode 1)
    (org-modern-indent-mode 1))


(provide 'local-org)

;;; local-org.el ends here
