;; -*- lexical-binding: t -*-
;; local-keycast.el

(require 'keycast)


;; Custom global minor mode for compatibility with `doom-modeline'
(define-minor-mode keycast-mode
	"Show current command and its key binding in the mode line, for use with
`doom-modeline'."
	:global t
	(if keycast-mode
		  (add-hook 'pre-command-hook 'keycast--update nil t)
    (remove-hook 'pre-command-hook 'keycast--update t)))


(provide 'local-keycast)

;; local-keycast.el ends here
