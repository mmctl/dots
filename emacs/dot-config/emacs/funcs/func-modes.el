;; func-modes.el (functions/hooks)

;; Text
(defun setup-a-text-mode ()
  "Setup modes mainly dealing with code."
  (when (display-graphic-p)
    (cond
     ((member "Open Sans" (font-family-list))
      (face-remap-add-relative 'default '(:family "Open Sans" :height 140 :weight semi-bold)))))
  (setq-local display-line-numbers-type t)
  (display-line-numbers-mode 1)
  (visual-line-mode 1))

;; Code/Prog
(defun setup-a-code-mode ()
  "Setup modes mainly dealing with code."
  (when (display-graphic-p)
    (cond
     ((member "MonaspiceNe Nerd Font" (font-family-list))
      (face-remap-add-relative 'default '(:family "MonaspiceNe Nerd Font" :height 110 :weight medium)))
     ((member "Monaspace Neon" (font-family-list))
      (face-remap-add-relative 'default '(:family "Monaspace Neon" :height 110 :weight medium))))
    (cond
     ((member "MonaspiceRn Nerd Font" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "MonaspiceRn Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Radon" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "Monaspace Radon" :height 110 :weight medium)))))
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers-type 'relative)
  (setq-local display-line-numbers-current-absolute nil)
  (display-line-numbers-mode 1)
  (hl-line-mode 1))

;; Mix of text and code (e.g., latex or configuration files)
(defun setup-a-mix-mode ()
  "Setup modes dealing with a (somewhat even) mix of text and code."
  (when (display-graphic-p)
    (cond
     ((member "MonaspiceAr Nerd Font" (font-family-list))
      (face-remap-add-relative 'default '(:family "MonaspiceAr Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Argon" (font-family-list))
      (face-remap-add-relative 'default '(:family "Monaspace Argon" :height 110 :weight medium))))
    (cond
     ((member "MonaspiceRn Nerd Font" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "MonaspiceRn Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Radon" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "Monaspace Radon" :height 110 :weight medium)))))
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers-type 'relative)
  (setq-local display-line-numbers-current-absolute nil)
  (visual-line-mode 0)
  (display-line-numbers-mode 1)
  (hl-line-mode 1))

;; Mix of text and code in small buffers (e.g., minibuffer or pop-up)
(defun setup-a-mini-mix-mode ()
  "Setup modes dealing with a (somewhat even) mix of text and code."
  (when (display-graphic-p)
    (cond
     ((member "MonaspiceAr Nerd Font" (font-family-list))
      (face-remap-add-relative 'default '(:family "MonaspiceAr Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Argon" (font-family-list))
      (face-remap-add-relative 'default '(:family "Monaspace Argon" :height 110 :weight medium))))
    (cond
     ((member "MonaspiceRn Nerd Font" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "MonaspiceRn Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Radon" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "Monaspace Radon" :height 110 :weight medium))))))

(provide 'func-modes)
;;; func-modes.el ends here
