;; -*- lexical-binding: t -*-
;; loc-modes.el (functions/hooks)

;; Text
(defun loc-setup-text-mode ()
  "Setup modes mainly dealing with text."
  (when (display-graphic-p)
    (cond
     ((member "Open Sans" (font-family-list))
      (face-remap-add-relative 'default '(:family "Open Sans" :height 140 :weight semi-bold)))
     (t
      (face-remap-add-relative 'default '(:family "Sans Serif" :height 140)))))
  (setq-local display-line-numbers-type t)
  (display-line-numbers-mode 1)
  (visual-line-mode 1))

;; Code/Prog
(defun loc-setup-code-mode ()
  "Setup modes mainly dealing with code."
  (when (display-graphic-p)
    (cond
     ((member "MonaspiceNe Nerd Font" (font-family-list))
      (face-remap-add-relative 'default '(:family "MonaspiceNe Nerd Font" :height 110 :weight medium)))
     ((member "Monaspace Neon" (font-family-list))
      (face-remap-add-relative 'default '(:family "Monaspace Neon" :height 110 :weight medium)))
     (t
      (face-remap-add-relative 'default '(:family "Monospace" :height 110))))
    (cond
     ((member "MonaspiceRn Nerd Font" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "MonaspiceRn Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Radon" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "Monaspace Radon" :height 110 :weight medium)))
     (t
      (face-remap-add-relative 'default '(:family "Monospace" :height 110)))))
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers-type 'relative)
  (setq-local display-line-numbers-current-absolute nil)
  (display-line-numbers-mode 1)
  (hl-line-mode 1))

;; Mix of text and code (e.g., latex or configuration files)
(defun loc-setup-mix-mode ()
  "Setup modes dealing with a (somewhat even) mix of text and code."
  (when (display-graphic-p)
    (cond
     ((member "MonaspiceAr Nerd Font" (font-family-list))
      (face-remap-add-relative 'default '(:family "MonaspiceAr Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Argon" (font-family-list))
      (face-remap-add-relative 'default '(:family "Monaspace Argon" :height 110 :weight medium)))
     (t
      (face-remap-add-relative 'default '(:family "Monospace" :height 110))))
    (cond
     ((member "MonaspiceRn Nerd Font" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "MonaspiceRn Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Radon" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "Monaspace Radon" :height 110 :weight medium)))
     (t
      (face-remap-add-relative 'default '(:family "Monospace" :height 110)))))
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers-type 'relative)
  (setq-local display-line-numbers-current-absolute nil)
  (visual-line-mode 0)
  (display-line-numbers-mode 1)
  (hl-line-mode 1))

;; Mix of text and code in small buffers (e.g., minibuffer or pop-up)
(defun loc-setup-mini-mix-mode ()
  "Setup modes dealing with a (somewhat even) mix of text and code in more
transient buffers."
  (when (display-graphic-p)
    (cond
     ((member "MonaspiceAr Nerd Font" (font-family-list))
      (face-remap-add-relative 'default '(:family "MonaspiceAr Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Argon" (font-family-list))
      (face-remap-add-relative 'default '(:family "Monaspace Argon" :height 110 :weight medium)))
     (t
      (face-remap-add-relative 'default '(:family "Monospace" :height 110))))
    (cond
     ((member "MonaspiceRn Nerd Font" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "MonaspiceRn Nerd Font" :height 110 :weight medium)))
     ((member "Monaspice Radon" (font-family-list))
      (face-remap-add-relative 'font-lock-comment-face '(:family "Monaspace Radon" :height 110 :weight medium)))
     (t
      (face-remap-add-relative 'default '(:family "Monospace" :height 110))))))


(provide 'loc-modes)

;;; loc-modes.el ends here
