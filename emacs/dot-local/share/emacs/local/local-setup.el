;; -*- lexical-binding: t -*-
;; local-setup.el (frames/modes)


;;; Utilities
;; Re-enable custom themes
(defsubst local-reenable-custom-themes ()
  "Re-enable currently enabled custom themes"
  (let ((revcenth (reverse custom-enabled-themes)))
    (dolist (cth revcenth) (enable-theme cth))))


;;; Frames
(defun local-setup-frame-defaults-from-given (frame)
  "Set up default frame settings for current (FRAME) and future frames,
based on current (non-dummy) graphical frame FRAME."
  ;; Faces
  (when (and (display-graphic-p frame)
             (not (frame-parameter frame 'server-dummy-buffer)))
    (let ((ffl (font-family-list frame)))
      ;; Default/Fixed-pitch
      (cond
       ((member "Iosevka" ffl)
        (set-face-attribute 'default frame :family "Iosevka" :height 140 :weight 'medium)
        (set-face-attribute 'default t :family "Iosevka" :height 140 :weight 'medium)
        (set-face-attribute 'fixed-pitch frame :family "Iosevka" :height 1.0)
        (set-face-attribute 'fixed-pitch t :family "Iosevka" :height 1.0))
       ((member "Iosevka Nerd Font" ffl)
        (set-face-attribute 'default frame :family "Iosevka Nerd Font" :height 140 :weight 'medium)
        (set-face-attribute 'default t :family "Iosevka Nerd Font" :height 140 :weight 'medium)
        (set-face-attribute 'fixed-pitch frame :family "Iosevka Nerd Font" :height 1.0)
        (set-face-attribute 'fixed-pitch t :family "Iosevka Nerd Font" :height 1.0))
       ((member "Monaspace Neon" ffl)
        (set-face-attribute 'default frame :family "Monaspace Neon" :height 120 :weight 'medium)
        (set-face-attribute 'default t :family "Monaspace Neon" :height 120 :weight 'medium)
        (set-face-attribute 'fixed-pitch frame :family "Monaspace Neon" :height 1.0)
        (set-face-attribute 'fixed-pitch t :family "Monaspace Neon" :height 1.0)
        (when (member "Monaspace Radon" ffl)
          (set-face-attribute 'font-lock-comment-face frame :family "Monaspace Radon")
          (set-face-attribute 'font-lock-comment-face t :family "Monaspace Radon")))
       ((member "MonaspiceNe Nerd Font" ffl)
        (set-face-attribute 'default frame :family "MonaspiceNe Nerd Font" :height 120 :weight 'medium)
        (set-face-attribute 'default t :family "MonaspiceNe Nerd Font" :height 120 :weight 'medium)
        (set-face-attribute 'fixed-pitch frame :family "MonaspiceNe Nerd Font" :height 1.0)
        (set-face-attribute 'fixed-pitch t :family "MonaspiceNe Nerd Font" :height 1.0)
        (when (member "MonaspiceRn Nerd Font" ffl)
          (set-face-attribute 'font-lock-comment-face frame :family "MonaspiceRn Nerd Font")
          (set-face-attribute 'font-lock-comment-face t :family "MonaspiceRn Nerd Font")))
       (t
        (set-face-attribute 'default frame :family "Monospace" :height 120)
        (set-face-attribute 'default t :family "Monospace" :height 120)
        (set-face-attribute 'fixed-pitch frame :family "Monospace" :height 1.0)
        (set-face-attribute 'fixed-pitch t :family "Monospace" :height 1.0)))
      ;; Variable-pitch
      (cond
       ((and (equal (face-attribute 'fixed-pitch :family frame) "Iosevka")
             (member "Iosevka Aile" ffl))
        (set-face-attribute 'variable-pitch frame :family "Iosevka Aile" :height 1.0 :weight 'medium)
        (set-face-attribute 'variable-pitch t :family "Iosevka Aile" :height 1.0 :weight 'medium))
       ((and (equal (face-attribute 'fixed-pitch :family frame) "Iosevka Nerd Font")
             (member "Iosevka Nerd Font Propo" ffl))
        (set-face-attribute 'variable-pitch frame :family "Iosevka Nerd Font Propo" :height 1.0 :weight 'medium)
        (set-face-attribute 'variable-pitch t :family "Iosevka Nerd Font Propo" :height 1.0 :weight 'medium))
       ((member "Open Sans" ffl)
        (set-face-attribute 'variable-pitch frame :family "Open Sans" :height 1.0)
        (set-face-attribute 'variable-pitch t :family "Open Sans" :height 1.0))
       (t
        (set-face-attribute 'variable-pitch frame :family "Sans Serif" :height 1.0)
        (set-face-attribute 'variable-pitch t :family "Sans Serif" :height 1.0))))
    (remove-hook 'after-make-frame-functions #'local-setup-frame-defaults-from-given))
  (setopt display-line-numbers-width-start 3))

(defun local-setup-frame-defaults-from-selected ()
  (local-setup-frame-defaults-from-given (selected-frame)))

;; TTY
(defun local-setup-tty-frame (frame)
  "Setup TTY frame.")

;; GUI
(defun local-setup-gui-frame (&optional frame)
  "Setup GUI frame."
  ;; Faces
  ;; Default/Fixed-pitch
  (when-let* ((ffl (font-family-list frame)))
    (cond
     ((member "Iosevka" ffl)
      (set-face-attribute 'default frame :family "Iosevka" :height 140 :weight 'medium)
      (set-face-attribute 'fixed-pitch frame :family "Iosevka" :height 1.0))
     ((member "Iosevka Nerd Font" ffl)
      (set-face-attribute 'default frame :family "Iosevka Nerd Font" :height 140 :weight 'medium)
      (set-face-attribute 'fixed-pitch frame :family "Iosevka Nerd Font" :height 1.0))
     ((member "Monaspace Neon" ffl)
      (set-face-attribute 'default frame :family "Monaspace Neon" :height 120 :weight 'medium)
      (set-face-attribute 'fixed-pitch frame :family "Monaspace Neon" :height 1.0)
      (when (member "Monaspace Radon" ffl)
        (set-face-attribute 'font-lock-comment-face frame :family "Monaspace Radon")))
     ((member "MonaspiceNe Nerd Font" ffl)
      (set-face-attribute 'default frame :family "MonaspiceNe Nerd Font" :height 120 :weight 'medium)
      (set-face-attribute 'fixed-pitch frame :family "MonaspiceNe Nerd Font" :height 1.0)
      (when (member "MonaspiceRn Nerd Font" ffl)
        (set-face-attribute 'font-lock-comment-face frame :family "MonaspiceRn Nerd Font")))
     (t
      (set-face-attribute 'default frame :family "Monospace" :height 120)
      (set-face-attribute 'fixed-pitch frame :family "Monospace" :height 1.0)))
    ;; Variable-pitch
    (cond
     ((and (equal (face-attribute 'fixed-pitch :family frame) "Iosevka")
           (member "Iosevka Aile" ffl))
      (set-face-attribute 'variable-pitch frame :family "Iosevka Aile" :height 1.0 :weight 'medium))
     ((and (equal (face-attribute 'fixed-pitch :family frame) "Iosevka Nerd Font")
           (member "Iosevka Nerd Font Propo" ffl))
      (set-face-attribute 'variable-pitch frame :family "Iosevka Nerd Font Propo" :height 1.0 :weight 'medium))
     ((member "Open Sans" ffl)
      (set-face-attribute 'variable-pitch frame :family "Open Sans" :height 1.0))
     (t
      (set-face-attribute 'variable-pitch frame :family "Sans Serif" :height 1.0)))))


;; General
(defun local-setup-frame (frame)
  "Set up (TTY or GUI) FRAME once."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (if (display-graphic-p frame)
          (local-setup-gui-frame frame)
        (local-setup-tty-frame frame))
      (setopt display-line-numbers-width 3))))

(defun local-setup-selected-frame ()
  "Set up selected frame once."
  (let ((frame (selected-frame)))
    (local-setup-frame frame)))


;;; Modes
;; Text
(defun local-setup-text-mode ()
  "Setup modes mainly dealing with text."
  (setq-local display-line-numbers-type t)
  (display-line-numbers-mode 1)
  (hl-line-mode 0)
  (visual-line-mode 1))

;; Code/Prog
(defun local-setup-code-mode ()
  "Setup modes mainly dealing with code."
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers-type 'relative)
  (setq-local display-line-numbers-current-absolute nil)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (visual-line-mode 0))

;; Special
(defun local-setup-special-mode ()
  "Setup modes mainly dealing with special text/images."
  (display-line-numbers-mode 0)
  (hl-line-mode 0)
  (visual-line-mode 1))


(provide 'local-setup)

;;; local-setup.el ends here
