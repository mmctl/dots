;; -*- lexical-binding: t -*-
;; local-setup.el (frames/modes)


;;; Utilities
;; Re-enable custom themes
(defsubst local-reenable-custom-themes ()
  "Re-enable currently enabled custom themes"
  (let ((revcenth (reverse custom-enabled-themes)))
    (dolist (cth revcenth) (enable-theme cth))))


;;; Frames
;; TTY
(defun local-setup-tty-frame (frame)
  "Setup TTY frame.")

;; GUI
(defun local-setup-gui-frame (frame)
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
(defun local-setup-frame (&optional frame)
  "Set up (TTY or GUI) FRAME, defaulting to the selected frame."
  (let ((frame (or frame (selected-frame))))
    (when (frame-live-p frame)
      (if (display-graphic-p frame)
          (local-setup-gui-frame frame)
        (local-setup-tty-frame frame))
      (setopt display-line-numbers-width 3))))

;; Nonserver/Client
(defun local-setup-nonserver-frame (frame)
  "Set up FRAME unless it belongs to an Emacs server request."
  (unless (frame-parameter frame 'server-dummy-buffer)
    (local-setup-frame frame)))

(defun local-setup-client-frame ()
  "Set up the selected real Emacs server client frame."
  (let ((frame (selected-frame)))
    (when (frame-parameter frame 'client)
      (local-setup-frame frame)
      (local-reenable-custom-themes))))


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
