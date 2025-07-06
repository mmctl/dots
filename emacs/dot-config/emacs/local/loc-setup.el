;; -*- lexical-binding: t -*-
;; loc-setup.el (frames/modes)

;;; Utilities
;; Re-enable custom themes
(defsubst reenable-custom-themes ()
  "Re-enable currently enabled custom themes"
  (let ((revcenth (reverse custom-enabled-themes)))
    (dolist (cth revcenth) (enable-theme cth))))

;;; Frames
;; TTY
(defun loc-setup-tty-frame (frame)
  "Setup TTY frame."
  (setopt display-line-numbers-width 3))

;; GUI
(defun loc-setup-gui-frame (frame)
  "Setup GUI frame."
  ;; Faces
  ;; Default/Fixed-pitch
  (let ((ffl (font-family-list)))
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
      (set-face-attribute 'variable-pitch frame :family "Sans Serif" :height 1.0))))
  ;; Miscellaneous
  (setopt display-line-numbers-width 3))

;; General
(defun loc-setup-frame (&optional frame)
  "Setup any (TTY or GUI) frame. Assumes no global/default setup."
  (if (display-graphic-p frame)
      (loc-setup-gui-frame frame)
    (loc-setup-tty-frame frame)))

(defun loc-setup-frame-after (&optional frame)
  "Setup any (TTY or GUI) frame. Assumes global/default setup.")

;; Client
(defun loc-setup-client-frame ()
  "Setup inital client frame created by daemon/server. Assumes no global/default
setup."
  (loc-setup-frame (selected-frame))
  ;; Fixes bug of server not properly applying custom themes for first frame
  (reenable-custom-themes))

(defun loc-setup-client-frame-after ()
  "Setup inital client frame created by daemon/server. Assumes global/default
  setup.")

;; Global/Default
(defun loc-setup-global-frame ()
  "Setup global/default frame for whole session."
  (loc-setup-frame))


;;; Modes
;; Text
(defun loc-setup-text-mode ()
  "Setup modes mainly dealing with text."
  (setq-local display-line-numbers-type t)
  (display-line-numbers-mode 1)
  (hl-line-mode 0)
  (visual-line-mode 1))

;; Code/Prog
(defun loc-setup-code-mode ()
  "Setup modes mainly dealing with code."
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers-type 'relative)
  (setq-local display-line-numbers-current-absolute nil)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (visual-line-mode 0))

(provide 'loc-setup)

;;; loc-setup.el ends here
