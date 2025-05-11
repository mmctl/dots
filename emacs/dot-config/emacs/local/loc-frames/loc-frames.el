;; -*- lexical-binding: t -*-
;; loc-frames.el (functions/hooks)

;; Setup
;;; TTY
(defun loc-setup-tty-frame (frame)
  "Setup TTY frame."
  (set-face-attribute 'default frame :background "unspecified")
  (setopt display-line-numbers-width 3))

;;; GUI
(defun loc-setup-gui-frame (frame)
  "Setup GUI frame."
  ;;;; Faces
  (cond
   ((member "MonaspiceAr Nerd Font" (font-family-list))
    (set-face-attribute 'default frame :family "MonaspiceAr Nerd Font"
                        :height 120 :weight 'medium)
    (set-face-attribute 'mode-line frame :family "MonaspiceAr Nerd Font")
    (set-face-attribute 'mode-line-active frame :family "MonaspiceAr Nerd Font")
    (set-face-attribute 'mode-line-inactive frame :family "MonaspiceAr Nerd Font")
    (set-face-attribute 'minibuffer-prompt frame :family "MonaspiceAr Nerd Font"))
   ((member "Monaspace Argon" (font-family-list))
    (set-face-attribute 'default frame :family "Monaspice Argon" :height 120 :weight 'medium)
    (set-face-attribute 'mode-line frame :family "Monaspace Argon")
    (set-face-attribute 'mode-line-active frame :family "Monaspace Argon")
    (set-face-attribute 'mode-line-inactive frame :family "Monaspace Argon")
    (set-face-attribute 'minibuffer-prompt frame :family "Monaspace Argon"))
   (t
    (set-face-attribute 'default '(:family "Monospace" :height 120))))
  (cond
   ((member "MonaspiceNe Nerd Font" (font-family-list))
    (set-face-attribute 'line-number frame :family "MonaspiceNe Nerd Font")
    (set-face-attribute 'line-number-current-line frame :family "MonaspiceNe Nerd Font"))
   ((member "Monaspace Neon" (font-family-list))
    (set-face-attribute 'line-number frame :family "Monaspace Neon")
    (set-face-attribute 'line-number-current-line frame :family "Monaspace Neon")))
  (cond
   ((member "MonaspiceKr Nerd Font" (font-family-list))
    (set-face-attribute 'fixed-pitch frame :family "MonaspiceKr Nerd Font"
                        :height 120 :weight 'medium))
   ((member "Monaspace Krypton" (font-family-list))
    (set-face-attribute 'fixed-pitch frame :family "Monaspace Krypton"
                        :height 120 :weight 'medium)))
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
  "Setup inital client frame created by daemon/server. Assumes no global/default setup."
  (loc-setup-frame (selected-frame)))

(defun loc-setup-client-frame-after ()
  "Setup inital client frame created by daemon/server. Assumes global/default setup.")


;; Global/Default
(defun loc-setup-global-frame ()
  "Setup global/default frame for whole session."
  (loc-setup-frame))


(provide 'loc-frames)

;;; loc-frames.el ends here
