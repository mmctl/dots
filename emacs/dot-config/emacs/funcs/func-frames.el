;; -*- lexical-binding: t -*-
;; func-frames.el (functions/hooks)
;; Setup
;;; TTY
(defun setup-a-tty-frame (frame)
  "Setup TTY frame."
  (set-face-attribute 'default frame :background "unspecified")
  (setopt display-line-numbers-width 3))

;;; GUI
(defun setup-a-gui-frame (frame)
  "Setup GUI frame."
  ;;;; Faces
  (cond
   ((member "MonaspiceAr Nerd Font" (font-family-list))
    (set-face-attribute 'default frame :family "MonaspiceAr Nerd Font"
                        :height 110 :weight 'medium)
    (set-face-attribute 'mode-line frame :family "MonaspiceAr Nerd Font")
    (set-face-attribute 'mode-line-active frame :family "MonaspiceAr Nerd Font")
    (set-face-attribute 'mode-line-inactive frame :family "MonaspiceAr Nerd Font")
    (set-face-attribute 'minibuffer-prompt frame :family "MonaspiceAr Nerd Font"))
   ((member "Monaspace Argon" (font-family-list))
    (set-face-attribute 'default frame :family "Monaspice Argon" :height 110 :weight 'medium)
    (set-face-attribute 'mode-line frame :family "Monaspace Argon")
    (set-face-attribute 'mode-line-active frame :family "Monaspace Argon")
    (set-face-attribute 'mode-line-inactive frame :family "Monaspace Argon")
    (set-face-attribute 'minibuffer-prompt frame :family "Monaspace Argon")))
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
                        :height 110 :weight 'medium))
   ((member "Monaspace Krypton" (font-family-list))
    (set-face-attribute 'fixed-pitch frame :family "Monaspace Krypton"
                        :height 110 :weight 'medium)))
  (setopt display-line-numbers-width 3))


;; General
(defun setup-a-frame (&optional frame)
  "Setup any (TTY or GUI) frame. Assumes no global/default setup."
  (if (display-graphic-p frame)
      (setup-a-gui-frame frame)
    (setup-a-tty-frame frame)))

(defun setup-a-frame-after (&optional frame)
  "Setup any (TTY or GUI) frame. Assumes global/default setup.")


;; Client
(defun setup-a-client-frame ()
  "Setup inital client frame created by daemon/server. Assumes no global/default setup."
  (setup-a-frame (selected-frame)))

(defun setup-a-client-frame-after ()
  "Setup inital client frame created by daemon/server. Assumes global/default setup.")


;; Global/Default
(defun setup-a-global-frame ()
  "Setup global/default frame for whole session."
  (setup-a-frame))


(provide 'func-frames)

;;; func-frames.el ends here
