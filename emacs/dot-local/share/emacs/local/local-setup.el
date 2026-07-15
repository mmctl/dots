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
      (set-face-attribute 'variable-pitch frame :family "Sans Serif" :height 1.0)))
    ;; Symbols
    (when (and (featurep 'nerd-icons)
               (fboundp 'nerd-icons-set-font))
      (nerd-icons-set-font nil frame))))

;; (defvar local-gui-bootstrapped-p nil
;;   "Non-nil after session-wide graphical setup has completed.")

;; (defun local-bootstrap-gui (frame)
;;   "Perform session-wide bootstrap of GUI using (initial) graphical FRAME."
;;   (unless local-gui-bootstrapped-p
;;     (with-selected-frame frame
;;       (local-reenable-custom-themes)
;;       (when-let* ((ffl (font-family-list frame)))
;;         (cond
;;          ((member "Iosevka" ffl)
;;           ;; (set-face-attribute 'default frame :family "Iosevka" :height 140 :weight 'medium)
;;           ;; (set-frame-font (font-spec :family "Iosevka" :height 140 :weight 'medium) nil t)
;;           (set-frame-font (font-spec :family "Iosevka" :size 14.0 :weight 'medium))
;;           (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 1.0))
;;           ((member "Iosevka Nerd Font" ffl)
;;            ;; (set-face-attribute 'default frame :family "Iosevka Nerd Font" :height 140 :weight 'medium)
;;            (set-frame-font (font-spec :family "Iosevka Nerd Font" :height 140 :weight 'medium) nil t)
;;            (set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font" :height 1.0))
;;           ((member "Monaspace Neon" ffl)
;;            ;; (set-face-attribute 'default frame :family "Monaspace Neon" :height 120 :weight 'medium)
;;            (set-frame-font (font-spec :family "Monaspace Neon" :height 120 :weight 'medium) nil t)
;;            (set-face-attribute 'fixed-pitch nil :family "Monaspace Neon" :height 1.0)
;;            (when (member "Monaspace Radon" ffl)
;;              (set-face-attribute 'font-lock-comment-face nil :family "Monaspace Radon")))
;;           ((member "MonaspiceNe Nerd Font" ffl)
;;            ;; (set-face-attribute 'default frame :family "MonaspiceNe Nerd Font" :height 120 :weight 'medium)
;;            (set-frame-font (font-spec :family "MonaspiceNe Nerd Font" :height 120 :weight 'medium) nil t)
;;            (set-face-attribute 'fixed-pitch nil :family "MonaspiceNe Nerd Font" :height 1.0)
;;            (when (member "MonaspiceRn Nerd Font" ffl)
;;              (set-face-attribute 'font-lock-comment-face nil :family "MonaspiceRn Nerd Font")))
;;           (t
;;            (set-frame-font (font-spec :family "Monospace" :height 120 :weight 'medium) nil t)
;;            (set-face-attribute 'fixed-pitch nil :family "Monospace" :height 1.0)))
;;          ;; Variable-pitch
;;          (cond
;;           ((and (equal (face-attribute 'fixed-pitch :family frame) "Iosevka")
;;                 (member "Iosevka Aile" ffl))
;;            (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 1.0 :weight 'medium))
;;           ((and (equal (face-attribute 'fixed-pitch :family frame) "Iosevka Nerd Font")
;;                 (member "Iosevka Nerd Font Propo" ffl))
;;            (set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font Propo" :height 1.0 :weight 'medium))
;;           ((member "Open Sans" ffl)
;;            (set-face-attribute 'variable-pitch nil :family "Open Sans" :height 1.0))
;;           (t
;;            (set-face-attribute 'variable-pitch nil :family "Sans Serif" :height 1.0)))))
;;     (setopt display-line-numbers-width 3)
;;     (setq local-gui-bootstrapped-p t)))

;; (defun local-setup-gui-frame (frame)
;;   "Set up graphical frame."
;;   ;; Bootstrap
;;   (local-bootstrap-gui frame)
;;   ;; Symbols
;;   (when (and (featurep 'nerd-icons)
;;              (fboundp 'nerd-icons-set-font))
;;     (with-selected-frame frame
;;       (nerd-icons-set-font nil frame))))

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
  (unless (or (frame-parameter frame 'client)
              (frame-parameter frame 'server-dummy-buffer))
    (local-setup-frame frame)))

(defun local-setup-client-frame ()
  "Set up the selected real Emacs server client frame."
  (let ((frame (selected-frame)))
    (when (frame-parameter frame 'client)
      (local-setup-frame frame)
      (local-reenable-custom-themes))))

;; (defun local-setup-client-frame ()
;;   "Setup initial client frame created by daemon/server. Assumes no global/default
;; setup."
;;   (local-setup-frame (selected-frame))
;;   (local-reenable-custom-themes))

;; Global/Default
;; (defun local-setup-global-frame ()
;;   "Setup global/default frame for whole session."
;;   (local-setup-frame))


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
