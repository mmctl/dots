;; -*- lexical-binding: t -*-
;; func-utils.el
;; Quitting
(defun save-buffers-kill-terminal-silent ()
  "Execute save-buffers-kill-terminal, automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-terminal t))

(defun save-buffers-kill-emacs-silent ()
  "Execute save-buffers-kill-emacs, automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-emacs t))

(defun save-buffers-restart-emacs ()
  "Execute save-buffers-kill-emacs, restarting Emacs afterward."
  (interactive)
  (save-buffers-kill-emacs nil t))

(defun save-buffers-restart-emacs-silent ()
  "Execute save-buffers-restart-emacs, automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-emacs t t))


;; Indentation
;;; Basic rigid indentation
(defun a-basic-indent (arg)
  "Indent all lines touched by the active region by ARG * `tab-width`.
   If no region is active, insert ARG tabs at point or un-tab current line (ARG times)."
  (interactive "p")
  ;; If region is active,...
  (if (use-region-p)
      ;; Then, take stock of current region, point, and mark positions
      ;; and compute new region positions
      (let* ((orig-region-start (region-beginning))
             (orig-region-stop (region-end))
             (new-region-start (save-excursion (goto-char orig-region-start)
                                               (line-beginning-position)))
             (new-region-stop (save-excursion (goto-char orig-region-stop)
                                              (if (= orig-region-stop (line-beginning-position))
                                                  orig-region-stop
                                                (line-end-position))))
             (region-forward (<= orig-region-start orig-region-stop)))
        ;; Expand visual region to new region positions
        (if region-forward
            (progn (goto-char new-region-stop) (set-mark new-region-start))
          (progn (goto-char new-region-start) (set-mark new-region-stop)))
        ;; Indent visually expanded region
        (indent-rigidly new-region-start new-region-stop (* arg tab-width))
        ;; Don't deactivate-mark, so we don't have to re-select region to repeat
        (setq-local deactivate-mark nil))
    ;; Else (no region is active), if prefix argument is negative...
    (if (< arg 0)
        ;; Then, take stock of whitespace behind point, and
        (let* ((orig-point (point))
               (del-ub (min (* (abs arg) tab-width) (- orig-point (line-beginning-position))))
               (del (save-excursion
                      (skip-chars-backward "[:space:]" (- orig-point del-ub))
                      (- orig-point (point)))))
          ;; if there is at least some whitespace...
          (if (< 0 del)
              ;; Then, delete ARG * tab-width of white-space
              ;; (at most until first non-whitespace character)
              (delete-region (- orig-point del) orig-point)
            ;; Else, un-tab current line (at most) ARG times
            (indent-rigidly (line-beginning-position)
                            (line-end-position)
                            (* arg tab-width))))
      ;; Else, insert ARG tabs at point
      (insert-tab arg))))

(defun a-basic-deindent (arg)
  "De-indent all lines touched by the active region by ARG * `tab-width`.
   If no region is active, un-tab current line by ARG * `tab-width`."
  (interactive "p")
  (a-basic-indent (- arg)))

;;; Hooks
;;;; Indent when last input was }, ), or ] (meant for post-self-insert-hook)
(defun indent-on-insertion-closer ()
  (when (memq last-command-event '(?\} ?\) ?\]))
    (save-excursion
      (indent-according-to-mode))))


(provide 'func-utils)

;;; func-utils.el ends here
