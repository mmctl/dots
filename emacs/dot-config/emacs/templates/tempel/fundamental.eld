;; fundamental.eld
;; Tempel templates for fundamental mode

fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "%Y-%0m-%0d %a %H:%0M"))
(yesterday (format-time-string "%Y-%0m-%0d %a" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "%Y-%0m-%0d %a" (time-add nil (* 24 60 60))))

;;; fundamental.eld ends here
