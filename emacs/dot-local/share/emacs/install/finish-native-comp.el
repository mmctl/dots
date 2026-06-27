;; -*- lexical-binding: t -*-
;; finish-native-comp.el

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (require 'comp-run)

  (while (or comp-files-queue
             (> (comp--async-runnings) 0))
    ;; Allow compiler subprocess output and sentinels to be processed.
    (accept-process-output nil 0.1)))

(message "Native compilation finished")
