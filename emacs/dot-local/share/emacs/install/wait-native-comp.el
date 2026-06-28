;; -*- lexical-binding: t -*-
;; wait-native-comp.el
;; Meant to be run at the end of initial non-interactive installation/setup
;; (e.g., via `--batch')

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (require 'comp-run)

  (while (or comp-files-queue
             (> (comp--async-runnings) 0))
    (accept-process-output nil 0.1)))
