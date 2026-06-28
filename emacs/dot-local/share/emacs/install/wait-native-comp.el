;; -*- lexical-binding: t -*-
;; wait-native-comp.el
;; Meant to be run at the end of initial non-interactive installation/setup
;; (e.g., via `--batch')

;; Recursively queue all installed packages for native compilation
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (native-compile-async (list package-user-dir) t))

;; Wait for native compilation processes to finish
(message "Waiting for native compilation to finish...")

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (require 'comp-run)

  (while (or comp-files-queue
             (> (comp--async-runnings) 0))
    (accept-process-output nil 0.1)))

(message "Native compilation finished.")
