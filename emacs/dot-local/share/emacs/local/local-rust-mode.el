;; -*- lexical-binding: t -*-
;; local-rust-mode.el

(require 'rust-mode)


(defun project-find-cargo-toml (dir)
  (when-let ((root (locate-dominating-file dir "Cargo.toml")))
    (cons 'cargo-toml root)))

(cl-defmethod project-root ((project (head cargo-toml)))
  (cdr project))

(defun rust-test-nocapture ()
  (interactive)
  (let ((rust-cargo-default-arguments "-- --nocapture"))
    (rust-test)))


(provide 'local-rust-mode)

;;; local-rust-mode.el ends here
