;; -*- lexical-binding: t -*-
;; local-easycrypt-ext.el


(require 'easycrypt-ext)


;; Silence, byte-compiler
(declare-function consult-ripgrep "consult")
(declare-function consult-fd "consult")

;;;###autoload
(defun ece-consult-ripgrep-standard-library ()
  "Performs `consult-ripgrep' with EasyCrypt's standard library root as
starting directory."
  (interactive)
  (consult-ripgrep (ece--standard-library-root-canonical)))

;;;###autoload
(defun ece-consult-fd-standard-library ()
  "Performs `consult-fd' with EasyCrypt's standard library root as
starting directory."
  (interactive)
  (consult-fd (ece--standard-library-root-canonical)))


(provide 'local-easycrypt-ext)

;; local-easycrypt-ext.el ends here
