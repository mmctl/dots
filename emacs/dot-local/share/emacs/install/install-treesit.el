;; -*- lexical-binding: t -*-
;; install-treesit.el
;; Meant to be run with `early-init.el' and `init.el' loaded.

(require 'treesit)

(defconst TREESIT_LANGUAGE_SOURCES
  '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
    (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.6")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
    (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
    (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")
    (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "v0.24.2" "grammars/ocaml/src")
    (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" "v0.24.2" "grammars/interface/src"))
  "Alist of tree-sitter grammars to install, formatted for `treesit-language-source-alist'.")


(dolist (gramal TREESIT_LANGUAGE_SOURCES)
  (let ((lang (car gramal)))
    (add-to-list 'treesit-language-source-alist gramal)
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang TREESIT_DIR))))
