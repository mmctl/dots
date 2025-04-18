;; -*- lexical-binding: t -*-
;; custom-consts.el
;;; EasyCrypt
;;;; "Special" keywords
(defconst cst-easycrypt-internal-keywords
  (list "debug" "fail" "pragma" "prover" "time" "timeout"
        "undo"))

(defconst cst-easycrypt-functionality-keywords
  (list "exit" "goal" "locate" "print" "search" "why3"))

(defconst cst-easycrypt-meta-keywords
  (list "as" "clone" "from" "hint" "export" "import"
        "include" "remove" "rename" "require" "with"))

(defconst cst-easycrypt-scope-keywords
  (list "declare" "local"))

(defconst cst-easycrypt-proof-start-keywords
  (list "proof"))

(defconst cst-easycrypt-proof-end-keywords
  (list "qed"))

(defconst cst-easycrypt-proof-delimit-keywords
  (append cst-easycrypt-proof-start-keywords
          cst-easycrypt-proof-end-keywords))

(defconst cst-easycrypt-structural-keywords
  (list "section" "realize" "Self" "Top"))

;;;; Specification keywords
(defconst cst-easycrypt-proof-spec-keywords
  (list "equiv" "hoare" "lemma"))

(defconst cst-easycrypt-functional-spec-start-keywords
  (list "abbrev" "abstract" "axiom" "class" "const" "eager"
        "ehoare" "equiv" "hoare" "inductive" "instance" "lemma"
        "nosmt" "notation" "op" "phoare" "pred" "subtype"
        "theory" "type"))

(defconst cst-easycrypt-functional-spec-other-keywords
  (list "axiomatized" "else" "end" "exists" "forall" "fun"
        "glob" "if" "in" "islossless" "let" "of"
        "Pr" "res" "then"))

(defconst cst-easycrypt-functional-spec-keywords
  (delete-dups (append cst-easycrypt-proof-spec-keywords
                       cst-easycrypt-functional-spec-start-keywords
                       cst-easycrypt-functional-spec-other-keywords)))

(defconst cst-easycrypt-imperative-spec-keywords
  (list "assert" "elif" "else" "for" "if" "import"
        "include" "is" "match" "module" "proc" "return"
        "var" "while"))

(defconst cst-easycrypt-spec-keywords
  (delete-dups (append cst-easycrypt-functional-spec-keywords
                       cst-easycrypt-imperative-spec-keywords)))

;;;; Tactics keywords
(defconst cst-easycrypt-tactics-regular-keywords
  (list "algebra" "alias" "apply" "async" "auto" "beta" "byequiv"
        "byphoare" "bypr" "byupto" "call" "case" "cbv" "cfold"
        "change" "clear" "congr" "conseq" "cut" "delta"
        "dump" "eager" "ecall" "elim" "eta" "exfalso"
        "exlim" "fel" "field" "fieldeq" "fission" "fusion"
        "gen" "have" "hoare" "idtac" "inline" "interleave"
        "iota" "kill" "left" "logic" "modpath" "move"
        "outline" "pose" "pr_bounded" "progress" "rcondf" "rcondt"
        "replace" "rewrite" "right" "ring" "ringeq" "rnd"
        "rndsem" "rwnormal" "seq" "sim" "simplify" "skip"
        "sp" "split" "splitwhile" "subst" "suff" "swap"
        "symmetry" "transitivity" "trivial" "unroll" "weakmem" "while"
        "wlog" "wp" "zeta"))

(defconst cst-easycrypt-tactics-closing-keywords
  (list "admit" "admitted" "assumption" "by"
        "check" "done" "edit" "exact" "fix" "reflexivity"
        "smt" "solve"))

(defconst cst-easycrypt-tactics-dangerous-keywords
  (list "admit" "admitted"))

(defconst cst-easycrypt-tactics-tactical-keywords
  (list "do" "expect" "first" "last" "strict" "try"))

(defconst cst-easycrypt-tactics-keywords
  (delete-dups (append cst-easycrypt-tactics-regular-keywords
                       cst-easycrypt-tactics-closing-keywords
                       cst-easycrypt-tactics-dangerous-keywords
                       cst-easycrypt-tactics-tactical-keywords)))


;;;; All keywords
(defconst cst-easycrypt-keywords
  (delete-dups (append cst-easycrypt-internal-keywords
                       cst-easycrypt-functionality-keywords
                       cst-easycrypt-meta-keywords
                       cst-easycrypt-scope-keywords
                       cst-easycrypt-spec-keywords
                       cst-easycrypt-tactics-keywords)))

;;;; All "start" keywords (i.e., regularly starting a sentence, excluding inside proof and programs)
(defconst cst-easycrypt-start-keywords
  (delete-dups (append cst-easycrypt-internal-keywords
                       cst-easycrypt-functionality-keywords
                       cst-easycrypt-meta-keywords
                       cst-easycrypt-scope-keywords
                       cst-easycrypt-proof-start-keywords
                       cst-easycrypt-proof-end-keywords
                       cst-easycrypt-proof-spec-keywords
                       cst-easycrypt-functional-spec-start-keywords)))


(provide 'custom-consts)

;;; custom-consts.el ends here
