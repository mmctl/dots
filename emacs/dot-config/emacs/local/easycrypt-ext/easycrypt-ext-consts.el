;; -*- lexical-binding: t -*-
;; easycrypt-ext-consts.el

;; "Special" keywords
(defconst ece-internal-keywords
  (list "debug" "fail" "pragma" "prover" "time" "timeout"
        "undo"))

(defconst ece-functionality-keywords
  (list "exit" "goal" "locate" "print" "search" "why3"))

(defconst ece-meta-keywords
  (list "as" "clone" "from" "hint" "export" "import"
        "include" "remove" "rename" "require" "with"))

(defconst ece-scope-keywords
  (list "declare" "local"))

(defconst ece-proof-start-keywords
  (list "proof" "realize"))

(defconst ece-proof-end-keywords
  (list "qed"))

(defconst ece-proof-delimit-keywords
  (append ece-proof-start-keywords
          ece-proof-end-keywords))

(defconst ece-structural-keywords
  (list "section" "Self" "Top"))

;; Specification keywords
(defconst ece-proof-spec-keywords
  (list "equiv" "hoare" "lemma" "clone"))

(defconst ece-functional-spec-start-keywords
  (list "abbrev" "abstract" "axiom" "class" "const" "eager"
        "ehoare" "equiv" "hoare" "inductive" "instance" "lemma"
        "nosmt" "notation" "op" "phoare" "pred" "subtype"
        "theory" "type"))

(defconst ece-functional-spec-other-keywords
  (list "axiomatized" "else" "end" "exists" "forall" "fun"
        "glob" "if" "in" "islossless" "let" "of"
        "Pr" "res" "then"))

(defconst ece-functional-spec-keywords
  (delete-dups (append ece-proof-spec-keywords
                       ece-functional-spec-start-keywords
                       ece-functional-spec-other-keywords)))

(defconst ece-imperative-spec-keywords
  (list "assert" "elif" "else" "for" "if" "import"
        "include" "is" "match" "module" "proc" "return"
        "var" "while"))

(defconst ece-spec-keywords
  (delete-dups (append ece-functional-spec-keywords
                       ece-imperative-spec-keywords)))

;;;; Tactics keywords
(defconst ece-tactics-regular-keywords
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

(defconst ece-tactics-closing-keywords
  (list "admit" "admitted" "assumption" "by"
        "check" "done" "edit" "exact" "fix" "reflexivity"
        "smt" "solve"))

(defconst ece-tactics-dangerous-keywords
  (list "admit" "admitted"))

(defconst ece-tactics-tactical-keywords
  (list "do" "expect" "first" "last" "strict" "try"))

(defconst ece-tactics-keywords
  (delete-dups (append ece-tactics-regular-keywords
                       ece-tactics-closing-keywords
                       ece-tactics-dangerous-keywords
                       ece-tactics-tactical-keywords)))


;; All keywords
(defconst ece-keywords
  (delete-dups (append ece-internal-keywords
                       ece-functionality-keywords
                       ece-meta-keywords
                       ece-scope-keywords
                       ece-spec-keywords
                       ece-tactics-keywords)))

;; All "start" keywords (i.e., regularly starting a sentence, excluding inside proof and programs)
(defconst ece-start-keywords
  (delete-dups (append ece-internal-keywords
                       ece-functionality-keywords
                       ece-meta-keywords
                       ece-scope-keywords
                       ece-proof-start-keywords
                       ece-proof-end-keywords
                       ece-proof-spec-keywords
                       ece-functional-spec-start-keywords)))


(provide 'easycrypt-ext-consts)

;;; easycrypt-ext-consts.el ends here
