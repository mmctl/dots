;; -*- lexical-binding: t -*-
;; easycrypt-ext-consts.el
;; Note: extracting these from source would be more robust
;; (E.g., extract syntax elements from syntax table and keywords from parser)

;; Syntax
(defconst ece-delimiters-comments-open
  (list "(*" "(*&" "(*^"))

(defconst ece-delimiters-comments-close
  (list "*)" "&*)" "^*)"))

(defconst ece-delimiters-expression-open
  (list ?\[ ?\())

(defconst ece-delimiters-expression-close
  (list ?\] ?\)))

(defconst ece-delimiters-code-open
  (list ?\{))

(defconst ece-delimiters-code-close
  (list ?\}))

(defconst ece-delimiters-open
  (eval-when-compile
    (append ece-delimiters-comments-open
            (mapcar #'string ece-delimiters-expression-open)
            (mapcar #'string ece-delimiters-code-open))))

(defconst ece-delimiters-close
  (eval-when-compile
    (append ece-delimiters-comments-close
            (mapcar #'string ece-delimiters-expression-close)
            (mapcar #'string ece-delimiters-code-close))))

(defconst ece-delimiters
  (eval-when-compile
    (append ece-delimiters-open ece-delimiters-close)))

(defconst ece-bullets-proof
  (list ?+ ?- ?*))

;; "Special" keywords
(defconst ece-keywords-internal
  (list "debug" "fail" "pragma" "prover" "time" "timeout"
        "undo"))

(defconst ece-keywords-functionality
  (list "exit" "goal" "locate" "print" "search" "why3"))

(defconst ece-keywords-meta
  (list "as" "clone" "from" "hint" "export" "import"
        "include" "remove" "rename" "require" "with"))

(defconst ece-keywords-scope
  (list "declare" "local" "global"))

(defconst ece-keywords-proof-start
  (list "proof" "realize"))

(defconst ece-keywords-proof-end
  (list "qed"))

(defconst ece-keywords-proof-delimit
  (eval-when-compile
    (append ece-keywords-proof-start
            ece-keywords-proof-end)))

(defconst ece-keywords-structural
  (list "section" "Self" "Top"))

;; Specification keywords
(defconst ece-keywords-proof-spec-start
  (list "equiv" "hoare" "lemma" "clone"))

(defconst ece-keywords-functional-spec-start
  (list "abbrev" "abstract" "axiom" "class" "const" "eager"
        "ehoare" "equiv" "hoare" "inductive" "instance" "lemma"
        "nosmt" "notation" "op" "phoare" "pred" "subtype"
        "theory" "type"))

(defconst ece-keywords-functional-spec-other
  (list "axiomatized" "else" "end" "exists" "forall" "fun"
        "glob" "if" "in" "islossless" "let" "of"
        "Pr" "res" "then"))

(defconst ece-keywords-functional-spec
  (eval-when-compile
    (delete-dups (append ece-keywords-functional-spec-start
                         ece-keywords-functional-spec-other))))

(defconst ece-keywords-imperative-spec-start
  (list "elif" "else" "if" "match" "module" "proc" "while"))

(defconst ece-keywords-imperative-spec-start-scope
  (eval-when-compile
    (append ece-keywords-imperative-spec-start
            ece-keywords-scope)))

(defconst ece-keywords-imperative-spec-other
  (list "assert" "for" "import" "include" "is" "return" "var"))

(defconst ece-keywords-imperative-spec
  (eval-when-compile
    (delete-dups (append ece-keywords-imperative-spec-start
                         ece-keywords-imperative-spec-other))))

(defconst ece-keywords-spec
  (eval-when-compile
    (delete-dups (append ece-keywords-functional-spec
                         ece-keywords-imperative-spec))))

;;;; Tactics keywords
(defconst ece-keywords-tactic-regular
  (list "algebra" "alias" "apply" "async" "auto" "beta" "byequiv"
        "byphoare" "bypr" "byupto" "call" "case" "cbv" "cfold"
        "change" "clear" "congr" "conseq" "cut" "delta"
        "dump" "eager" "ecall" "ehoare" "elim" "eta" "exfalso"
        "exlim" "fel" "field" "fieldeq" "fission" "fusion"
        "gen" "have" "hoare" "idtac" "inline" "interleave"
        "iota" "kill" "left" "logic" "modpath" "move"
        "outline" "pose" "pr_bounded" "progress" "rcondf" "rcondt"
        "replace" "rewrite" "right" "ring" "ringeq" "rnd"
        "rndsem" "rwnormal" "seq" "sim" "simplify" "skip"
        "sp" "split" "splitwhile" "subst" "suff" "swap"
        "symmetry" "transitivity" "trivial" "unroll" "weakmem" "while"
        "wlog" "wp" "zeta"))

(defconst ece-keywords-tactic-close
  (list "admit" "admitted" "assumption" "by"
        "check" "done" "edit" "exact" "fix" "reflexivity"
        "smt" "solve"))

(defconst ece-keywords-tactic-dangerous
  (list "admit" "admitted"))

(defconst ece-keywords-tactic-tactical
  (list "do" "expect" "first" "last" "strict" "try"))

(defconst ece-keywords-tactic
  (eval-when-compile
    (delete-dups (append ece-keywords-tactic-regular
                         ece-keywords-tactic-close
                         ece-keywords-tactic-dangerous
                         ece-keywords-tactic-tactical))))


;; All keywords
(defconst ece-keywords
  (eval-when-compile
    (delete-dups (append ece-keywords-internal
                         ece-keywords-functionality
                         ece-keywords-meta
                         ece-keywords-scope
                         ece-keywords-spec
                         ece-keywords-tactic))))

;; All "start" keywords (i.e., regularly starting a sentence, excluding inside proof and programs)
(defconst ece-keywords-start
  (eval-when-compile
    (delete-dups (append ece-keywords-internal
                         ece-keywords-functionality
                         ece-keywords-meta
                         ece-keywords-scope
                         ece-keywords-proof-spec-start
                         ece-keywords-functional-spec-start))))


(provide 'easycrypt-ext-consts)

;;; easycrypt-ext-consts.el ends here
