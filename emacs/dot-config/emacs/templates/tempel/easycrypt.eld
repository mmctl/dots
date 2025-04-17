;; easycrypt.eld
;; Tempel templates for easycrypt mode
;; Doesn't include possibilities that are too close to single keywords
;; (as these should be covered by another mechanism)

easycrypt-mode

;; Internal
(pragmap "pragma +" q ".")
(pragmam "pragma -" q ".")

;; Comments and documentation
;;; Regular comments
(comment "(* " q " *)")
(commentn "(*" n>
          q n
          "*)" >)

;;;; Aliases
(com (i comment))
(comn (i commentn))


;;; Documentation comments
(doccomment "(** " q " **)")
(doccommentn "(**" n>
             q n
             "**)" >)
(doccommentf "(*^" n>
             (file-name-nondirectory (or (buffer-file-name) (buffer-name))) n
             n>
             "Author: " p n>
             "Maintainer: " p n>
             "Date: " (pfl (format-time-string "%Y-%m-%d")) n>
             "Description:" n>
             q n
             "^*)" >)

;;;; Aliases
(dc (i doccomment))
(dcn (i doccommentn))
(dcf (i doccommentf))


;; Meta
(require "require " q ".")
(import "import " q ".")
(requireimport "require import " q ".")

(fromrequireimport "from " p " require import " q)
(fromjasminrequireimport "from Jasmin " p " require import " q)

(clone "clone " q ".")
(cloneas "clone " p " as " q ".")
(cloneimport "clone import " q ".")
(cloneimportas "clone import " p " as " q ".")
(cloneimportwith "clone import " p " with" n>
                 q ".")
(cloneimportaswith "clone import " p " as " p " with" n>
                   q ".")
(cloneinclude "clone include " q ".")
(cloneincludeas "clone include " p " as " q ".")
(cloneincludewith "clone include " p " with" n>
                  q ".")
(cloneincludeaswith "clone include " p " as " p " with" n>
                    q ".")

(hintsimplify "hint simplify " q)
(hintrewrite "hint rewrite " p " : " q)
(hintexact "hint exact " p " : " q)

;;; Aliases
(req (i require))
(im (i import))
(rim (i requireimport))

(frim (i fromimport))
(frjim (i fromimport))

(cl (i clone))
(cla (i cloneas))
(clim (i cloneimport))
(clima (i cloneimportas))
(climw (i cloneimportwith))
(climaw (i cloneimportaswith))
(clin (i cloneinclude))
(clina (i cloneincludeas))
(clinw (i cloneincludewith))
(clinaw (i cloneincludeaswith))


;; Scope

;; Functional specification
(type "type " p " = " q ".")
(typeinfo "type " (p "Name") " = " (p "Definition") ".")
(typen "type " p " =" n>
       q "." >)

(op "op " p " : " p " = " q ".")
(opinfo "op " (p "OpName") " (" (p "ArgName") " : " (p "ArgType") ") : " (p "OpType") " = " (p "Definition") ".")
(opn "op " p " : " p " =" n>
     q "." >)
(opabs "op " p " : " q ".")
(opas "op " p " : { " p " | " p " } as " q ".")
(opasinfo "op " (p "OpName") " : { " (p "OpType") " | " (p "AxiomStatement") " } as " (p "AxiomName") ".")

(const "const " p " : " p " = " q ".")
(constn "const " p " : " p " =" n>
        q "." >)
(constabs "const " p " : " q ".")

(abbrev "abbrev " p " = " q ".")
(abbrevn "abbrev " p " =" n>
         q "." >)

(theory "theory " (p "" thid)  "." n>
        q n>
        "end " (s thid) "." >)
(abstracttheory "abstract " (i theory))

(section "section." n>
         q >
         "end section." >)
(sectionid "section" (p "" secid) "." n>
           q >
           "end section " (s secid) "." >)

(declaremodule "declare module " p " <: " p "{ " q " }.")
(declaremoduleinfo "declare module " (p "Name")  " <: " (p "Type")  "{ " (p "+/- Module") " }.")
(declaremodulenm "declare module " p " <: " q ".")

(pr "Pr[" p " @ " p " : " q "]")
(prinfo "Pr[" (p "Mod.Proc(Args)") " @ " (p "Memory") " : " (p "Event") "]")
(prn "Pr[" p " @ " p " :" n>
     q "]" >)
(prns "Pr[" n>
      p " @ " p " :" n>
      q "]" >)
(prm "Pr[" q " @ &m : " q "]")
(prmn "Pr[" q " @ &m : " n>
      q "]" >)
(prdef "Pr[" q " @ &m : res]")

(hoare "hoare[" p " : " p " ==> " q "]")
(hoareinfo "hoare[" (p "Mod.Proc") " : " (p "Pre") " ==> " (p "Post") "]")
(hoaren "hoare[" n>
        p " :" n>
        p " ==> " p n>
        "]" >)
(hoarens "hoare[" n>
         p " :" n>
         p n>
         "==>" n>
         q n>
         "]" >)

(phoare "phoare[" p " : " p " ==> " p "] = " q)
(phoareinfo "phoare[" (p "Mod.Proc") " : " (p "Pre") " ==> " (p "Post") "] = " (p "Prob"))
(phoaren "phoare[" n>
         p " :" n>
         p " ==> " p n>
         "] = " q >)
(phoarens "phoare[" n>
          p " :" n>
          p n>
          "==>" n>
          p n>
          "] = " q >)
(phoare1 "phoare[" p " : " p " ==> " q "] = 1%r")
(phoare1n "phoare[" n>
          p " :" n>
          p " ==> " q n>
          "] = 1%r" >)
(phoare1ns "phoare[" n>
           p " :" n>
           p n>
           "==>" n>
           q n>
           "] = 1%r" >)

(equiv "equiv[" p " ~ " p " : " p " ==> " q "]")
(equivn "equiv[" n>
        p " ~ " p " :" n>
        p " ==> " q n>
        "]" >)
(equivns "equiv[" n>
         p n>
         "~" n>
         p " :" n>
         p "==>" q n>
         "]" >)
(equivnsp "equiv[" n>
          p " ~ " p n>
          p n>
          "==>" n>
          q "]" >)

(axiom "axiom " p " : " q ".")
(axiominfo "axiom " (p "AxiomName") " (" (p "ArgName") " : " (p "ArgType") ") : " (p "Statement") ".")
(axiomn "axiom " p " :" n>
        q "." >)

(proof "proof." n>
       q n>
       "qed." >)
(proofinfo "proof." n>
        (p "Proof") n>
        "qed." >)
(proofl "proof." q "qed.")

(lemma "lemma " p " : " p "." n>
       (i proof))
(lemmainfo "lemma " (p "LemmaName") " (" (p "ArgName") " : " (p "ArgType") ") : " (p "Statement") "." n>
        (i proofi))
(lemmaby "lemma " p " : " p " by " q ".")
(lemmapq "lemma " p " : " p "." n>
         (i proofl) >)
(lemman "lemma " p " :" n>
        p "." n>
        (i proof))
(lemmanpq "lemma " p " :" n>
          p "." n>
          (i proofl))

(lemmahoare "hoare " p " : " p " : " p " ==> " p "." n>
             (i proof))
(lemmahoarei "hoare " (p "Name") " : " (p "Mod.Proc") " : " (p "Pre") " ==> " (p "Post") "." n>
             (i proofi))
(lemmapqhoare "hoare " p " : " p " : " p " ==> " p "." n>
              (i proofl))
(lemmanhoare "hoare " p " :" n>
             p " : " p " ==> " p "." n>
             (i proof))
(lemmanhoaren "hoare " p " :" n>
              p " :" n>
              p " ==> " p "." n>
              (i proof))
(lemmanhoarens "hoare " p " :" n>
               p " :" n>
               p n>
               "==>" n>
               p "." n>
               (i proof))

(lemmall "lemma " p " : islossless " p "." n>
         (i proof))
(lemmallinfo "lemma " (p "Name") " : islossless " (p "Mod.Proc") "." n>
          (i proofi))
(lemmapqll "lemma " p " : islossless " p "." n>
           (i proofl))

(lemmaequiv "equiv Eqv_" p " : " p " ~ " p " : " p " ==> "p "." n>
            (i proof))
(lemmaequivinfo "equiv " (p "Name") " : " (p "Mod1.Proc1") " ~ " (p "Mod2.Proc2") " : " (p "Pre") " ==> " (p "Post") "." n>
                (i proofi))
(lemmapqequiv "equiv Eqv_" p " : " p " ~ " p " : " p " ==> "p "." n>
              (i proofl))
(lemmanequiv "equiv Eqv_" p " :" n>
             p " ~ " p " : " p " ==> "p "." n>
             (i proof))
(lemmanequivn "equiv Eqv_" p " :" n>
              p " ~ " p " :" n>
              p " ==> "p "." n>
              (i proof))
(lemmanequivns "equiv Eqv_" p " :" n>
               p " ~ " p " :" n>
               p n>
               "==>" n>
               p "." n>
               (i proof))
(lemmanequivnsp "equiv Eqv_" p " :" n>
                p n>
                "~" n>
                p " :" n>
                p n>
                "==>" n>
                p "."
                (i proof))

(fun "fun (" p ") => " q)
(funn "fun (" p ") =>" n>
      q)
(forall "forall (" p "), " q)
(foralln "forall (" p ")," n>
         q)
(exists "exists (" p "), " q)
(existsn "exists (" p ")," n>
         q)
(ite "if " p " then " p " else " q)
(iten "if " p n>
      "then " p n>
      "else " q)
(ge0 "0 <= " q)
(gt0 "0 < " q)
(rngin p " <= " p " <= " q)
(rngex p " < " p " < " q)
(rnginex p " <= " p " < " q)
(rngexin p " < " p " <= " q)
(letin "let " p " in " q)
(letinn "let " p n>
        "in " q)

;;; Aliases
(ty (i type))
(tyi (i typeinfo))
(tyn (i typen))
(opi (i opinfo))
(opa (i opabs))
(abv (i abbrev))
(abvn (i abbrevn))
(th (i theory))
(absth (i abstractheory))
(sec (i section))
(secid (i sectionid))
(decm (i declaremodule))
(decmi (i declaremoduleinfo))
(decmnm (i declaremodulenm))

(ho (i hoare))
(hoi (i hoareinfo))
(hon (i hoaren))
(hons (i hoaren))
(ph (i phoare))
(phi (i phoareinfo))
(phn (i phoaren))
(phn (i phoarens))
(ph1 (i phoare1))
(ph1n (i phoare1n))
(ph1ns (i phoare1ns))
(eq (i equiv))
(eqi (i equivinfo))
(eqn (i equivn))
(eqns (i equivn))
(eqnsp (i equivn))

(ax (i axiom))
(axi (i axiominfo))
(axn (i axiomn))
(lem (i lemma))
(lemi (i lemmainfo))
(lemby (i lemmaby))
(lempq (i lemmapq))
(lemn (i lemman))

(lemho (i lemmahoare))
(lemhoi (i lemmahoareinfo))
(lempqho (i lemmapqhoare))
(lemnho (i lemmanhoare))
(lemnhon (i lemmanhoaren))
(lemnhons (i lemmanhoarens))
(lemll (i lemmall))
(lemlli (i lemmallinfo))
(lempqll (i  lemmapqll))
(lemeq (i lemmaequiv))
(lemeqi (i lemmaequivinfo))
(lempqeq (i lemmapqequiv))
(lemneq (i lemmanequiv))
(lemneqn (i lemmanequivn))
(lemneqns (i lemmanequivns))
(lemneqnsp (i lemmanequivnsp))


;; Imperative specification
(procsig "proc " p "(" p ") : " q)
(procsiginfo "proc " (p "ProcName") "(" (p "ArgName") " : " (p "ArgType") ") : " (p "ProcType"))
(procsiglim "proc " p "(" p ") : " q " { " q " }")

(var "var " p " : " q)
(varinfo "var " (p "VarName") " : " (p "VarType"))
(varp (i var) ";")
(varpinfo (i varinfo) ";")

(proc "proc " p "(" p ") : " p " = {" n>
      q n>
      "}" >)
(procinfo "proc " (p "ProcName") "(" (p "ArgName") " : " (p "ArgType") ") : " (p "ProcType") " = {" n>
          (i varpinfo) n>
          (p "Body") n>
          "}" >)

(procnt "proc " p "(" p ") = {" n>
        q n>
        "}" >)
(procl "proc " p "(" p ") : " p " = { " q " }")
(proclnt "proc " p "(" p ") = { " q " }")
(procalias "proc " p " = " q)
(procaliasinfo "proc " (p "Name") " = " (p "Mod.Proc"))

(moduletype "module type " p " = {" n>
            q n>
            "}." >)
(moduletypep "module type " p " (" p ") = {" n>
             q n>
             "}." >)
(moduletypeinfo "module type " (p "ModTypeName") " (" (p "ArgName") " : " (p "ArgType") ") = {" n>
                (i procsiginfo) n>
                "}." >)

(module "module " p " = {" n>
        q n>
        "}." >)
(moduleinfo "module " (p "ModName") " (" (p "ArgName") " : " (p "ArgType") ") : " (p "ModType") " = {" n>
            (i varinfo) n>
            (i procinfo) n>
            "}." >)
(modulep "module " p "(" p ") = {" n>
         q n>
         "}." >)
(modulet "module " p " : " p " = {" n>
         q n>
         "}." >)
(modulept "module " p " (" p ") : " p " = {" n>
          q n>
          "}." >)

(modulealias "module " p " = " q ".")
(modulealiasinfo "module " (p "ModAliasName") " = " (p "Mod(Args)") ".")
(modulepalias "module " p " = " p "(" q ").")

(if "if (" p ") {" n>
    q n>
    "}" >)
(ifelse "if (" p ") {" n>
        p n>
        "} else {" > n>
        q n>
        "}" >)
(ifelifelse "if (" p ") {" n>
            p n>
            "} elif {" > n>
            p n>
            "} else {" > n>
            q n>
            "}" >)

(while "while (" p ") {" n>
       q
       "}" >)

(assign p "<-" q ";")
(call p "<@" q ";")
(sample p "<$" q ";")

;;; Aliases
(prcs (i procsig))
(prcsi (i procsiginfo))
(prcsl (i procsiglim))

(prc (i proc))
(prci (i procinfo))
(prcnt (i procnt))
(prcl (i procl))
(prclnt (i proclnt))
(prca (i procalias))
(prcai (i procaliasinfo))

(modty (i moduletype))
(modtyi (i moduletypeinfo))
(modtyp (i moduletypep))

(mod (i module))
(modi (i moduleinfo))
(modp (i modulep))
(modt (i modulet))
(modpt (i modulept))

(moda (i modulealias))
(modai (i modulealiasinfo))
(modpa (i modulepalias))

(vi (i varinfo))
(vp (i varp))
(vpi (i varpinfo))

(ife (i ifelse))
(ifee (i ifelifelse))

(wh (i while))

(ca (i call))
(sam (i sample))


;; Tactics
;;; Ambient
(smt "smt(" q ")")
(smtinfo "smt(" (p "LemmaName or @TheoryName") ")")

(movein "move=> " q)
(movegen "move: " q)

(pose "pose " p " := " q)

(have "have " p ": " q)
(haveby "have " p ": " p " by " q ".")
(haver "have ->: " q)
(havel "have <-: " q)
(haves "have /#: " q)

(applyin "apply " p " in " q)

(rewrited "rewrite (:" q ")")
(rewriter "rewrite (:" p " = " q ")")
(rewritel "rewrite -(:" p " = " q ")")
(rewritein "rewrite " p " in " q)

;;;; Aliases
(mvin (i movein))
(mvgn (i movegen))

(ps (i pose))

(hv (i have))
(hvby (i haveby))
(hvr (i haveby))
(hvl (i havel))
(hvs (i haves))

(appin (i applyin))

(rwd (i rewrited))
(rwr (i rewriter))
(rwl (i rewritel))
(rwin (i rewritein))


;;; Program
(proctinfo "proc (" (p "Invariant") ")")
(procubinfo "proc (" (p "BadEvent") ") (" (p "InvariantUptoBad") ")")
(procubeinfo "proc (" (p "BadEvent") ") (" (p "InvariantUptoBad") ") (" (p "InvariantAfterBad") ")")

(seqe "seq " p " " p " :  (" q ")")
(seqeinfo "seq " (p "LineNumLeft") " " (p "LineNumRight") " :  (" (p "Predicate") ")")
(seqp "seq " p " : (" p ") (" p ") (" p ") (" p ") (" p ") (" q ")")
(seqpinfo "seq " (p "LineNum") " : (" (p "Event") ") (" (p "ProbTrueBegMid") ") (" (p "ProbTrueMidEnd") ") (" (p "ProbFalseBegMid") ") (" (p "ProbFalseMidEnd") ") (" (p "InvariantMid") ")")
(seqh "seq " p " :  (" q ")")
(seqhinfo "seq " (p "LineNum") " :  (" (p "Predicate") ")")

(sp1 "sp{1} " q)
(sp2 "sp{2} " q)
(wp1 "wp{1} " q)
(wp2 "wp{2} " q)

(rnd1 "rnd{1} " q)
(rnd2 "rnd{2} " q)

(if1 "if{1} " q)
(if2 "if{1} " q)

(while1 "while{1} (" p ") (" q ")")
(while2 "while{1} (" p ") (" q ")")
(while1info "while{1} " (p "Invariant") (p "TerminationVariant")) ; TODO: ask PY if this is correct
(while2info "while{2} " (p "Invariant") (p "TerminationVariant"))
(whilehinfo "while " (p "Invariant"))
(whilepinfo "while " (p "Invariant") (p "TerminationVariant") (p "UpperBndTermVar") (p "ProbDecreaseTermVar")) ; TODO: ask PY if correct
(whileeinfo "while " (p "Invariant"))


;; Advanced/Specific
(prub (i pr) n>
      "<=" n>
      (i pr))
(prnub (i prn) n>
       "<=" n>
       (i prn))
(prnsub (i prns) n>
        "<=" n>
        (i prns))
(prmub (i prm) n>
       "<=" n>
       (i prm))
(prmnub (i prmn) n>
        "<=" n>
        (i prmn))
(prubdef (i prdef) n>
         "<=" n>
         (i prdef))

(preq (i pr) n>
      "=" n>
      (i pr))
(prneq (i prn) n>
       "=" n>
       (i prn))
(prnseq (i prns) n>
        "=" n>
        (i prns))
(prmeq (i prm) n>
       "=" n>
       (i prm))
(prmneq (i prmn) n>
        "=" n>
        (i prmn))
(preqdef (i prdef) n>
         "=" n>
         (i prdef))
