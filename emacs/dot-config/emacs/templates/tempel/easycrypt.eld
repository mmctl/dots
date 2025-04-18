;; easycrypt.eld
;; Tempel templates for easycrypt mode
;; Doesn't include possibilities that are too close to single keywords
;; (as these should be covered by another mechanism)

easycrypt-mode

;; Internal
(pragmap "pragma +" q ".")
(pragmam "pragma -" q ".")

;; Comments and documentation
(comment "(* " q " *)")
(commentn "(*" n>
          q n
          "*)" >)
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
(com (i comment))
(comn (i commentn))
(doc (i doccomment))
(docn (i doccommentn))
(docf (i doccommentf))


;; Meta
(require "require " q ".")
(import "import " q ".")
(requireimport "require import " q ".")

(fromrequire "from " p " require " q)
(fromrequireimport "from " p " require import " q)
(fromjasminrequire "from Jasmin " p " require " q)
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
(imp (i import))
(reqim (i requireimport))

(froreq (i fromrequire))
(frojasreq (i fromjasminrequire))
(froreqim (i fromrequireimport))
(frojasreqim (i fromjasminrequireimport))

(clo (i clone))
(cloa (i cloneas))
(cloim (i cloneimport))
(cloima (i cloneimportas))
(cloimw (i cloneimportwith))
(cloimaw (i cloneimportaswith))
(cloin (i cloneinclude))
(cloina (i cloneincludeas))
(cloinw (i cloneincludewith))
(cloinaw (i cloneincludeaswith))


;; Scope

;; Functional specification
(type "type " p " = " q ".")
(typeinfo "type " (p "Name") " = " (p "Def") ".")
(typen "type " p " =" n>
       q "." >)

(op "op " p " : " p " = " q ".")
(opinfo "op " (p "Name") " " (p "(Param1 : Type1) ... (ParamN : TypeN)") " : " (p "Type") " = " (p "Def") ".")
(opn "op " p " : " p " =" n>
     q "." >)
(opas "op " p " : { " p " | " p " } as " q ".")
(opasinfo "op " (p "Name (Op)") " " (p "(Param1 : Type1) ... (ParamN : TypeN)") " : { " (p "Type") " | " (p "AxiomStatement") " } as " (p "Name (Axiom)") ".")

(const "const " p " : " p " = " q ".")
(constinfo "const "  (p "Name") " (" (p "Param") " : " (p "Type") ") : " (p "Type") " = " (p "Def") ".")
(constn "const " p " : " p " =" n>
        q "." >)

(abbrev "abbrev " p " = " q ".")
(abbrevinfo "abbrev "  (p "Name") " " (p "(Param1 : Type1) ... (ParamN : TypeN)") " : " (p "Type") " = " (p "Def") ".")
(abbrevt "abbrev " p " : " p " = " q ".")
(abbrevn "abbrev " p " =" n>
         q "." >)

(theory "theory " (p "" thid)  "." n>
        q n>
        "end " (s thid) "." >)
(theoryinfo "theory " (p "Name" thid)  "." n>
            (p "Body") n>
            "end " (s thid) "." >)
(abstracttheory "abstract " (i theory))

(section "section." n>
         q >
         "end section." >)
(sectioninfo "section" (p "Name" secid) "." n>
           (p "Body") >
           "end section " (s secid) "." >)
(sectionid "section" (p "" secid) "." n>
           q >
           "end section " (s secid) "." >)

(declaremodule "declare module " p " <: " p "{ " q " }.")
(declaremoduleinfo "declare module " (p "Name")  " <: " (p "ModType")  "{ " (p "+/- Mod1(ModArg11, ..., ModArg1N), ..., +/- ModN(ModArgN1, ..., ModArgNN)") " }.")
(declaremodulenm "declare module " p " <: " q ".")

(Pr "Pr[" p " @ " p " : " q "]")
(Prinfo "Pr[" (p "Mod.Proc(Arg1, ..., ArgN)") " @ " (p "Memory") " : " (p "Event") "]")
(Prn "Pr[" n>
      p " @ " p " :" n>
      q "]" >)
(Prm "Pr[" p " @ &m : " q "]")
(Prmr "Pr[" q " @ &m : res]")

(hoare "hoare[" p " : " p " ==> " q "]")
(hoareinfo "hoare[" (p "Mod.Proc") " : " (p "Pre") " ==> " (p "Post") "]")
(hoaren "hoare[" n>
         p " :" n>
         p n>
         "==>" n>
         q n>
         "]" >)

(phoare "phoare[" p " : " p " ==> " p "] = " q)
(phoareinfo "phoare[" (p "Mod.Proc") " : " (p "Pre") " ==> " (p "Post") "] = " (p "Prob"))
(phoaren "phoare[" n>
          p " :" n>
          p n>
          "==>" n>
          p n>
          "] = " q >)
(phoare1 "phoare[" p " : " p " ==> " q "] = 1%r")
(phoare1n "phoare[" n>
           p " :" n>
           p n>
           "==>" n>
           q n>
           "] = 1%r" >)

(equiv "equiv[" p " ~ " p " : " p " ==> " q "]")
(equivinfo "equiv[" (p "Mod1.Proc1") " ~ " (p "Mod2.Proc2") " : " (p "Pre") " ==> " (p "Post") "]")
(equivn "equiv[" n>
        p " ~ " p ":" n>
        p n>
        "==>" n>
        q n>
        "]" >)

(axiom "axiom " p " : " q ".")
(axiominfo "axiom " (p "Name") " " (p "(Param1 : Type1) ... (ParamN : TypeN)") " : " (p "Statement") ".")
(axiomn "axiom " p " :" n>
        q "." >)

(proof "proof." > n>
       q n>
       "qed." >)
(proofinfo "proof." > n>
        (p "Proof") n>
        "qed." >)
(proofpq "proof." q "qed." >)

(lemma "lemma " p " : " p "." n>
       (i proof))
(lemmainfo "lemma " (p "Name") " " (p "(Param1 : Type1) ... (ParamN : TypeN)") " : " (p "Statement") "." n>
        (i proofi))
(lemmaby "lemma " p " : " p " by " q ".")
(lemmapq "lemma " p " : " p "." n>
         (i proofpq) >)
(lemman "lemma " p " :" n>
        p "." n>
        (i proof))
(lemmanpq "lemma " p " :" n>
          p "." n>
          (i proofpq))

(lemmall "lemma " p " : islossless " p "." n>
         (i proof))
(lemmallinfo "lemma " (p "Name") " : islossless " (p "Mod.Proc") "." n>
          (i proofi))
(lemmallpq "lemma " p " : islossless " p "." n>
           (i proofpq))

(hoarelemma "hoare " p " : " p " : " p " ==> " p "." n>
             (i proof))
(hoarelemmainfo "hoare " (p "Name") " : " (p "Mod.Proc") " : " (p "Pre") " ==> " (p "Post") "." n>
                (i proofinfo))
(hoarelemmapq "hoare " p " : " p " : " p " ==> " p "." n>
              (i proofpq))
(hoarelemman "hoare " p " :" n>
             p " : " p " ==> " p "." n>
             (i proof))
(hoarenlemman "hoare " p " :" n>
              p " :" n>
              p n>
              "==>" n>
              p "." n>
              (i proof))

(equivlemma "equiv Eqv_" p " : " p " ~ " p " : " p " ==> "p "." n>
            (i proof))
(equivlemmainfo "equiv " (p "Name") " : " (p "Mod1.Proc1") " ~ " (p "Mod2.Proc2") " : " (p "Pre") " ==> " (p "Post") "." n>
                (i proofi))
(equivlemmapq "equiv Eqv_" p " : " p " ~ " p " : " p " ==> "p "." n>
              (i proofpq))
(equivlemman "equiv Eqv_" p " :" n>
             p " ~ " p " : " p " ==> "p "." n>
             (i proof))
(equivnlemman "equiv Eqv_" p " :" n>
               p " ~ " p " :" n>
               p n>
               "==>" n>
               p "." n>
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
(ifthenelse "if " p " then " p " else " q)
(ifthenelsen "if " p n>
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
(abvi (i abbrevinfo))
(abvn (i abbrevn))
(th (i theory))
(thi (i theoryinfo))
(absth (i abstractheory))
(sec (i section))
(seci (i sectioninfo))
(secid (i sectionid))
(decm (i declaremodule))
(decmi (i declaremoduleinfo))
(decmnm (i declaremodulenm))

(hoa (i hoare))
(hoai (i hoareinfo))
(hoan (i hoaren))
(ph (i phoare))
(phi (i phoareinfo))
(phn (i phoaren))
(ph1 (i phoare1))
(ph1n (i phoare1n))
(eqv (i equiv))
(eqvi (i equivinfo))
(eqvn (i equivn))

(ax (i axiom))
(axi (i axiominfo))
(axn (i axiomn))
(lem (i lemma))
(lemi (i lemmainfo))
(lemby (i lemmaby))
(lempq (i lemmapq))
(lemn (i lemman))

(lemll (i lemmall))
(lemlli (i lemmallinfo))
(lemllpq (i lemmallpq))

(hoalem (i hoarelemma))
(hoalemi (i hoarelemmainfo))
(hoalempq (i hoarelemmapq))
(hoalemn (i hoarelemman))
(hoanlemn (i hoarenlemman))
(eqvlem (i equivlemma))
(eqvlemi (i equivlemmainfo))
(eqvlempq (i equivlemmapq))
(eqvlemn (i equivlemman))
(eqvnlemn (i equivlemman))

(ifte (i ifthenelse))
(iften (i ifthenelsen))

;; Imperative specification
(procsig "proc " p "(" p ") : " q)
(procsiginfo "proc " (p "Name") "(" (p "Param1 : Type1, ..., ParamN : TypeN") ") : " (p "Type") " { " (p "ModParam1.Proc11, ... ModParam1.Proc1N, ..., ModParamN.ProcNN") " }")
(procsiglim "proc " p "(" p ") : " q " { " q " }")

(var "var " p " : " q)
(varinfo "var " (p "Name") " : " (p "Type"))
(varp (i var) ";")
(varpinfo (i varinfo) ";")

(proc "proc " p "(" p ") : " p " = {" n>
      q n>
      "}" >)
(procinfo "proc " (p "Name") "(" (p "Param1 : Type1, ..., ParamN : TypeN") ") : " (p "Type") " = {" n>
          (i varpinfo) n>
          (p "Body") n>
          "}" >)
(procnt "proc " p "(" p ") = {" n>
        q n>
        "}" >)
(procl "proc " p "(" p ") : " p " = { " q " }")
(proclnt "proc " p "(" p ") = { " q " }")
(procalias "proc " p " = " q)
(procaliasnfo "proc " (p "Name") " = " (p "Mod.Proc"))

(moduletype "module type " p " = {" n>
            q n>
            "}." >)
(moduletypep "module type " p " (" p ") = {" n>
             q n>
             "}." >)
(moduletypeinfo "module type " (p "Name") " " (p "(ModParam1 : ModType1) ... (ModParamN : ModTypeN)") " = {" n>
                (i procsiginfo) n>
                "}." >)

(module "module " p " = {" n>
        q n>
        "}." >)
(moduleinfo "module " (p "Name") " "  (p "(ModParam1 : ModType1) ... (ModParamN : ModTypeN)") " : " (p "ModType") " = {" n>
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

(modulealias "module " p " = " p "(" q ").")
(modulealiasinfo "module " (p "Name") " = " (p "Mod(ModArg1, ..., ModArgN)") ".")
(modulenpalias "module " p " = " q ".")

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

(whiles "while (" p ") {" n>
        q
        "}" >)

(assign p "<-" q ";")
(calls p "<@" q ";")
(sample p "<$" q ";")

;;; Aliases
(procs (i procsig))
(prcosi (i procsiginfo))
(prcosl (i procsiglim))

(proci (i procinfo))

(proca (i procalias))
(procai (i procaliasinfo))

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
(modnpa (i modulenpalias))

(vari (i varinfo))
(varpi (i varpinfo))

(ife (i ifelse))
(ifee (i ifelifelse))

(whis (i whiles))

(assn (i assign))
(sam (i sample))


;; Tactics
;;; Ambient
(smt "smt(" q ")")
(smtinfo "smt(" (p "Lemma1 or @Theory1, ..., LemmaN or @TheoryN") ")")

(movein "move=> " q)
(movegen "move: " q)

(pose "pose " p " := " q)
(poseinfo "pose " (p "Name") " := " (p "Expression"))

(have "have " p ": " q)
(haveinfo "have " (p "IntroPattern") ": " (p "Statement"))
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
(movi (i movein))
(movg (i movegen))

(appin (i applyin))

(rewd (i rewrited))
(rewr (i rewriter))
(rewl (i rewritel))
(rewin (i rewritein))


;;; Program
(proct "proc (" p ")")
(proctinfo "proc (" (p "Invariant") ")")

(proctub "proc (" p ") (" p ")")
(proctubinfo "proc (" (p "BadEvent") ") (" (p "InvariantUptoBad") ")")

(proctube "proc (" p ") (" p ") (" p ")")
(proctubeinfo "proc (" (p "BadEvent") ") (" (p "InvariantUptoBad") ") (" (p "InvariantAfterBad") ")")

(seq "seq " p " " p " :  (" q ")")
(seqinfo "seq " (p "LineNumCutLeft") " " (p "LineNumCutRight") " :  (" (p "ConditionAtCut") ")")

(seqhoa "seq " p " :  (" q ")")
(seqhoainfo "seq " (p "LineNumCut") " :  (" (p "ConditionAtCut") ")")

(seqph "seq " p " : (" p ") (" p ") (" p ") (" p ") (" p ") (" q ")")
(seqphinfo "seq " (p "LineNumCut") " : (" (p "Event") ") (" (p "ProbEventTrueAtCut") ") (" (p "ProbPostTrueWhenEventTrue") ") (" (p "ProbEventFalseAtCut") ") (" (p "ProbPostTrueWhenEventFalse") ") (" (p "InvariantAtCut") ")")

(sp1 "sp{1} " q)
(sp2 "sp{2} " q)
(spinfo "sp " (p "LineNumLeft") " " (p "LineNumRight"))

(wp1 "wp{1} " q)
(wp2 "wp{2} " q)
(wpinfo "wp " (p "LineNumLeft") " " (p "LineNumRight"))

(rnd1 "rnd{1} " q)
(rnd2 "rnd{2} " q)
(rndinfo "rnd " (p "Function") " " (p "InverseFunction"))
(rndinvinfo "rnd " (p "InvolutiveFunction"))

(if1 "if{1}")
(if2 "if{2}")

(while "while (" q ")")
(whileinfo "while " (p "Invariant"))

(whileph "while (" p ") (" p ") (" p ") (" q ")")
(whilephinfo "while " (p "Invariant") (p "TerminationVariant") (p "UpperBoundTermVar") (p "ProbTermVarDecrease")) ; TODO: ask PY if correct

(while1 "while{1} (" p ") (" q ")")
(while2 "while{1} (" p ") (" q ")")
(while1info "while{1} " (p "Invariant") (p "TerminationVariant")) ; TODO: ask PY if this is correct
(while2info "while{2} " (p "Invariant") (p "TerminationVariant"))

(callcont "call (_: " p " ==> " q ")")
(call1cont "call{1} (_: " p " ==> " q ")")
(call2cont "call{2} (_: " p " ==> " q ")")
(callcontinfo "call (_: " (p "Pre") " ==> " (p "Post") ")")

(callpt "call (" q ")")
(call1pt "call{1} (" q ")")
(call2pt "call{2} (" q ")")
(callptinfo "call (" (p "Proofterm") ")")

(callinv "call (_:" q ")")
(callinvinfo "call (_:" (p "Invariant") ")")

(callub "call (_: " p ", " q ")")
(callubinfo "call (_: " (p "BadEvent") ", " (p "InvariantUptoBad") ")")

(callube "call (_: " p ", " p ", " q ")")
(callubeinfo "call (_: " (p "BadEvent") ", " (p "InvariantUptoBad") ", " (p "InvariantAfterBad") ")")

(swapbl "swap [" p ".." p "] " q)
(swap1bl "swap{1} [" p ".." p "] " q)
(swap2bl "swap{2} [" p ".." p "] " q)
(swapblinfo "swap [" (p "LineNumStart") ".." (p "LineNumEnd") "] " (p "(-)MoveAmount"))

(swap1l "swap{1} " p " " q )
(swap2l "swap{2} " p " " q )
(swaplinfo "swap " (p "LineNum") " " (p "(-)MoveAmount"))

(swap1m "swap{1} " q)
(swap2m "swap{2} " q)
(swapminfo "swap " (p "(-)MoveAmount"))

(swap1s "swap{1} " p " " p " " q)
(swap2s "swap{2} " p " " p " " q)
(swapsinfo "swap " (p "LineNumBegin") " " (p "LineNumMid") " " (p "LineNumEnd"))

(inline1 "inline{1} " q)
(inline2 "inline{2} " q)
(inlineinfo "inline " (p "Mod1.Proc1, ..., Modn.Procn or LineNumProc"))

(inlines "inline *")
(inline1s "inline{1} *")
(inline2s "inline{2} *")

(inlineo "inline (" p ") " q)
(inline1o "inline{1} (" p ") " q)
(inline2o "inline{2} (" p ") " q)
(inlineoinfo "inline (" (p "LineNumOccur1 ... LineNumOccurN") ") " (p "Mod.Proc"))

(rcondt1 "rcondt{1} " q)
(rcondt2 "rcondt{2} " q)
(rcondtinfo "rcondt " (p "LineNumIfOrWhile"))

(rcondf1 "rcondf{1} " q)
(rcondf2 "rcondf{2} " q)
(rcondfinfo "rcondf " (p "LineNumIfOrWhile"))

(unroll1 "unroll{1} " q)
(unroll2 "unroll{2} " q)
(unrollinfo "unroll " (p "LineNumWhile"))

(splitwhile "splitwhile " p " : (" q ")")
(splitwhile1 "splitwhile{1} " p " : (" q ")")
(splitwhile2 "splitwhile{2} " p " : (" q ")")
(splitwhileinfo "splitwhile " (p "LineNumWhile") " : (" (p "WhileCondition") ")")

(fission "fission " p "!" p " @ " p ", " q)
(fission1 "fission{1} " p "!" p " @ " p ", " q)
(fission2 "fission{2} " p "!" p " @ " p ", " q)
(fissioninfo "fission " (p "LineNumWhile") "!" (p "NumLinesBeforeWhile") " @ " (p "LineNumEnd1") ", " (p "LineNumEnd2"))

(fusion "fusion " p "!" p " @ " p ", " q)
(fusion1 "fusion{1} " p "!" p " @ " p ", " q)
(fusion2 "fusion{2} " p "!" p " @ " p ", " q)
(fusioninfo "fusion " (p "LineNumWhile") "!" (p "NumLinesBeforeWhile") " @ " (p "LineNumEnd1") ", " (p "LineNumEnd2"))

(alias "alias " p " with " q)
(alias1 "alias{1} " p " with " q)
(alias2 "alias{2} " p " with " q)
(aliasinfo "alias " (p "LineNumAssignment") " with " (p "VarName"))

(aliase "alias " p " " p " = " q)
(aliase1 "alias{1} " p " " p " = " q)
(aliase2 "alias{2} " p " " p " = " q)
(aliaseinfo "alias " (p "LineNumAssignment") " " (p "VarName") " = " (p "Expression"))


;; Advanced/Specific
(Prub (i Pr) n>
      "<=" n>
      (i Pr))
(Prnub (i Prn) n>
       "<=" n>
       (i Prn))
(Prmub (i prm) n>
       "<=" n>
       (i prm))
(Prmrub (i prmr) n>
        "<=" n>
        (i prmr))

(Preq (i Pr) n>
      "=" n>
      (i Pr))
(Prneq (i Prn) n>
       "=" n>
       (i Prn))
(Prmeq (i prm) n>
       "=" n>
       (i prm))
(Prmreq (i prmr) n>
        "=" n>
        (i prmr))

(Prublem "lemma " p " :" n>
         (i Prub) "." n>
         (i proof))
(Prnublem "lemma " p " :" n>
          (i Prnub) "." n>
          (i proof))
(Prmublem "lemma " p " &m :" n>
          (i Prmub) "." n>
          (i proof))
(Prmrublem "lemma " p " &m :" n>
           (i Prmrub) "." n>
           (i proof))

(Preqlem "lemma " p " :" n>
         (i Preq) "." n>
         (i proof))
(Prneqlem "lemma " p " :" n>
          (i Prneq) "." n>
          (i proof))
(Prmeqlem "lemma " p " &m :" n>
          (i Prmeq) "." n>
          (i proof))
(Prmreqlem "lemma " p " &m :" n>
           (i Prmreq) "." n>
           (i proof))
