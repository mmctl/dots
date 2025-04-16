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

(fromimport "from " p " import " q)

;;; Aliases
(req (i require))
(imp (i import))
(ri (i requireimport))

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

(fi (i fromimport))


;; Scope

;; Functional specification
(type "type " p " = " q ".")
(typen "type " p " =" n>
       q "." >)

(op "op " p " : " p " = " q ".")
(opn "op " p " : " p " =" n>
     q "." >)
(opabs "op " p " : " q ".")

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
           "end section " (s secid) >)

(pr "Pr[" p " @ " p " : " q "]")
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
(axiomn "axiom " p " :" n>
        q "." >)

(proof "proof." n>
       q n>
       "qed." >)
(proofl "proof." q "qed.")

(lemma "lemma " p " : " p "." n>
       (i proof))
(lemmaby "lemma " p " : " p " by " q ".")
(lemmapq "lemma " p " : " p "." n>
         (i proofl) >)
(lemman "lemma " p " :" n>
        p "." n>
        (i proof))
(lemmanpq "lemma " p " :" n>
          p "." n>
          (i proofl))

(hoarelemma "hoare " p " : " p " : " p " ==> " p "." n>
            (i proof))
(hoarelemmapq "hoare " p " : " p " : " p " ==> " p "." n>
              (i proofl))
(hoarelemman "hoare " p " :" n>
             p " : " p " ==> " q "." n>
             (i proof))
(hoarenlemman "hoare " p " :" n>
              p " :" n>
              p " ==> " q "." n>
              (i proof))
(hoarenslemman "hoare " p " :" n>
               p " :" n>
               p n>
               "==>" n>
               q "." n>
               (i proof))

(equivlemma "equiv " p " : " p " ~ " p " : " p " ==> " q "." n>
            (i proof))
(equivlemmapq "equiv " p " : " p " ~ " p " : " p " ==> " q "." n>
              (i proofl))
(equivlemman "equiv " p " :" n>
             p " ~ " p " : " p " ==> " q "." n>
             (i proof))
(equivnlemman "equiv " p " :" n>
              p " ~ " p " :" n>
              p " ==> " q "." n>
              (i proof))
(equivnslemman "equiv " p " :" n>
               p " ~ " p " :" n>
               p n>
               "==>" n>
               q "." n>
               (i proof))
(equivnsplemman "equiv " p " :" n>
                p n>
                "~" n>
                p " :" n>
                p n>
                "==>" n>
                q "."
                (i proof))

;;; Aliases
(abv (i abbrev))
(absth (i abstractheory))


;; Imperative specification
(procsig "proc " p " : " q)
(procsiglim "proc " p " : " q " { " q " }")

(proc "proc " p " : " p " = {" n>
      q n>
      "}" >)
(procnt "proc " p " = {" n>
        q n>
        "}" >)
(procl "proc " p " : " p " = { " q " }")
(proclnt "proc " p " = { " q " }")

(moduletype "module type " p " = {" n>
            q
            "}." >)
(moduletypep "module type " p " (" p ") = {" n>
             q
             "}." >)

;; (moduletype1 "module type " p " = {" n>
;;              (i procsig) n>
;;              "}." >)
;; (moduletype2 "module type " p " = {" n>
;;              (i procsig) n>
;;              (i procsig) n>
;;              "}." >)
;; (moduletype3 "module type " p " = {" n>
;;              (i procsig) n>
;;              (i procsig) n>
;;              (i procsig) n>
;;              "}." >)

(module "module " p " = {" n>
        q
        "}." >)
(modulep "module " p "(" p ") = {" n>
         q
         "}." >)

;; (module1 "module " p " = {" n>
;;            (i proc) n>
;;            "}." >)
;; (module2 "module " p " = {" n>
;;            (i proc) n
;;            n>
;;            (i proc) n
;;            "}." >)
;; (module3 "module " p " = {" n>
;;            (i proc) n
;;            n>
;;            (i proc) n
;;            n>
;;            (i proc) n
;;            "}." >)


(modulet "module " p " : " p " = {" n>
         q
         "}." >)

(modulept "module " p " (" p ") : " p " = {" n>
          q
          "}." >)


;; (modulet1 "module " p " : " p " = {" n>
;;          (i proc) n>
;;          "}." >)
;; (modulet2 "module " p " : " p " = {" n>
;;          (i proc) n>
;;          (i proc) n>
;;          "}." >)
;; (modulet3 "module " p " : " p " = {" n>
;;          (i proc) n>
;;          (i proc) n>
;;          (i proc) n>
;;          "}." >)

;; Tactics

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
