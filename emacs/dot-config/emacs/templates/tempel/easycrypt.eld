;; easycrypt.eld
;; Tempel templates for easycrypt mode

easycrypt-mode

;; Internal
(pragma "pragma " q ".")
(pragmap "pragma +" q ".")
(pragmam "pragma -" q ".")

;; (Extra) Functionality (e.g., print, locate, search)
(locate "locate " q ".")
(print "print " q ".")
(search "search " q ".")

;; Comments and documentation
;;; Regular comments
(comment "(* " q " *)")
(commentn "(*" n>
          q n
          "*)" >)

;;;; Aliases
(com (i comment))
(com (i commentn))


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
(declare "declare " q ".")
(local "local " q ".")

;;; Aliases
(dec (i declare))
(loc (i local))


;; Functional specification
(type "type " p " = " q ".")
(typen "type " p " =" n>
       q "." >)
(typeabs "type " q ".")

(op "op " p " : " p " = " q ".")
(opn "op " p " : " p " = " n>
     q "." >)
(opabs "op " p " : " q ".")

(const "const " p " : " p " = " q ".")
(constn "const " p " : " p " = " n>
     q "." >)
(constabs "const " p " : " q ".")

(abbrev "abbrev " p " = " q ".")
(abbrevn "abbrev " p " =" n>
         q "." >)

(abstract "abstract " p ".")
(abstracttheory "abstract theory " (p "" thname)  "." n>
                q n>
                "end " (s thname) "." >)

(axiom "axiom " p " : " q ".")
(axiomn "axiom " p " :" n>
        q "." >)
(declareaxiom "declare " (i axiom))
(declareaxiomn "declare " (i axiomn))

(lemma "lemma " p " : " p "." n>
       "proof." n>
        q n>
        "qed." >)
(lemmaby "lemma " p " : " p " by " q ".")
(lemmapq "lemma " p " : " p "." n>
         "proof." q "qed.")
(lemman "lemma " p " :" n>
        q "." n>
        "proof." n>
        n>
        "qed." >)
(lemmanpq "lemma " p " :" n>
          p "." n>
          "proof." q "qed.")
(locallemma "local " (i lemma))
(locallemmaby "local " (i lemmaby))
(locallemmapq "local " (i lemmapq))
(locallemman "local " (i lemman))
(locallemmanpq "local " (i lemmanpq))

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
         p "==>" p "]")
(equivnlongs "equiv[" n>
           p " ~ " p n>
           p n>
           "==>" n>
           p "]")
(equivlongps "equiv[" n>
             p "." p n>
             "~" n>
             p "." p " :" n>
             p n>
             "==>" n>
             p "]")

(hoarelemma "hoare " p " :" n>
            p "." p " : " p " ==> " p ".")

(equivlemma "equiv " p)


;; Imperative specification


;; (pr "Pr[" (p "Mod") "." (p "Proc") "(" (p "Args") ")" " @ " (p "Mem") " : " (p "Event") "]")
;;(prdefault "Pr[" (p "Mod") "." (p "Proc") "(" (p "Args") ")" " @ &m : res]")
(pr "Pr[" p "." p "(" p ")" " @ " p " : " q "]")
(prdef "Pr[" p "." p "(" q ") @ &m : res]")

;;; Aliases
(abv (i abbrev))
(abs (i abstract))
(absth (i abstractheory))


;; Imperative specification




;; Advanced/Specific
(prub (i pr) n>
      "<=" n>
      (i pr))
(prubdef (i prdef) n>
         "<=" n>
         (i prdef))
(lemprub (i lemma)
         (i pr) n>
         "<=" n>
         (i pr))
(lemprubdef (i prdef) n>
            "<=" n>
            (i prdef))
