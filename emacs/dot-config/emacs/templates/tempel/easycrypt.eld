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
(com "(*" n>
     q n
     "*)" >)
(comline "(* " q " *)")

;;; Documentation comments
(doccom "(**" n>
        q n
        "**)" >)
(doccomline "(** " q " **)")
(doccomfile "(*^" n>
            (file-name-nondirectory (or (buffer-file-name) (buffer-name))) n
            n>
            "Author: " p n>
            "Maintainer: " p n>
            "Date: " (po (format-time-string "%Y-%m-%d")) n>
            "Description:" n>
            q n
            "^*)" >)

;; Meta
(require "require " q ".")
(import "import " q ".")
(reqimp "require import " q ".")

(clone "clone " q ".")
