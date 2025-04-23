;; prog.eld
;; Tempel templates for prog mode

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: " q comment-end
       :pre (unless (looking-at-p "[[:blank:]]*$") (end-of-line) (insert "\t")))
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: " q comment-end
      :pre (unless (looking-at-p "[[:blank:]]*$") (end-of-line) (insert "\t")))
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: " q comment-end
     :pre (unless (looking-at-p "[[:blank:]]*$") (end-of-line) (insert "\t")))
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK: " q comment-end
      :pre (unless (looking-at-p "[[:blank:]]*$") (end-of-line) (insert "\t")))
