;; prog.eld
;; Tempel templates for prog mode

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: " q comment-end)
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: " q comment-end)
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: " q comment-end)
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK: " q comment-end)

(fm (i fixme))
(td (i todo))
