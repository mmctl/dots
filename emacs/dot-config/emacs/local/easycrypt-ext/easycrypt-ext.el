;; -*- lexical-binding: t -*-
;; easycrypt-ext.el
(require 'easycrypt-ext-consts)


;; Customizations
(defgroup easycrypt-ext nil
  "Customization group for additional EasyCrypt functionality."
  :group 'easycrypt)

(defcustom ece-enable-indentation t
  "Non-nil (resp. `nil`) to enable (resp. disable) enhanced
(but still ad-hoc) indentation in EasyCrypt."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-indentation-keybindings t
  "Non-nil (resp. `nil`) to enable (resp. disable) suggested keybindings
for indentation-related commands in EasyCrypt."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-keywords-completion t
  "Non-nil (resp. `nil`) to enable (resp. disable) completion for
EasyCrypt keywords (depends on `cape`)."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-templates t
  "Non-nil (resp. `nil`) to enable (resp. disable) code templates for
EasyCrypt (depends on `tempel`). If you enable this, it is recommended to
also enable enhanced indentation (see `ece-enable-indentation`),
since the templates use indentation and were made with the enhanced
EasyCrypt indentation in mind."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-templates-keybindings 'ece-enable-templates
  "Non-nil (resp. `nil`) to enable (resp. disable) keybindings for inserting
EasyCrypt (regular) templates (depends on `tempel`). Does not make much sense
to enable this when you have disabled templates themselves (see `ece-enable-templates`)."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-templates-info 'ece-enable-templates
  "Non-nil (resp. `nil`) to enable (resp. disable) informative code templates
for EasyCrypt (depends on `tempel`). If you enable this, it is recommended to also
enable enhanced indentation (see `ece-enable-indentation`), since the
templates use indentation and were made with the enhanced EasyCrypt indentation in mind."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-auxiliary-functionality-keybindings t
  "Non-nil (resp. `nil`) to enable (resp. disable) keybindings for additional
EasyCrypt functionality (e.g., printing/searching with mouse click)."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-theme nil
  "`'dark`, `'light`, or `nil` to enable the dark mode EasyCrypt theme,
light EasyCrypt theme, or no EasyCrypt theme, respectively (depends on `doom-themes`).
Can be 'dark, 'light, or nil."
  :type '(choice
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "No theme" nil))
  :group 'easycrypt-ext)


;; Constants
(defconst ece--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where this file is stored (and so also where rest of package should be).")


;; Indentation
;;; Basic rigid indentation
;;;###autoload
(defun ece-basic-indent (arg)
  "Indent all lines touched by the active region by ARG * `tab-width`.
If no region is active, insert ARG tabs at point or un-tab current line (ARG times)."
  (interactive "p")
  ;; If region is active,...
  (if (use-region-p)
      ;; Then, take stock of current region, point, and mark positions
      ;; and compute new region positions
      (let* ((orig-region-start (region-beginning))
             (orig-region-stop (region-end))
             (new-region-start (save-excursion (goto-char orig-region-start)
                                               (line-beginning-position)))
             (new-region-stop (save-excursion (goto-char orig-region-stop)
                                              (if (= orig-region-stop (line-beginning-position))
                                                  orig-region-stop
                                                (line-end-position))))
             (region-forward (<= orig-region-start orig-region-stop)))
        ;; Expand visual region to new region positions
        (if region-forward
            (progn (goto-char new-region-stop) (set-mark new-region-start))
          (progn (goto-char new-region-start) (set-mark new-region-stop)))
        ;; Indent visually expanded region
        (indent-rigidly new-region-start new-region-stop (* arg tab-width))
        ;; Don't deactivate-mark, so we don't have to re-select region to repeat
        (setq-local deactivate-mark nil))
    ;; Else (no region is active), if prefix argument is negative...
    (if (< arg 0)
        ;; Then, take stock of whitespace behind point, and
        (let* ((orig-point (point))
               (del-ub (min (* (abs arg) tab-width) (- orig-point (line-beginning-position))))
               (del (save-excursion
                      (skip-chars-backward "[:space:]" (- orig-point del-ub))
                      (- orig-point (point)))))
          ;; if there is at least some whitespace...
          (if (< 0 del)
              ;; Then, delete ARG * tab-width of white-space
              ;; (at most until first non-whitespace character)
              (delete-region (- orig-point del) orig-point)
            ;; Else, un-tab current line (at most) ARG times
            (indent-rigidly (line-beginning-position)
                            (line-end-position)
                            (* arg tab-width))))
      ;; Else, insert ARG tabs at point
      (insert-tab arg))))

;;;###autoload
(defun ece-basic-deindent (arg)
  "De-indent all lines touched by the active region by ARG * `tab-width`.
If no region is active, un-tab current line by ARG * `tab-width`."
  (interactive "p")
  (ece-basic-indent (- arg)))

;;; Contextual indentation
(defun ece--indent-level-fallback ()
  "Returns fallback indentation level for EasyCrypt code
(i.e., when no 'special indentation' case is detected)."
  (save-excursion
    ;; Find previous non-blank line
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:blank:]]*$"))
      (forward-line -1))
    (setq prev-line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
    (back-to-indentation)
    ;; If previous non-blank line is an unfinished non-proof/non-proc spec
    ;; (E.g., starts with `lemma` but does not end with a `.`)
    (if (and (seq-some (lambda (kw)
                         (string-match-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b") prev-line))
                       ece-start-keywords)
             (not (string-match-p "\\.[[:blank:]]*$" prev-line)))
        ;; Then, align with previous non-blank line + tab
        (+ (current-indentation) tab-width)
      ;; Else, align with previous non-blank line (default)
      (current-indentation))))

(defun ece--indent-level ()
  "Returns desired indentation level of EasyCrypt code."
  (let ((indent-level 0))
    ;; Compute desired level of indentation
    (save-excursion
      (back-to-indentation) ; Base decision on beginning of line
      (let* ((synps (syntax-ppss))
             (exprop (nth 1 synps))
             (instr (nth 3 synps))
             (incom (nth 4 synps))
             (csop (nth 8 synps)))
        (cond
         ;; If we are in a comment or string...
         ((or incom instr)
          (let ((comcl (looking-at-p "[\\^\\*]?\\*)"))
                (strcl (looking-at-p "\"")))
            (goto-char csop)
            ;; Then, if our line closes the comment or string...
            (if (or (and incom comcl) (and instr strcl))
                ;; Then, align closer with the opener
                (setq indent-level (current-column))
              ;; Else, align with the opener + tab
              (setq indent-level (+ (current-column) tab-width)))))
         ;; Else, if we are in an opened or enclosed expression...
         ;; (E.g., between { and }, or ( and ), or after { or ( with no closing counterpart)
         (exprop
          (let ((brcl (looking-at "}" t))
                (btcl (looking-at "]" t))
                (prcl (looking-at ")" t)))
            (goto-char exprop)
            ;; If opener is {...
            (if (looking-at-p "{")
                ;; Then, if first char on our line is }...
                (if brcl
                    ;; Then, align to indentation of opener
                    (setq indent-level (current-indentation))
                  ;; Else, align to indentation of opener + tab
                  (setq indent-level (+ (current-indentation) tab-width)))
              ;; Else, if first char on our line is ] or )
              ;; and current expression is opened by [ or (...
              (if (or (and btcl (looking-at-p "[[]"))
                      (and prcl (looking-at-p "(")))
                  ;; Then, align to indentation to opener
                  (setq indent-level (current-column))
                ;; Else, align to indentation of opener + tab
                (setq indent-level (+ (current-column) tab-width))))))
         ;; Else, if we are looking at a proof starter (i.e., "proof")
         ((seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                    ece-proof-start-keywords)
          (let ((bob nil))
            (progn
              (save-excursion
                ;; Find previous "proof spec starter" (i.e., keyword starting a
                ;; lemma such as lemma, hoare (without following [), and
                ;; equiv (without following [)
                (forward-line -1)
                (while (and (not (bobp))
                            (not (seq-some
                                  (lambda (kw)
                                    (and (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b"))
                                         (not (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b[[:space:]]*\\[")))))
                                  ece-proof-spec-keywords)))
                  (forward-line -1))
                ;; If we didn't find such a line (i.e., we are at the beginning of a buffer)...
                (if (bobp)
                    ;; Then, record this fact
                    (setq bob t)
                  ;; Else, align to indentation of proof spec starter
                  (setq indent-level (current-indentation))))
              ;; If we didn't find a proof spec starter
              (when bob
                ;; Indent as per the fallback
                (setq indent-level (ece--indent-level-fallback))))))
         ;; Else, if we are looking at a proof ender (i.e., "qed")
         ((seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                    ece-proof-end-keywords)
          (let ((bob nil))
            (progn
              (save-excursion
                ;; Find line that started the proof (i.e., one that starts with "proof")
                (forward-line -1)
                (while (and (not (bobp))
                            (not (seq-some (lambda (kw) (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b")))
                                           ece-proof-start-keywords)))
                  (forward-line -1))
                ;; If we didn't find such a line (i.e., we are at the beginning of a buffer)...
                (if (bobp)
                    ;; Then, record this fact
                    (setq bob t)
                  ;; Else, align to indentation of proof starter
                  (setq indent-level (current-indentation))))
              ;; If we didn't find a proof starter
              (when bob
                ;; Indent as per the fallback
                (setq indent-level (ece--indent-level-fallback))))))
         ;; Else,...
         (t
          ;; Indent as per the fallback
          (setq indent-level (ece--indent-level-fallback))))))
    indent-level))

;;;###autoload
(defun ece-indent-line ()
  "Indents line of EasyCrypt code."
  (interactive)
  ;; Indent accordingly
  (let ((indent-level (ece--indent-level)))
    ;; `indent-line-to`would move point to new indentation, and
    ;; we prevent this by `save-excursion` so point position remains consistent
    ;; (making templates more consistent as well)
    (save-excursion
      (indent-line-to indent-level))
    ;; Still adjust point if it's inside indentation
    (when (< (current-column) (current-indentation))
      (move-to-column indent-level))))

;;;###autoload
(defun ece-indent-on-insertion-closer ()
  "Indent when last input was one of }, ), ], \" and it is the
first character on current line, or if the last input was . and
the current line starts/ends a proof. However, only
allow de-indents (to prevent automatically indenting
code that has been manually de-indented; this is a hack
and a limitation of the localized ad-hoc computation
of the indent level).
Meant for `post-self-insert-hook`."
  (when (or (and (memq last-command-event '(?\} ?\) ?\] ?\"))
                 (let ((line-before (buffer-substring-no-properties (line-beginning-position)
                                                                    (- (point) 1))))
                   (string-match-p "^[[:blank:]]*$" line-before)))
            (and (eq last-command-event ?\.)
                 (save-excursion
                   (back-to-indentation)
                   (seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                             ece-proof-delimit-keywords))))
    (let* ((orig-col (current-column))
           (indent-level (ece--indent-level))
           (indent-diff (- (current-indentation) indent-level)))
      ;; If 0 < indent-diff, i.e., we are de-indenting
      (when (< 0 indent-diff)
        ;; Go to the computed indent level
        (indent-line-to indent-level)
        ;; Keep point in same relative position
        ;; (`indent-line-to` moves it to end of indentation)
        (move-to-column (- orig-col indent-diff))))))


;; Shell commands
(defun ece--validate-shell-command (command)
  "Checks if the provided `command` is a valid/supported EasyCrypt shell command
(in the sense that a command below is implemented for it).
Prints a message informing the user if that is not the case."
  (or (member command '("print" "search" "locate"))
      (user-error "Unknown/Unsupported command: `%s`." command)))

(defun ece--exec-shell-command (command &rest args)
  "Combines `command` and `args` into a command for the EasyCrypt shell, and
directly calls the shell with it."
  (if (and (fboundp 'proof-shell-ready-prover)
           (fboundp 'proof-shell-invisible-command))
      (progn
        (proof-shell-ready-prover)
        (if args
            (proof-shell-invisible-command (concat command " " (string-join args " ")))
          (proof-shell-invisible-command command)))
    (user-error "Necessary functionality for executing proof commands not found. Did you load Proof General?")))

(defun ece--thing-at (event)
  "If `event` is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point."
  (if (mouse-event-p event)
      (or (thing-at-mouse event 'sexp t)
          (thing-at-mouse event 'word t)
          (thing-at-mouse event 'symbol t))
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'sexp t)
          (thing-at-point 'word t)
          (thing-at-point 'symbol t)))))

(defun ece--command (command event)
  "If `event` is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. The result is used as an
argument to the `command` command of EasyCrypt. If nothing (reasonable) is
found, or the provided `command` is not valid, prints a message informing
the user (see `ece--validate-shell-command`)."
  (when (ece--validate-shell-command command)
    (let ((arg (ece--thing-at event)))
      (if arg
          (ece--exec-shell-command command arg)
        (user-error "No reasonable thing at %s found for command `%s`.%s"
                    (if (mouse-event-p event) "mouse" "point")
                    command
                    (if (mouse-event-p event) "" " Try selecting the thing if automatic detection doesn't work."))))))

;;;###autoload
(defun ece-print (&optional event)
  "If called with a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `print` command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "print" event))

;;;###autoload
(defun ece-search (&optional event)
  "If called with a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `search` command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "search" event))

;;;###autoload
(defun ece-locate (&optional event)
  "If called with a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `locate` command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "locate" event))


;; Key maps
(defvar-keymap ece-proof-mode-process-repeat-map
  :doc "Keymap (repeatable) for processing proof commands"
  :repeat (:hints ((proof-undo-last-successful-command . "p: Undo last succesful command")
                   (proof-assert-next-command-interactive . "n: Assert next command")
                   (proof-undo-and-delete-last-successful-command . "d: Undo and delete last successful command")))
  "p" #'proof-undo-last-successful-command
  "n" #'proof-assert-next-command-interactive
  "d" #'proof-undo-and-delete-last-successful-command)

(defvar-keymap ece-bufhist-repeat-map
  :doc "Keymap (repeatable) for browsing and managing buffer history"
  :repeat (:hints ((bufhist-prev . "p: Go to previous history element")
                   (bufhist-next . "n: Go to next history element")
                   (bufhist-first . "f: Go to first history element")
                   (bufhist-last . "l: Go to last history element")
                   (bufhist-delete . "d: Delete current history element")))
  "p" #'bufhist-prev
  "n" #'bufhist-next
  "f" #'bufhist-first
  "l" #'bufhist-last
  "d" #'bufhist-delete)

(defvar-keymap ece-template-map
  :doc "Keymap for EasyCrypt templates"
  :prefix 'ece-template-map-prefix)


;; Setup
;;; Indentation
(defun ece--setup-indentation ()
  (when ece-enable-indentation
    (setq-local electric-indent-mode nil)
    (setq-local electric-indent-inhibit t)
    (setq-local tab-width 2)
    (setq-local indent-line-function #'ece-indent-line)
    (add-hook 'post-self-insert-hook #'ece-indent-on-insertion-closer nil t)))

(defun ece--setup-indentation-keybindings ()
  (when ece-enable-indentation-keybindings
    (keymap-local-set "RET" #'newline-and-indent)
    (keymap-local-set "<return>" "RET")
    (keymap-local-set "S-<return>" #'newline)
    (keymap-local-set "TAB" #'ece-basic-indent)
    (keymap-local-set "<tab>" "TAB")
    (keymap-local-set "<backtab>" #'ece-basic-deindent)
    (keymap-local-set "M-i" #'indent-for-tab-command)))

;;; Auxiliary functionality
(defun ece--setup-auxiliary-functionality-keybindings ()
  (when ece-enable-auxiliary-functionality-keybindings
    (keymap-set easycrypt-mode-map "C-c C-y p" #'ece-print)
    (keymap-set easycrypt-mode-map "C-c C-y l" #'ece-locate)
    (keymap-set easycrypt-mode-map "C-c C-y s" #'ece-search)
    (keymap-set easycrypt-mode-map "S-<down-mouse-3>" #'ece-print)
    (keymap-set easycrypt-mode-map "C-<down-mouse-3>" #'ece-locate)
    (keymap-set easycrypt-mode-map "M-<down-mouse-3>" #'ece-search)
    (keymap-set easycrypt-response-mode-map "C-c C-y p" #'ece-print)
    (keymap-set easycrypt-response-mode-map "C-c C-y l" #'ece-locate)
    (keymap-set easycrypt-response-mode-map "C-c C-y s" #'ece-search)
    (keymap-set easycrypt-response-mode-map "S-<down-mouse-3>" #'ece-print)
    (keymap-set easycrypt-response-mode-map "C-<down-mouse-3>" #'ece-locate)
    (keymap-set easycrypt-response-mode-map "M-<down-mouse-3>" #'ece-search-at-mouse)
    (keymap-set easycrypt-goals-mode-map "C-c C-y p" #'ece-print-at-point)
    (keymap-set easycrypt-goals-mode-map "C-c C-y l" #'ece-locate-at-point)
    (keymap-set easycrypt-goals-mode-map "C-c C-y s" #'ece-search-at-point)
    (keymap-set easycrypt-goals-mode-map "S-<down-mouse-3>" #'ece-print-at-mouse)
    (keymap-set easycrypt-goals-mode-map "C-<down-mouse-3>" #'ece-locate-at-mouse)
    (keymap-set easycrypt-goals-mode-map "M-<down-mouse-3>" #'ece-search-at-mouse)))

;;; Keywords
(defun ece--setup-keywords-completion ()
  (when ece-enable-keywords-completion
    (with-eval-after-load 'cape-keyword
      (add-to-list 'cape-keyword-list (cons 'easycrypt-mode ece-keywords)))))

;;; Templates
(defun ece--tempel-placeholder-form-as-lit (elt)
"Define slight adjustment of regular placeholder element
so that a prompt form evaluating to a string is inserted as
default value in the same way as a literal string prompt."
  (pcase elt
    (`(pfl ,prompt . ,rest)
     (let ((evprompt (eval prompt)))
       (if (stringp evprompt)
           `(p ,evprompt ,@rest)
         `('p ,prompt ,@rest))))))

(defun ece--tempel-include (elt)
  "Define 'include' element (taken and slightly adjusted from TempEL github repo)
that allows to include other templates by their name."
  (when (eq (car-safe elt) 'i)
    (when-let (template (alist-get (cadr elt) (tempel--templates)))
      (cons 'l template))))


(defun ece--tempel-template-file-read (file)
  (let ((res '()))
    (dolist (metatemps (tempel--file-read file))
      (let ((modes (car metatemps))
            (plist (cadr metatemps))
            (temps (cddr metatemps)))
        (when (tempel--condition-p modes plist)
          (setq res (append res temps)))))
    res))

(defun ece--setup-templates ()
  (when ece-enable-templates
    (with-eval-after-load 'tempel
      (add-to-list 'tempel-user-elements #'ece--tempel-placeholder-form-as-lit)
      (add-to-list 'tempel-user-elements #'ece--tempel-include)
      (let ((ece-temp (file-name-concat ece--dir "easycrypt-ext-templates.eld")))
        (add-to-list 'tempel-template-sources
                     #'(lambda () (ece--tempel-template-file-read ece-temp)))))))

(defun ece--setup-templates-info ()
  (when ece-enable-templates-info
    (with-eval-after-load 'tempel
      (let ((ece-temp (file-name-concat ece--dir "easycrypt-ext-templates-info.eld")))
        (add-to-list 'tempel-template-sources
                     #'(lambda () (ece--tempel-template-file-read ece-temp)))))))

(defun ece--setup-templates-keybindings ()
  (when ece-enable-templates-keybindings
    (with-eval-after-load 'tempel
      (tempel-key "a" axiomn ece-template-map)
      (tempel-key "A" abbrevn ece-template-map)
      (tempel-key "b" byequiv ece-template-map)
      (tempel-key "B" byphoare ece-template-map)
      (tempel-key "c" conseq ece-template-map)
      (tempel-key "C" conseqehh ece-template-map)
      (tempel-key "d" doccommentn ece-template-map)
      (tempel-key "D" declaremodule ece-template-map)
      (tempel-key "e" equivn ece-template-map)
      (tempel-key "E" equivnlemman ece-template-map)
      (tempel-key "f" funn ece-template-map)
      (tempel-key "F" fel ece-template-map)
      (tempel-key "g" ge0 ece-template-map)
      (tempel-key "G" gt0 ece-template-map)
      (tempel-key "h" hoaren ece-template-map)
      (tempel-key "H" hoarenlemman ece-template-map)
      (tempel-key "i" ifelse ece-template-map)
      (tempel-key "I" ifthenelse ece-template-map)
      (tempel-key "l" lemman ece-template-map)
      (tempel-key "L" letinn ece-template-map)
      (tempel-key "m" module ece-template-map)
      (tempel-key "M" modulept ece-template-map)
      (tempel-key "o" op ece-template-map)
      (tempel-key "O" opas ece-template-map)
      (tempel-key "p" proc ece-template-map)
      (tempel-key "P" procsig ece-template-map)
      (tempel-key "r" rewrited ece-template-map)
      (tempel-key "R" rngin ece-template-map)
      (tempel-key "s" seq ece-template-map)
      (tempel-key "S" seqph ece-template-map)
      (tempel-key "t" moduletype ece-template-map)
      (tempel-key "T" moduletypep ece-template-map)
      (tempel-key "u" Prmub ece-template-map)
      (tempel-key "U" Prmrub ece-template-map)
      (tempel-key "v" Prmeq ece-template-map)
      (tempel-key "V" Prmreq ece-template-map)
      (tempel-key "w" whiles ece-template-map)
      (tempel-key "W" whileph ece-template-map)
      (tempel-key "x" cloneimportaswith ece-template-map)
      (tempel-key "X" requireimport ece-template-map)
      (tempel-key "y" phoaren ece-template-map)
      (tempel-key "Y" phoare1n ece-template-map)
      (tempel-key "z" theory ece-template-map)
      (tempel-key "Z" abstracttheory ece-template-map))
    (keymap-set easycrypt-mode-map "C-c C-y t" 'ece-template-map-prefix)))

;; Themes
(defun ece--setup-theme ()
  (cond
   ((eq ece-enable-theme 'dark)
    (with-eval-after-load 'doom-themes
      (setopt doom-nord-brighter-modeline t
              doom-nord-brighter-comments nil
              doom-nord-comment-bg nil
              doom-nord-region-highlight 'snowstorm)
      (load-theme 'doom-nord t)
      (doom-themes-set-faces 'doom-nord
        '(trailing-whitespace :background magenta)
        '(aw-background-face :inherit 'avy-background-face)
        '(aw-leading-char-face :inherit 'avy-lead-face)
        '(proof-queue-face :background magenta)
        '(proof-locked-face :background base3)
        '(proof-script-sticky-error-face :background red :underline yellow)
        '(proof-script-highlight-error-face :inherit 'proof-script-sticky-error-face
                                            :weight 'semi-bold :slant 'italic)
        '(proof-highlight-dependent-name-face :foreground magenta)
        '(proof-highlight-dependency-name-face :foreground violet)
        '(proof-declaration-name-face :foreground cyan)
        '(proof-tacticals-name-face :foreground green)
        '(proof-tactics-name-face :foreground teal)
        '(proof-error-face :foreground red :weight 'semi-bold)
        '(proof-warning-face :foreground yellow :weight 'semi-bold)
        '(proof-debug-message-face :foreground orange)
        '(proof-boring-face :foreground base5)
        '(proof-eager-annotation-face :inherit 'proof-warning-face :weight 'normal)
        '(proof-mouse-highlight-face :inherit 'lazy-highlight)
        '(proof-region-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
        '(proof-command-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
        '(proof-active-area-face :inherit 'secondary-selection)
        '(easycrypt-tactics-tacticals-face :inherit 'proof-tacticals-name-face)
        '(easycrypt-tactics-closing-face :foreground yellow)
        '(easycrypt-tactics-dangerous-face :foreground red))
      (enable-theme 'doom-nord)))
   ((eq ece-enable-theme 'light)
    (with-eval-after-load 'doom-themes
      (setopt doom-nord-light-brighter-modeline t
              doom-nord-light-brighter-comments nil
              doom-nord-light-comment-bg t
              doom-nord-light-region-highlight 'frost)
      (load-theme 'doom-nord-light t)
      (doom-themes-set-faces 'doom-nord-light
        '(trailing-whitespace :background magenta)
        '(aw-background-face :inherit 'avy-background-face)
        '(aw-leading-char-face :inherit 'avy-lead-face)
        '(proof-queue-face :background base6)
        '(proof-locked-face :background base3)
        '(proof-script-sticky-error-face :background red :underline yellow)
        '(proof-script-highlight-error-face :inherit 'proof-script-sticky-error-face
                                            :weight 'semi-bold :slant 'italic)
        '(proof-highlight-dependent-name-face :foreground magenta)
        '(proof-highlight-dependency-name-face :foreground violet)
        '(proof-declaration-name-face :foreground cyan)
        '(proof-tacticals-name-face :foreground green)
        '(proof-tactics-name-face :foreground teal)
        '(proof-error-face :foreground red :weight 'semi-bold)
        '(proof-warning-face :foreground yellow :weight 'semi-bold)
        '(proof-debug-message-face :foreground orange)
        '(proof-boring-face :foreground base5)
        '(proof-eager-annotation-face :inherit 'proof-warning-face :weight 'normal)
        '(proof-mouse-highlight-face :inherit 'lazy-highlight)
        '(proof-region-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
        '(proof-command-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
        '(proof-active-area-face :inherit 'secondary-selection)
        '(easycrypt-tactics-tacticals-face :inherit 'proof-tacticals-name-face)
        '(easycrypt-tactics-closing-face :foreground yellow)
        '(easycrypt-tactics-dangerous-face :foreground red))
      (enable-theme 'doom-nord-light)))))

;;;###autoload
(defun ece-setup ()
  "Setup EasyCrypt extensions (meant as hook for `easycrypt-mode`)"
  (ece--setup-indentation)
  (ece--setup-indentation-keybindings)
  (ece--setup-templates)
  (ece--setup-templates-keybindings)
  (ece--setup-templates-info)
  (ece--setup-keywords-completion)
  (ece--setup-auxiliary-functionality-keybindings)
  (ece--setup-theme))


(provide 'easycrypt-ext)

;;; easycrypt-ext ends here
