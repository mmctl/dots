;; -*- lexical-binding: t -*-
;; easycrypt-ext.el
;;
;; EasyCrypt is a toolset primarily designed for the formal verification
;; of code-based, game-playing crytpographic proofs. At its core,
;; it features an interactive theorem prover with a front-end implemented
;; in Proof General.
;; This package aims to add useful extensions to this EasyCrypt front-end.
;; Key features include the following:
;; - improved (but still ad-hoc) indentation;
;; - keyword completion (requires `cape', specifically `cape-keyword');
;; - code templates (requires `tempel');
;; - informative templates (requires `tempel'); and
;; - auxiliary functionality for dynamic printing, searching, and locating
;;   of items through keybindings or mouse clicks (eliminating the need to manually
;;   type the corresponding commands).
;; These features are (partially) implemented through three minor modes, one
;; for each of the major modes provided by the existing front-end:
;; - `easycrypt-ext-mode', for `easycrypt-mode';
;; - `easycrypt-ext-goals-mode', for `easycrypt-goals-mode'; and
;; - `easycrypt-ext-response-mode', for `easycrypt-response-mode'.
;;
;; For setup and usage instructions, see: TODO
;;
;; Author/Maintainer: Matthias Meijers
;; Date: 01-06-2025
(require 'easycrypt-ext-consts)


;; Constants
(defconst ece--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where this file is stored (and so also where rest of package should
  be).")

(defconst ece--templates-file
  (file-name-concat ece--dir "easycrypt-ext-templates.eld")
  "File where code templates for EasyCrypt are stored.")

(defconst ece--templates-info-file
  (file-name-concat ece--dir "easycrypt-ext-templates-info.eld")
  "File where informative code templates for EasyCrypt are stored.")


;; Customization options
(defgroup easycrypt-ext nil
  "Customization group for EasyCrypt extension package."
  :prefix "ece-"
  :group 'easycrypt)

(defcustom ece-indentation t
  "Non-nil (resp. `nil') to enable (resp. disable) enhanced
(but still ad-hoc) indentation in EasyCrypt."
  :type 'boolean
  :group 'easycrypt-ext
  :initialize #'custom-initialize-default
  :set #'(lambda (symbol value)
           (ece--configure-indentation value)
           (set-default-toplevel-value symbol value)
           (if value
               (message "EasyCrypt Ext indentation enabled! Current style: %s." ece-indentation-style)
             (message "EasyCrypt Ext indentation disabled!"))
           value))

(defcustom ece-indentation-style 'local
  "\='local or \='nonlocal to make local or non-local
the default indentation style. The difference between the
two styles mainly pertains to indentation inside
enclosed expressions (e.g., between { and }, ( and ),
or [ and ]): \='local indents w.r.t. previous
non-blank line in the expression; \='non-local indents
w.r.t. expression opener (e.g., { or ( or [).
In any case, indentation using the opposite style is available
through the command `ece-indent-for-tab-command-inverse-style', which see.
Only has effect if `ece-indentation', which see, is non-nil."
  :type '(choice
          (const :tag "Local indentation style" local)
          (const :tag "Non-local indentation style" nonlocal))
  :group 'easycrypt-ext)

(defcustom ece-keyword-completion nil
  "Non-nil (resp. nil) to enable (resp. disable) completion for
EasyCrypt keywords (depends on `cape')."
  :type 'boolean
  :group 'easycrypt-ext
  :initialize #'custom-initialize-default
  :set #'(lambda (symbol value)
           (if (not (featurep 'cape-keyword))
               (user-error "Package `cape-keyword' is required for this option, but was not detected. Load `cape-keyword' and try again.")
             (ece--configure-keyword-completion value)
             (set-default-toplevel-value symbol value)
             (message "EasyCrypt Ext keywords completion %s!" (if value "enabled" "disabled")))))

(defcustom ece-templates nil
  "Non-nil (resp. `nil') to enable (resp. disable) code templates for
EasyCrypt (depends on `tempel'). If you enable this, it is recommended to
also enable enhanced indentation (see `ece-indentation'),
since the templates use indentation and were made with the enhanced
EasyCrypt indentation in mind."
  :type 'boolean
  :group 'easycrypt-ext
  :initialize #'custom-initialize-default
  :set #'(lambda (symbol value)
           (if (not (featurep 'tempel))
               (user-error "Package `tempel' is required for this option, but was not detected. Load `tempel' and try again.")
             (ece--configure-templates value)
             (set-default-toplevel-value symbol value)
             (message "EasyCrypt Ext templates %s!" (if value "enabled" "disabled")))))

(defcustom ece-templates-info 'ece-templates
  "Non-nil (resp. `nil') to enable (resp. disable) informative code templates
for EasyCrypt (depends on `tempel'). If you enable this, it is recommended to
also enable enhanced indentation (see `ece-indentation'), since the templates
use indentation and were made with the enhanced EasyCrypt indentation in mind."
  :type 'boolean
  :group 'easycrypt-ext
  :initialize #'custom-initialize-default
  :set #'(lambda (symbol value)
           (if (not (featurep 'tempel))
               (user-error "Package `tempel' is required for this option, but was not detected. Load `tempel' and try again.")
             (ece--configure-templates-info value)
             (set-default-toplevel-value symbol value)
             (message "EasyCrypt Ext templates %s!" (if value "enabled" "disabled")))))

;; Indentation
(defun ece--insert-tabs-of-whitespace (n)
  "Insert N literal tabs worth of whitespace, respecting `indent-tabs-mode'."
  (if indent-tabs-mode
      ;; Then, insert N actual tab characters
      (insert (make-string n ?\t))
    ;; Else, insert N * `tab-width' spaces
    (insert (make-string (* n tab-width) ?\s))))

;;; Basic indentation
;;;###autoload
(defun ece-basic-indent (arg)
  "Indent (ARG > 0) resp. de-indent (ARG < 0) all lines touched by the
active region by |ARG| tab stops.
If no region is active and point is inside indentation,
then indent (ARG > 0) resp. de-indent (ARG < 0) current line |ARG| times
(respecting tab stops).
If no region is active and point is in the middle of a line, insert (ARG > 0)
or delete (ARG < 0) literal whitespace (|ARG| * `tab-width' worth).
If ARG < 0 and there is no whitespace behind point in the middle of a line,
again de-indent line |ARG| times (respecting tab stops)."
  (interactive "p")
  (let* ((neg (< arg 0))
         (count (if neg (abs arg) arg))
         (orig-point (point))
         (orig-col (current-column))
         (orig-ind (current-indentation)))
    ;; If region is active,...
    (if (use-region-p)
        ;; Then, store start position of point and compute indentation region
        (let ((ind-region-start (save-excursion (goto-char (region-beginning))
                                                (pos-bol)))
              (ind-region-stop (save-excursion (goto-char (region-end))
                                               (when (bolp) (forward-line -1))
                                               (pos-eol))))
          ;; If it is inside region to indent within margins of indentation,
          ;; move to (end of) indentation
          ;; (Otherwise, leave point as is)
          (cond
           ;; If point is before start of region to indent...
           ((< orig-point ind-region-start)
            ;; Then, move it to indentation on first line of region to indent
            (goto-char ind-region-start)
            (back-to-indentation))
           ;; Else, if point is after end of region to indent...
           ((< ind-region-stop orig-point)
            ;; Then, move it to indentation on last line of region to indent
            (goto-char ind-region-stop)
            (back-to-indentation))
           ;; Else (point is inside region to indent), move it to indentation of current line
           ((< orig-col orig-ind)
            (back-to-indentation)))
          ;; Indent indentation region appropriately
          (if neg
              (dotimes (_ count) (indent-rigidly-left-to-tab-stop ind-region-start ind-region-stop))
            (dotimes (_ count) (indent-rigidly-right-to-tab-stop ind-region-start ind-region-stop)))
          ;; Don't deactivate-mark, so we don't have to re-select region to repeat
          (setq-local deactivate-mark nil))
      ;; Else (no region is active), if line is empty...
      (if (string-match-p "^[[:blank:]]*$" (buffer-substring-no-properties (pos-bol) (pos-eol)))
          ;; Then, if ARG < 0...
          (if neg
              ;; Then, remove (at most) |ARG| tabs worth of whitespace before point
              (delete-region (max (pos-bol) (- orig-point (* count tab-width))) orig-point)
            ;; Else, simply insert ARG tabs worth of whitespace
            (ece--insert-tabs-of-whitespace count))
        ;; Else (line is not empty), if inside indentation...
        (if (<= orig-col orig-ind)
            ;; Then, (de-)indent current line (at most) |ARG| times (and move to indentation)
            (progn
              (if neg
                  (dotimes (_ count) (indent-rigidly-left-to-tab-stop (pos-bol) (pos-eol)))
                (dotimes (_ count) (indent-rigidly-right-to-tab-stop (pos-bol) (pos-eol))))
              (back-to-indentation))
          ;; Else (line is not empty and not inside indentation), if ARG < 0...
          (if neg
              ;; Then, take stock of whitespace before point...
              (let* ((del-ub (min (* count tab-width) (- orig-point (pos-bol))))
                     (del (save-excursion
                            (skip-chars-backward "[:space:]" (- orig-point del-ub))
                            (- orig-point (point)))))
                ;; If there is at least some whitespace before point...
                (if (< 0 del)
                    ;; Then, delete |ARG| * tab-width worth of whitespace
                    ;; (at most, until first non-whitespace character)
                    (delete-region (- orig-point del) orig-point)
                  ;; Else, de-indent current line (at most) |ARG| times
                  (dotimes (_ count) (indent-rigidly-left-to-tab-stop (pos-bol) (pos-eol)))))
            ;; Else (ARG >= 0), simply insert ARG tabs worth of whitespace
            (ece--insert-tabs-of-whitespace count)))))))

;;;###autoload
(defun ece-basic-deindent (arg)
  "Passes negation of ARG to `ece-basic-indent', which see."
  (interactive "p")
  (ece-basic-indent (- arg)))

;;; Contextual indentation
(defun ece--goto-previous-nonblank-line ()
  "Moves point to start of previous non-blank line,
or beginning of buffer if there is no such line."
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:blank:]]*$"))
      (forward-line -1)))

(defun ece--indent-level-fallback ()
  "Returns fallback indentation level for EasyCrypt code (i.e., when no `special
indentation' case is detected). In short, the behavior is as follows. If
previous non-blank line start with a proof bullet (i.e., `+', `-', or `*'),
indent as to put point after proof bullet. Else, if the previous non-blank line
is an unfinished non-proof/non-code specification (e.g., starts with `lemma' but
does not end with a `.'), indent 1 tab further than that line. Else, align with
previous non-blank line."
  (save-excursion
    ;; Go to (indentation of) previous non-blank line
    (ece--goto-previous-nonblank-line)
    (back-to-indentation)
    ;; If previous non-blank line is outside any special construct
    ;; (e.g., comments, strings, and enclosed expressions)
    ;; and starts with a proof bullet (i.e., `+', `-', or `*'),
    (if-let* ((synps (syntax-ppss))
              ((not (nth 1 synps)))
              ((not (nth 3 synps)))
              ((not (nth 4 synps)))
              ((memq (char-after) ece-bullets-proof)))
        ;; Then, align to that line + 2 (putting point right after bullet)
        (+ (current-indentation) 2)
      ; Else, get content of that line...
      (let ((prev-line (buffer-substring-no-properties (pos-bol) (pos-eol))))
        ;; If that line is an unfinished non-proof/non-proc spec
        ;; (E.g., starts with `lemma' but does not end with a `.')
        ;; Here, we also count a comma as ending a spec to deal with the case of
        ;; instantiation in `clone' (and hope it isn't common to end a line
        ;; with a comma outside of `clone').
        (if (and (seq-some (lambda (kw)
                             (string-match-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b") prev-line))
                           ece-keywords-start)
                 (not (string-match-p "[\\.,][[:blank:]]*\\(?:(\\*.*\\*)\\)?[[:blank:]]*$" prev-line)))
            ;; Then, align with that line + tab
            (+ (current-indentation) tab-width)
          ;; Else, align with that line (default)
          (current-indentation))))))

(defun ece--indent-level ()
  "Returns desired indentation level of EasyCrypt code.
In short, the default behavior is as follows.
- If we are in a multi-line comment, align with the previous non-blank line in
comment if it exists; otherwise, indent 1 tab beyond comment opener.
- Else, if we are in a multi-line string, remove indentation (because it
affects the value of the string).
- Else, if we are in an non-code expression (i.e., between `(' and `)' or
`[' and `]', or after `(' or `[' without a corresponding closer),
align with the previous non-blank line in expression if it exists; otherwise,
indent 1 space beyond expression opener.
- Else, if we are in a code expression (i.e., between `{' and `}', or after
`{' without a corresponding closer), align with the previous non-blank line
in code block if it exists; otherwise, indent 1 tab beyond the line that
started the specification corresponding to the code block (e.g., the line
that starts with `module' or `proc'.
- Else, if we are opening a proof (e.g., our line starts with `proof' or
`realize'), align to line that started the proof statement specification
(e.g., the line starting with `lemma'); if we cannot find such a line,
use fallback indentation.
- Else, if we are closing a proof (e.g., our line starts with `qed'), align to
line that opened the proof (e.g., the previous line starting with `proof'); if
we cannot find such a line, use fallback indentation.
- Else, use fallback indentation.
Here, fallback indentation refers to the indentation computed by
`ece--indent-level-fallback', which see."
  (let ((indent-level 0))
    (save-excursion
      ;; Base decision on context of line's first non-whitespace character
      ;; (or beginning of line if empty)
      (back-to-indentation)
      (let* ((synps (syntax-ppss))
             (exprop (nth 1 synps))
             (instr (nth 3 synps))
             (incom (nth 4 synps))
             (csop (nth 8 synps)))
        (cond
         ;; If we are in a comment...
         (incom
          (let ((comcl (looking-at-p (regexp-opt ece-delimiters-comments-close)))
                (opcol (save-excursion (goto-char csop) (current-column))))
            ;; Then, if our line closes the comment...
            (if (and incom comcl)
                ;; Then, align closer with the opener
                (setq indent-level opcol)
              ;; Else, if indentation style is non-local...
              (if (eq ece-indentation-style 'nonlocal)
                  ;; Then, align to opener + tab
                  (setq indent-level (+ opcol tab-width))
                ;; Else (indentation style is local)...
                (ece--goto-previous-nonblank-line) ; Go to beginning of previous non-blank line
                ;; If there is a previous non-blank line inside comment...
                (if (< (line-number-at-pos csop) (line-number-at-pos))
                    ;; Then, align to that line
                    (setq indent-level (current-indentation))
                  ;; Else, align to opener + tab
                  (setq indent-level (+ opcol tab-width)))))))
         ;; Else, if we are in a string...
         (instr
          ;; Then, set indent-level to 0 (because indentation affects value of string)
          (setq indent-level 0))
         ;; Else, if we are in an opened or enclosed expression...
         ;; E.g., between { and }, or ( and ), or after { with no closing counterpart
         (exprop
          (let* ((chcl (char-after))
                 (choci (save-excursion
                          (goto-char exprop)
                          (list (char-after) (current-column) (current-indentation))))
                 (chop (nth 0 choci))
                 (opcol (nth 1 choci))
                 (opind (nth 2 choci)))
              ;; Then, if opener is an expression opener (`[` or `(`)...
              (if (memq chop ece-delimiters-expression-open)
                  ;; Then, if first char on our line is a matching closer...
                  (if (eq chcl (matching-paren chop))
                      ;; Then, align to column of opener
                      (setq indent-level opcol)
                    ;; Else, if indentation style is non-local...
                    (if (eq ece-indentation-style 'nonlocal)
                        ;; Then, align to opener + 1
                        (setq indent-level (+ opcol 1))
                      ;; Else (indentation style is local)...
                      (ece--goto-previous-nonblank-line) ; Go to beginning of previous non-blank line
                      ;; If there is previous non-blank line in the enclosed expression...
                      (if (< (line-number-at-pos exprop) (line-number-at-pos))
                          ;; Then, align to that line
                          (setq indent-level (current-indentation))
                        ;; Else, align to opener + 1
                        (setq indent-level (+ opcol 1)))))
                ;; Else, if opener is a code opener (i.e., `{`)...
                (if (memq chop ece-delimiters-code-open)
                    ;; Then,...
                    (let ((bob nil))
                      (progn
                        ;; Find "imperative spec starter" (i.e., a keyword
                        ;; opening a code block, like module or proc) or
                        ;; "scoper" (i.e., keyword defining the scope of an
                        ;; artifact, like local, global, or declare)
                        (save-excursion
                          (goto-char exprop)
                          (forward-line 0)
                          (while (and (not (bobp))
                                      (not (seq-some (lambda (kw) (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b")))
                                                     ece-keywords-imperative-spec-start-scope)))
                            (forward-line -1))
                          ;; If we didn't find such a line (i.e., we are at the beginning of a buffer)...
                          (if (bobp)
                              ;; Then, record this fact
                              (setq bob t)
                            ;; Else, record indentation of keyword's line
                            (setq indent-level (current-indentation))))
                        ;; If we didn't find a valid keyword,...
                        (if bob
                            ;; Then, fallback to indenting relative to opening brace instead
                            ;; So, if first char on our line is a matching closing brace...
                            (if (eq chcl (matching-paren chop))
                                ;; Then, align to indentation of opening brace's line
                                (setq indent-level opind)
                              ;; Else, if indentation style is non-local...
                              (if (eq ece-indentation-style 'nonlocal)
                                  ;; Then, align to indentation of opening brace's line + tab
                                  (setq indent-level (+ opind tab-width))
                                ;; Else (indentation style is local)...
                                (ece--goto-previous-nonblank-line) ; Go to beginning of previous non-blank line
                                ;; If there is previous non-blank line in the enclosed expression...
                                (if (< (line-number-at-pos exprop) (line-number-at-pos))
                                    ;; Then, align to that line
                                    (setq indent-level (current-indentation))
                                  ;; Else, align to indentation of opening brace's line + tab
                                  (setq indent-level (+ opind tab-width)))))
                          ;; Else (we found a valid keyword and `indent-level` recorded
                          ;; indentation of corresponding line),
                          ;; if first char on our line is *not* a matching closer...
                          (unless (eq chcl (matching-paren chop))
                            ;; Else, if indentation style is non-local...
                              (if (eq ece-indentation-style 'nonlocal)
                                  ;; Then, align to recorded indentation of keyword's line + tab
                                  (setq indent-level (+ indent-level tab-width))
                                ;; Else (indentation style is local)...
                                (ece--goto-previous-nonblank-line) ; Go to beginning of previous non-blank line
                                ;; If there is previous non-blank line in the enclosed expression...
                                (if (< (line-number-at-pos exprop) (line-number-at-pos))
                                    ;; Then, align to that line
                                    (setq indent-level (current-indentation))
                                  ;; Else, align to recorded indentation of keyword's line + tab
                                  (setq indent-level (+ indent-level tab-width))))))))
                  ;; Else, parsing indicates we are in an expression that has
                  ;; an unknown opener, which shouldn't be possible.
                  ;; Although this is a bug and should be fixed, don't annoy user
                  ;; by throwing an error. Instead, log and use fallback.
                  (message "%s %s %s"
                           "ece--indent-level: parsing indicates expression with unknown delimiter."
                           "This should not be possible, please report."
                           "Using fallback indentation.")
                  (setq indent-level (ece--indent-level-fallback))))))
         ;; Else, if we are looking at a proof starter (e.g., proof or realize)
         ((seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                    ece-keywords-proof-start)
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
                                  ece-keywords-proof-spec-start)))
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
                    ece-keywords-proof-end)
          (let ((bob nil))
            (progn
              (save-excursion
                ;; Find line that started the proof (i.e., one that starts with "proof")
                (forward-line -1)
                (while (and (not (bobp))
                            (not (seq-some (lambda (kw) (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b")))
                                           ece-keywords-proof-start)))
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
  "Indents line of EasyCrypt code as per `ece--indent-level', which see."
  (interactive)
  ;; Indent accordingly
  (let ((indent-level (ece--indent-level)))
    ;; `indent-line-to' would move point to new indentation, and
    ;; we prevent this by `save-excursion' so point position remains consistent
    ;; (making templates more consistent as well)
    (save-excursion
      (indent-line-to indent-level))
    ;; Still adjust point if it's inside indentation
    (when (< (current-column) (current-indentation))
      (back-to-indentation))))

;;;###autoload
(defun ece-indent-for-tab-command-inverse-style ()
  "Calls `indent-for-tab-command' with `ece-indentation-style' inverted.
If `ece-indentation' is non-nil, `indent-line-function' will be set to
`ece-indent-line', which is used by `indent-for-tab-command' to indent a line
or region. So, this command essentially performs indentation according to the
style that is currently not selected."
  (interactive)
  (let ((ece-indentation-style (if (eq ece-indentation-style 'local) 'nonlocal 'local)))
    (indent-for-tab-command)))

;;;###autoload
(defun ece-indent-on-insertion-closer ()
  "Indent when (1) last input was one of }, ), ], and it is the
first character on current line (as an exception, `)` may also directly
be preceded by symbols that make it a comment closer), or (2) the last
input was . and the current line starts/ends a proof. However, only
allow de-indents (to prevent automatically indenting
code that has been manually de-indented; this is a hack
and a limitation of the localized ad-hoc computation
of the indent level).
Meant for `post-self-insert-hook'."
  (when-let* ((line-before (buffer-substring-no-properties (pos-bol) (- (point) 1)))
              ((or (and (memq last-command-event '(?\} ?\]))
                        (string-match-p "^[[:blank:]]*$" line-before))
                   (and (eq last-command-event ?\))
                        (string-match-p
                         (concat "^[[:blank:]]*" (regexp-opt (push (string ?\)) ece-delimiters-comments-close)) "$")
                         (concat line-before (string ?\)))))
                   (and (eq last-command-event ?\.)
                        (save-excursion
                          (back-to-indentation)
                          (seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                                    ece-keywords-proof-delimit)))))
              (orig-col (current-column))
              (indent-level (ece--indent-level))
              (indent-diff (- (current-indentation) indent-level)))
    ;; If 0 < indent-diff, i.e., we are de-indenting...
    (when (< 0 indent-diff)
      ;; Go to the computed indent level...
      (indent-line-to indent-level)
      ;; And keep point in same relative position
      ;; (`indent-line-to' moves it to end of indentation)
      (move-to-column (- orig-col indent-diff)))))


;; Auxiliary functionality
;;; Proof shell commands
(defun ece--validate-shell-command (command)
  "Checks if the COMMAND is a valid/supported EasyCrypt shell command
(in the sense that a command below is implemented for it).
Prints a message informing the user if that is not the case."
  (or (member command '("print" "search" "locate"))
      (user-error "Unknown/Unsupported command: `%s'." command)))

(defun ece--exec-shell-command (command &rest args)
  "Combines COMMAND and ARGS into a command for the EasyCrypt shell, and
directly calls the shell with it."
  (if (fboundp 'proof-shell-invisible-command)
      (progn
        ;; proof-shell-ready-prover called inside proof-shell-invisible-command
        (if args
            (proof-shell-invisible-command (concat command " " (string-join args " ")))
          (proof-shell-invisible-command command)))
    (user-error "Necessary functionality for executing proof commands not found. Did you load Proof General?")))

(defun ece--prompt-command (command)
  "Prompts user for arguments that are passed to the COMMAND command of EasyCrypt.
If COMMAND is not valid, prints a message informing
the user (see `ece--validate-shell-command')."
  (when (ece--validate-shell-command command)
    (let ((arg (read-string (format "Provide arguments for '%s': " command))))
      (if arg
          (ece--exec-shell-command command arg)
        (user-error "Please provide an argument.")))))

;;;###autoload
(defun ece-prompt-print ()
  "Prompts user for arguments that are passed to the `print' command of EasyCrypt."
  (interactive)
  (ece--prompt-command "print"))

;;;###autoload
(defun ece-prompt-search ()
  "Prompts user for arguments that are passed to the `search' command of EasyCrypt."
  (interactive)
  (ece--prompt-command "search"))

;;;###autoload
(defun ece-prompt-locate ()
  "Prompts user for arguments that are passed to the `locate' command of EasyCrypt."
  (interactive)
  (ece--prompt-command "locate"))

(defun ece--thing-at (event)
  "If EVENT is a mouse event, tries to find a (reasonable) thing at mouse
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
  "If EVENT is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. The result is used as an
argument to the COMMAND command of EasyCrypt. If nothing (reasonable) is
found, or the provided COMMAND is not valid, prints a message informing
the user (see `ece--validate-shell-command')."
  (when (ece--validate-shell-command command)
    (let ((arg (ece--thing-at event)))
      (if arg
          (ece--exec-shell-command command arg)
        (user-error "No reasonable thing at %s found for command `%s'.%s"
                    (if (mouse-event-p event) "mouse" "point")
                    command
                    (if (mouse-event-p event) "" " Try selecting the thing if automatic detection doesn't work."))))))

;;;###autoload
(defun ece-print (&optional event)
  "If EVENT is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `print' command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "print" event))

;;;###autoload
(defun ece-search (&optional event)
  "If EVENT is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `search' command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "search" event))

;;;###autoload
(defun ece-locate (&optional event)
  "If EVENT is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `locate' command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "locate" event))


;; Templates
(defun ece--tempel-placeholder-form-as-lit (elt)
"Defines slight adjustment of regular placeholder element
so that a prompt form evaluating to a string is inserted as
default value in the same way as a literal string prompt."
  (pcase elt
    (`(pfl ,prompt . ,rest)
     (let ((evprompt (eval prompt)))
       (if (stringp evprompt)
           `(p ,evprompt ,@rest)
         `('p ,prompt ,@rest))))))

(defun ece--tempel-include (elt)
  "Defines `include' element (taken and slightly adjusted from TempEL github repo)
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

(defsubst ece--templates-file-read ()
  (ece--tempel-template-file-read ece--templates-file))

(defsubst ece--templates-info-file-read ()
  (ece--tempel-template-file-read ece--templates-info-file))

(defmacro ece--tempel-key (keymap key template-name)
  "Binds KEY to TEMPLATE-NAME in KEYMAP.
Simplified version of `tempel-key' macro from `tempel' package."
  `(define-key ,keymap ,(key-parse key)
               ,(let ((cmd (intern (format "tempel-insert-%s" template-name))))
                  `(prog1 ',cmd
                     (defun ,cmd ()
                       ,(format "Insert template %s in the current buffer."
                                template-name)
                       (interactive)
                       (tempel-insert ',template-name))))))


;; Configuration
;; Indentation
(defvar-local original-indentation-state nil)
(defvar-local original-local-map nil)

(defun ece--enable-indentation-local ()
  (if original-indentation-state
      (setq-local tab-width 2
                  indent-line-function #'ece-indent-line
                  electric-indent-mode nil)
    (setq-local original-indentation-state
                (buffer-local-set-state tab-width 2
                                        indent-line-function #'ece-indent-line
                                        electric-indent-mode nil)))
  (add-hook 'post-self-insert-hook #'ece-indent-on-insertion-closer nil t)
  (let ((crlm (current-local-map)))
    (unless original-local-map
      (setq-local original-local-map crlm))
    (use-local-map (copy-keymap crlm))
    (keymap-local-set "RET" #'newline-and-indent)
    (keymap-local-set "<return>" "RET")
    (keymap-local-set "S-<return>" #'newline)
    (keymap-local-set "TAB" #'ece-basic-indent)
    (keymap-local-set "<tab>" "TAB")
    (keymap-local-set "<backtab>" #'ece-basic-deindent)
    (keymap-local-set "M-i" #'indent-for-tab-command)
    (keymap-local-set "M-I" #'ece-indent-for-tab-command-inverse-style)))

(defun ece--disable-indentation-local ()
  (when original-local-map
    (use-local-map original-local-map)
    (setq-local original-local-map nil))
  (remove-hook 'post-self-insert-hook #'ece-indent-on-insertion-closer t)
  (when original-indentation-state
    (buffer-local-restore-state original-indentation-state)
    (setq-local original-indentation-state nil)))

(defun ece--configure-indentation-local (enable)
  (if enable
      (ece--enable-indentation-local)
    (ece--disable-indentation-local)))

(defun ece--enable-indentation ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--enable-indentation-local)))))

(defun ece--disable-indentation ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--disable-indentation-local)))))

(defun ece--configure-indentation (enable)
  (if enable
      (ece--enable-indentation)
    (ece--disable-indentation)))

;;; Keyword completion
(defvar-local original-keyword-completion-state nil)

(defun ece--enable-keyword-completion-local ()
  (let* ((eckwcn (cons 'easycrypt-mode ece-keywords))
         (cpkwup (if (member eckwcn cape-keyword-list)
                     cape-keyword-list
                   (cons eckwcn cape-keyword-list))))
    (if original-keyword-completion-state
        (setq-local cape-keyword-list cpkwup)
      (setq-local original-keyword-completion-state
                  (buffer-local-set-state cape-keyword-list cpkwup)))))

(defun ece--disable-keyword-completion-local ()
  (when original-keyword-completion-state
    (buffer-local-restore-state original-keyword-completion-state)
    (setq-local original-keyword-completion-state nil)))

(defun ece--configure-keyword-completion-local (enable)
  (if enable
      (ece--enable-keyword-completion-local)
    (ece--disable-keyword-completion-local)))

(defun ece--enable-keyword-completion ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--enable-keyword-completion-local)))))

(defun ece--disable-keyword-completion ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--disable-keyword-completion-local)))))

(defun ece--configure-keyword-completion (enable)
  (if enable
      (ece--enable-keyword-completion)
    (ece--disable-keyword-completion)))


;;; Templates
(defvar-local original-templates-state nil)
(defvar-local original-templates-info-state nil)

(defvar-keymap ece-template-map
  :doc "Keymap for EasyCrypt templates."
  :prefix 'ece-template-map-prefix)

(eval-when-compile
  (ece--tempel-key ece-template-map "a" axiomn)
  (ece--tempel-key ece-template-map "A" abbrevn)
  (ece--tempel-key ece-template-map "b" byequiv)
  (ece--tempel-key ece-template-map "B" byphoare)
  (ece--tempel-key ece-template-map "c" conseq)
  (ece--tempel-key ece-template-map "C" conseqehh)
  (ece--tempel-key ece-template-map "d" doccommentn)
  (ece--tempel-key ece-template-map "D" declaremodule)
  (ece--tempel-key ece-template-map "e" equivn)
  (ece--tempel-key ece-template-map "E" equivnlemman)
  (ece--tempel-key ece-template-map "f" funn)
  (ece--tempel-key ece-template-map "F" fel)
  (ece--tempel-key ece-template-map "g" ge0)
  (ece--tempel-key ece-template-map "G" gt0)
  (ece--tempel-key ece-template-map "h" hoaren)
  (ece--tempel-key ece-template-map "H" hoarenlemman)
  (ece--tempel-key ece-template-map "i" ifelse)
  (ece--tempel-key ece-template-map "I" ifthenelse)
  (ece--tempel-key ece-template-map "l" lemman)
  (ece--tempel-key ece-template-map "L" letinn)
  (ece--tempel-key ece-template-map "m" module)
  (ece--tempel-key ece-template-map "M" modulept)
  (ece--tempel-key ece-template-map "o" op)
  (ece--tempel-key ece-template-map "O" opas)
  (ece--tempel-key ece-template-map "p" proc)
  (ece--tempel-key ece-template-map "P" procsig)
  (ece--tempel-key ece-template-map "r" rewrited)
  (ece--tempel-key ece-template-map "R" rngin)
  (ece--tempel-key ece-template-map "s" seq)
  (ece--tempel-key ece-template-map "S" seqph)
  (ece--tempel-key ece-template-map "t" moduletype)
  (ece--tempel-key ece-template-map "T" moduletypep)
  (ece--tempel-key ece-template-map "u" Prmub)
  (ece--tempel-key ece-template-map "U" Prmrub)
  (ece--tempel-key ece-template-map "v" Prmeq)
  (ece--tempel-key ece-template-map "V" Prmreq)
  (ece--tempel-key ece-template-map "w" whiles)
  (ece--tempel-key ece-template-map "W" whileph)
  (ece--tempel-key ece-template-map "x" cloneimportaswith)
  (ece--tempel-key ece-template-map "X" requireimport)
  (ece--tempel-key ece-template-map "y" phoaren)
  (ece--tempel-key ece-template-map "Y" phoare1n)
  (ece--tempel-key ece-template-map "z" theory)
  (ece--tempel-key ece-template-map "Z" abstracttheory))

(defun ece--enable-templates-local ()
  (let* ((ttsup (if (member #'ece--templates-file-read tempel-template-sources)
                    tempel-template-sources
                  (cons #'ece--templates-file-read tempel-template-sources)))
         (tueupp (if (member #'ece--tempel-placeholder-form-as-lit tempel-user-elements)
                     tempel-user-elements
                   (cons #'ece--tempel-placeholder-form-as-lit tempel-user-elements)))
         (tueup (if (member #'ece--tempel-include tueupp)
                    tueupp
                  (cons #'ece--tempel-include tueupp))))
    (if original-templates-state
        (setq-local tempel-user-elements tueup
                    tempel-template-sources ttsup)
      (setq-local original-templates-state
                  (buffer-local-set-state tempel-user-elements tueup
                                          tempel-template-sources ttsup)))))

(defun ece--disable-templates-local ()
  (when original-templates-state
    (buffer-local-restore-state original-templates-state)
    (setq-local original-templates-state nil)))

(defun ece--configure-templates-local (enable)
  (if enable
      (ece--enable-templates-local)
    (ece--disable-templates-local)))

(defun ece--enable-templates ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--enable-templates-local)))))

(defun ece--disable-templates ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--disable-templates-local)))))

(defun ece--configure-templates (enable)
  (if enable
      (ece--enable-templates)
    (ece--disable-templates)))

(defun ece--enable-templates-info-local ()
  (let ((ttsup (if (member #'ece--templates-info-file-read tempel-template-sources)
                   tempel-template-sources
                 (cons #'ece--templates-info-file-read tempel-template-sources))))
    (if original-templates-info-state
        (setq-local tempel-template-sources ttsup)
      (setq-local original-templates-info-state
                  (buffer-local-set-state tempel-template-sources ttsup)))))

(defun ece--disable-templates-info-local ()
  (when original-templates-info-state
    (buffer-local-restore-state original-templates-info-state)
    (setq-local original-templates-info-state nil)))

(defun ece--configure-templates-info-local (enable)
  (if enable
      (ece--enable-templates-info-local)
    (ece--disable-templates-info-local)))

(defun ece--enable-templates-info ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--enable-templates-info-local)))))

(defun ece--disable-templates-info ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when easycrypt-ext-mode
        (ece--disable-templates-info-local)))))

(defun ece--configure-templates-info (enable)
  (if enable
      (ece--enable-templates-info)
    (ece--disable-templates-info)))

;; Utilities
;;;###autoload
(defsubst ece-toggle-indentation-local ()
  (interactive)
  (ece--configure-indentation-local
   (if original-indentation-state nil t)))

;;;###autoload
(defsubst ece-toggle-keyword-completion-local ()
  (interactive)
  (ece--configure-keyword-completion-local
   (if original-keyword-completion-state nil t)))

;;;###autoload
(defun ece-toggle-templates-local ()
  (interactive)
  (ece--configure-templates-local
   (if original-templates-state nil t)))

;;;###autoload
(defun ece-toggle-templates-info-local ()
  (interactive)
  (ece--configure-templates-info-local
   (if original-templates-info-state nil t)))

(defun ece--toggle (symbol)
  (customize-set-variable symbol (if (symbol-value symbol) nil t)))

;;;###autoload
(defun ece-toggle-indentation ()
  (interactive)
  (ece--toggle 'ece-indentation))

;;;###autoload
(defun ece-toggle-keyword-completion ()
  (interactive)
  (ece--toggle 'ece-keyword-completion))

;;;###autoload
(defun ece-toggle-templates ()
  (interactive)
  (ece--toggle 'ece-templates))

;;;###autoload
(defun ece-toggle-templates-info ()
  (interactive)
  (ece--toggle 'ece-templates-info))


;; Keymaps
;;; Modes
(defvar-keymap easycrypt-ext-mode-map
  :doc "Keymap for `easycrypt-ext-mode'."
  "C-c C-y p" #'ece-print
  "C-c C-y P" #'ece-prompt-print
  "C-c C-y l" #'ece-locate
  "C-c C-y L" #'ece-prompt-locate
  "C-c C-y s" #'ece-search
  "C-c C-y S" #'ece-prompt-search
  "C-c C-y t" 'ece-template-map-prefix
  "C-c C-y o i" #'ece-toggle-indentation-local
  "C-c C-y o I" #'ece-toggle-indentation
  "C-c C-y o k" #'ece-toggle-keyword-completion-local
  "C-c C-y o K" #'ece-toggle-keyword-completion
  "C-c C-y o t" #'ece-toggle-templates-local
  "C-c C-y o T" #'ece-toggle-templates
  "C-c C-y o o" #'ece-toggle-templates-info-local
  "C-c C-y o O" #'ece-toggle-templates-info
  "C-S-<mouse-1>" #'ece-print
  "C-S-<mouse-2>" #'ece-locate
  "C-S-<mouse-3>" #'ece-search)

(defvar-keymap easycrypt-ext-goals-mode-map
  :doc "Keymap for `easycrypt-ext-goals-mode'."
  "C-c C-y p" #'ece-print
  "C-c C-y P" #'ece-prompt-print
  "C-c C-y l" #'ece-locate
  "C-c C-y L" #'ece-prompt-locate
  "C-c C-y s" #'ece-search
  "C-c C-y S" #'ece-prompt-search
  "C-S-<mouse-1>" #'ece-print
  "C-S-<mouse-2>" #'ece-locate
  "C-S-<mouse-3>" #'ece-search)

(defvar-keymap easycrypt-ext-response-mode-map
  :doc "Keymap for `easycrypt-ext-response-mode'."
  "C-c C-y p" #'ece-print
  "C-c C-y P" #'ece-prompt-print
  "C-c C-y l" #'ece-locate
  "C-c C-y L" #'ece-prompt-locate
  "C-c C-y s" #'ece-search
  "C-c C-y S" #'ece-prompt-search
  "C-S-<mouse-1>" #'ece-print
  "C-S-<mouse-2>" #'ece-locate
  "C-S-<mouse-3>" #'ece-search)

;;; Repeat
(defvar-keymap ece-proof-mode-process-repeat-map
  :doc "Keymap (repeatable) for processing proof commands."
  :repeat (:hints ((proof-undo-last-successful-command . "p/u: Undo last successful command")
                   (proof-assert-next-command-interactive . "n: Assert next command")
                   (proof-undo-and-delete-last-successful-command . "d: Undo and delete last successful command")))
  "u" #'proof-undo-last-successful-command
  "p" #'proof-undo-last-successful-command
  "n" #'proof-assert-next-command-interactive
  "d" #'proof-undo-and-delete-last-successful-command)

(defvar-keymap ece-bufhist-repeat-map
  :doc "Keymap (repeatable) for browsing and managing buffer history."
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

;;; Session setup/teardown
;;;###autoload
(defun ece-setup ()
  "Sets up EasyCrypt extensions."
  (ece--configure-indentation-local ece-indentation)

  (let ((cpcnf nil)
        (tpcnf nil))
    (when ece-keyword-completion
      (with-eval-after-load 'cape-keyword
        (ece--configure-keyword-completion-local ece-keyword-completion))
      (setq cpcnf (not (featurep 'cape-keyword))))

    (when (or ece-templates ece-templates-info)
      (with-eval-after-load 'tempel
        (ece--configure-templates-local ece-templates)
        (ece--configure-templates-info-local ece-templates-info))
      (setq tpcnf (not (featurep 'tempel))))

    (when (or cpcnf tpcnf)
      (message "Attempted to setup %s not detected. Loading dependencies at any point will complete the corresponding setup automatically."
               (cond
                ((and cpcnf tpcnf)
                 "keyword completion and templates, but dependencies `cape-keyword' and `tempel' were")
                (cpcnf
                 "keyword completion, but dependency `cape-keyword' was")
                (t
                 "templates, but dependency `tempel' was"))))))

;;;###autoload
(defun ece-teardown ()
  "Tears down EasyCrypt extensions.")

;; Minor modes
;;; Regular
(define-minor-mode easycrypt-ext-mode nil
  :lighter " ECE"
  :keymap easycrypt-ext-mode-map
  :interactive '(easycrypt-mode)
  (if easycrypt-ext-mode
      (ece-setup)
    (ece-teardown)))

;;; Goals
(define-minor-mode easycrypt-ext-goals-mode nil
  :lighter " ECEg"
  :keymap easycrypt-ext-goals-mode-map
  :interactive '(easycrypt-goals-mode))

;;; Response
(define-minor-mode easycrypt-ext-response-mode nil
  :lighter " ECEr"
  :keymap easycrypt-ext-response-mode-map
  :interactive '(easycrypt-response-mode))


(provide 'easycrypt-ext)

;;; easycrypt-ext.el ends here
