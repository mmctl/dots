;; -*- lexical-binding: t -*-
;; easycrypt-ext.el
(require 'easycrypt-ext-consts)


;; Customizations
(defgroup easycrypt-ext nil
  "Customization group for additional EasyCrypt functionality."
  :group 'easycrypt)

(defcustom ece-enable-indentation t
  "Non-nil (resp. `nil') to enable (resp. disable) enhanced
(but still ad-hoc) indentation in EasyCrypt."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-indentation-style 'local
  "`'local' or `'nonlocal' to make local or non-local
the default indentation style. The difference between the
two styles mainly pertains to indentation inside
enclosed expressions (e.g., between { and }, ( and ),
or [ and ]): `'local' indents w.r.t. previous
non-blank line in the expression; `'non-local' indents
w.r.t. expression opener (e.g., { or ( or [).
In any case, indentation using the opposite style is available
through the command `ece-indent-for-tab-command-inverse-style', which see.
Only has effect if `ece-enable-indentation', which see, is non-nil."
  :type '(choice
          (const :tag "Local indentation" local)
          (const :tag "Non-local indentation" nonlocal))
  :group 'easycrypt-ext)

(defcustom ece-enable-indentation-keybindings t
  "Non-nil (resp. `nil') to enable (resp. disable) suggested keybindings
for indentation-related commands in EasyCrypt."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-keywords-completion t
  "Non-nil (resp. `nil') to enable (resp. disable) completion for
EasyCrypt keywords (depends on `cape')."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-templates t
  "Non-nil (resp. `nil') to enable (resp. disable) code templates for
EasyCrypt (depends on `tempel'). If you enable this, it is recommended to
also enable enhanced indentation (see `ece-enable-indentation'),
since the templates use indentation and were made with the enhanced
EasyCrypt indentation in mind."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-templates-keybindings 'ece-enable-templates
  "Non-nil (resp. `nil') to enable (resp. disable) keybindings for inserting
EasyCrypt (regular) templates (depends on `tempel'). Does not make much sense
to enable this when you have disabled templates themselves (see `ece-enable-templates')."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-templates-info 'ece-enable-templates
  "Non-nil (resp. `nil') to enable (resp. disable) informative code templates
for EasyCrypt (depends on `tempel'). If you enable this, it is recommended to also
enable enhanced indentation (see `ece-enable-indentation'), since the
templates use indentation and were made with the enhanced EasyCrypt indentation in mind."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-auxiliary-functionality-keybindings t
  "Non-nil (resp. `nil') to enable (resp. disable) keybindings for additional
EasyCrypt functionality (e.g., printing/searching with mouse click)."
  :type 'boolean
  :group 'easycrypt-ext)

(defcustom ece-enable-theme nil
  "`'dark', `'light', or `nil' to enable the dark mode EasyCrypt theme,
light EasyCrypt theme, or no EasyCrypt theme, respectively (depends on `doom-themes').
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
  "Simply passes negation of ARG to `ece-basic-indent', which see."
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
  "Returns fallback indentation level for EasyCrypt code
(i.e., when no 'special indentation' case is detected)."
  (save-excursion
    ;; Go to (indentation of) previous non-blank line
    (ece--goto-previous-nonblank-line)
    (back-to-indentation)
    ;; If previous non-blank line starts with a proof bullet (i.e., +, -, or *),
    ;; but doesn't end a comment (i.e., isn't `*)`)...
    (if (and (memq (char-after) ece-bullets-proof)
             (not (looking-at-p "\\*)")))
        ;; Then, align to that line + 2 (putting point right after bullet)
        (+ (current-indentation) 2)
      ; Else, get content of that line...
      (setq prev-line (buffer-substring-no-properties (pos-bol) (pos-eol)))
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
        (current-indentation)))))

(defun ece--indent-level ()
  "Returns desired indentation level of EasyCrypt code."
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
          (let ((comcl (looking-at-p "[\\^\\*]?\\*)"))
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
  "Indents line of EasyCrypt code."
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
If `ece-enable-indentation' is non-nil, `indent-line-function' will be set to
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
be preceded by a `*` to form a comment closer), or (2) the last
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
                        (string-match-p "^[[:blank:]]*\\*?$" line-before))
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


;; Shell commands
(defun ece--validate-shell-command (command)
  "Checks if the provided `command' is a valid/supported EasyCrypt shell command
(in the sense that a command below is implemented for it).
Prints a message informing the user if that is not the case."
  (or (member command '("print" "search" "locate"))
      (user-error "Unknown/Unsupported command: `%s'." command)))

(defun ece--exec-shell-command (command &rest args)
  "Combines `command' and `args' into a command for the EasyCrypt shell, and
directly calls the shell with it."
  (if (fboundp 'proof-shell-invisible-command)
      (progn
        ;; proof-shell-ready-prover called inside proof-shell-invisible-command
        (if args
            (proof-shell-invisible-command (concat command " " (string-join args " ")))
          (proof-shell-invisible-command command)))
    (user-error "Necessary functionality for executing proof commands not found. Did you load Proof General?")))

(defun ece--prompt-command (command)
  "Prompts user for arguments that are passed to the `command' command of EasyCrypt.
If `command' is not valid, prints a message informing
the user (see 'ece--validate-shell-command`)."
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
  "If `event' is a mouse event, tries to find a (reasonable) thing at mouse
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
  "If `event' is a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. The result is used as an
argument to the `command' command of EasyCrypt. If nothing (reasonable) is
found, or the provided `command' is not valid, prints a message informing
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
  "If called with a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `print' command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "print" event))

;;;###autoload
(defun ece-search (&optional event)
  "If called with a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `search' command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "search" event))

;;;###autoload
(defun ece-locate (&optional event)
  "If called with a mouse event, tries to find a (reasonable) thing at mouse
(ignoring any active region). Otherwise, takes the active region
or tries to find a (reasonable) thing at point. Uses the result as an
argument to the `locate' command in EasyCrypt."
  (interactive (list (if (mouse-event-p last-input-event)
                         last-input-event
                       nil)))
  (ece--command "locate" event))


;; Key maps
(defvar-keymap ece-proof-mode-process-repeat-map
  :doc "Keymap (repeatable) for processing proof commands"
  :repeat (:hints ((proof-undo-last-successful-command . "p/u: Undo last successful command")
                   (proof-assert-next-command-interactive . "n: Assert next command")
                   (proof-undo-and-delete-last-successful-command . "d: Undo and delete last successful command")))
  "u" #'proof-undo-last-successful-command
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
    (keymap-local-set "M-i" #'indent-for-tab-command)
    (keymap-local-set "M-I" #'ece-indent-for-tab-command-inverse-style)))

;;; Auxiliary functionality
(defun ece--setup-auxiliary-functionality-keybindings ()
  (when ece-enable-auxiliary-functionality-keybindings
    (keymap-set easycrypt-mode-map "C-c C-y p" #'ece-print)
    (keymap-set easycrypt-mode-map "C-c C-y P" #'ece-prompt-print)
    (keymap-set easycrypt-mode-map "C-c C-y l" #'ece-locate)
    (keymap-set easycrypt-mode-map "C-c C-y L" #'ece-prompt-locate)
    (keymap-set easycrypt-mode-map "C-c C-y s" #'ece-search)
    (keymap-set easycrypt-mode-map "C-c C-y S" #'ece-prompt-search)
    (keymap-set easycrypt-mode-map "S-<down-mouse-3>" #'ece-print)
    (keymap-set easycrypt-mode-map "S-<mouse-3>" #'ignore)
    (keymap-set easycrypt-mode-map "C-<down-mouse-3>" #'ece-locate)
    (keymap-set easycrypt-mode-map "C-<mouse-3>" #'ignore)
    (keymap-set easycrypt-mode-map "M-<down-mouse-3>" #'ece-search)
    (keymap-set easycrypt-mode-map "M-<mouse-3>" #'ignore)
    (keymap-set easycrypt-response-mode-map "C-c C-y p" #'ece-print)
    (keymap-set easycrypt-response-mode-map "C-c C-y P" #'ece-prompt-print)
    (keymap-set easycrypt-response-mode-map "C-c C-y l" #'ece-locate)
    (keymap-set easycrypt-response-mode-map "C-c C-y L" #'ece-prompt-locate)
    (keymap-set easycrypt-response-mode-map "C-c C-y s" #'ece-search)
    (keymap-set easycrypt-response-mode-map "C-c C-y S" #'ece-prompt-search)
    (keymap-set easycrypt-response-mode-map "S-<down-mouse-3>" #'ece-print)
    (keymap-set easycrypt-response-mode-map "S-<mouse-3>" #'ignore)
    (keymap-set easycrypt-response-mode-map "C-<down-mouse-3>" #'ece-locate)
    (keymap-set easycrypt-response-mode-map "C-<mouse-3>" #'ignore)
    (keymap-set easycrypt-response-mode-map "M-<down-mouse-3>" #'ece-search)
    (keymap-set easycrypt-response-mode-map "M-<mouse-3>" #'ignore)
    (keymap-set easycrypt-goals-mode-map "C-c C-y p" #'ece-print)
    (keymap-set easycrypt-goals-mode-map "C-c C-y P" #'ece-prompt-print)
    (keymap-set easycrypt-goals-mode-map "C-c C-y l" #'ece-locate)
    (keymap-set easycrypt-goals-mode-map "C-c C-y L" #'ece-prompt-locate)
    (keymap-set easycrypt-goals-mode-map "C-c C-y s" #'ece-search)
    (keymap-set easycrypt-goals-mode-map "C-c C-y S" #'ece-prompt-search)
    (keymap-set easycrypt-goals-mode-map "S-<down-mouse-3>" #'ece-print)
    (keymap-set easycrypt-goals-mode-map "S-<mouse-3>" #'ignore)
    (keymap-set easycrypt-goals-mode-map "C-<down-mouse-3>" #'ece-locate)
    (keymap-set easycrypt-goals-mode-map "C-<mouse-3>" #'ignore)
    (keymap-set easycrypt-goals-mode-map "M-<down-mouse-3>" #'ece-search)
    (keymap-set easycrypt-goals-mode-map "M-<mouse-3>" #'ignore)))

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
  "Setup EasyCrypt extensions (meant as hook for `easycrypt-mode')"
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
