;; -*- lexical-binding: t -*-
;; Environment
(defconst ECE_DIR (file-name-as-directory
                   (file-name-concat user-emacs-directory "local/easycrypt-ext/"))
  "Directory where `easycrypt-ext` package is located. (By default it is the
local/easycrypt-ext/ directory, relative to your emacs configuration directory.
You can find this directory by launching Emacs, pressing `C-h v' (i.e., `Control
+ h' followed by `v'), typing `user-emacs-directory', and press Return (i.e.,
Enter).)")

;;; Add ECE_DIR to the load path, so we can, well, load it
(add-to-list 'load-path ECE_DIR)

;; Package system
;;; Load package system
(require 'package)

;;; Add MELPA package archive
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Initialize package system
(if (version< emacs-version "27.0")
    (package-initialize)
  (package-initialize t))

;;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;; Ensure use-package is loaded
(require 'use-package)


;; Packages
;; Completions
;;; Cape
;;; Provides completion-at-point (read: completion of thing you are typing)
;;; functions and related functionality. Used by `easycrypt-ext' to provide
;;; support for keyword completion through `cape-keyword'. Additionally allows
;;; for the addition/combination of other such completion functions, and easy
;;; integration with `tempel' for template (completion) support.
;; See: https://github.com/minad/cape
(use-package cape
  :ensure t)

;;; Tempel
;;; Provides functionality for defining and integrating templates in standard
;;; workflow. Used by `easycrypt-ext' to provide template support. Can be easily
;;; integrated with (and without) `cape'.
;;; See: https://github.com/minad/tempel
(use-package tempel
  :ensure t)

;;; Corfu
;;; Provides functionality to enhance and improve your experience
;;; when it comes to completions (i.e., actually using completion functions
;;; as provided by, e.g., `cape' and `tempel'). In particular, it provides
;;; an in-buffer completion popup.
;;; Not used by `easycrypt-ext' directly, but recommended.
;;; See: https://github.com/minad/corfu
(use-package corfu
  :ensure t

  :init

  ;; Consider uncommenting the following if you want the completion pop-up
  ;; to show up automatically (i.e., without explicitly calling it)
  ;; (setopt corfu-auto t) ; make pop-up automatic
  ;; (setopt corfu-preselect 'valid) ; select first candidate on pop-up
  ;; (setopt corfu-on-exact-match nil) ; don't automatically complete on single match

  :config
  ;; Consider uncommenting the following if think selecting a completion
  ;; shouldn't use your regular movement/editing keybindings.
  ;; May be especially annoying if you have an automatic pop-up
  ;; (keymap-unset corfu-map "RET") ; Don't use return/enter for accepting
  ;; (keymap-unset corfu-map "<up>") ; Don't use up arrow for going up
  ;; (keymap-unset corfu-map "<down>") ; Don't use up arrow for going up
  ;; (keymap-set corfu-map "C-p" #'corfu-previous) ; Example: Use `Control + p' for going up
  ;; (keymap-set corfu-map "C-n" #'corfu-next) ; Example: Use `Control + n' for going down
  ;; (keymap-set corfu-map "C-v" #'corfu-next) ; Example: Use `Control + v' for accepting
  )

;; Proof assistants
;;; Proof-General
;;; See: https://github.com/ProofGeneral/PG
(use-package proof-general
  :ensure t

  :pin melpa ; Get the up-to-date version from Melpa

  :config
  ;; Settings
  ;;; General
  ;; Disable start-up screen (splash)
  ;; (Can set to `t` to enable, and can set `proof-splash-time`
  ;; to number  of seconds you want it displayed.)
  (setopt proof-splash-enable nil)
  ;; Don't use electric terminator mode, i.e., don't automatically
  ;; send command to proof assistant upon entering terminator (`.` in Easycrypt)
  ;; (Can set to `t` to enable.)
  (setopt proof-electric-terminator-enable nil)
  ;; Enable browsable history of responses (i.e., allow for browsing
  ;; through response/goal history without undoing proof steps)
  ;; (Can set to `nil` to disable.)
  (setopt proof-keep-response-history t)
  ;; Disable automatically collapsing/folding proofs as they are completed.
  ;; In any case, can fold/unfold completed proofs using `pg-toggle-visibility`,
  ;; bound to `C-c v` (i.e., `Control + c` followed by `v`) by default.
  ;; (Can set to `t` to enable.)
  (setopt proof-disappearing-proofs nil)

  ;;; EasyCrypt
  ;; Disable indentation in EasyCrypt scripts
  ;; (Can set to `t` to enable, but don't.)
  (setopt easycrypt-script-indent nil)
  ;; Disable formatting for newlines after each command.
  ;; (Can set to `t` to enable, but don't.)
  (setopt easycrypt-one-command-per-line nil))


;;; EasyCrypt Extension
;;; See
(use-package easycrypt-ext
  :ensure nil ; Provided locally

  :hook (easycrypt-mode . ece-setup)

  :init

  ;; Enable enhanced (but still ad-hoc) indentation support for EasyCrypt.
  ;; By default, aligns a line to the first non-blank preceding line.
  ;; Roughly speaking, indents (by `tab-width`) if (1) inside an code/expression enclosure
  ;; (e.g., between ( and ), [ and ], or { and }), or (2) inside an
  ;; unfinished specification (e.g., lemma statement that wasn't yet terminated with a `.`).
  ;; Also, automatically de-indents upon insertion of (1) a }, ), or ] if it is the first char
  ;; on a line and closes an enclosure (opened with {, (, or [), or (2) a . if it is preceded by
  ;; `proof` or `qed`.
  ;; Can disable by setting to `nil`.
  (setopt ece-enable-indentation t)
  ;; Enable suggested keybindings for indentation-related commands in EasyCrypt.
  ;; This makes the tab key perform "basic" indentation, as you would normally expect
  ;; in most other editors. Specifically, it simply inserts a tab character (of `tab-width`).
  ;; Correspondingly, pressing the tab key while holding Shift does the opposite (but
  ;; de-indents the whole line if there is no white-space to remove at point).
  ;; When having a region selected, these will insert (resp. remove) a tab
  ;; at the beginning of each line in the region, and furthermore select the full region.
  ;; The original command bound to the tab key (i.e., `indent-for-tab-command`) is
  ;; still bound to `M-i` (i.e., `Meta + i`).
  ;; Further, binds the return key (i.e., enter) to the `newline-and-indent` command
  ;; which, well, inserts a newline and indents it. Normally, return is bound to `newline`,
  ;; and `electric-indent-mode` indents both the current line and newline, which our
  ;; indentation is not consistent enough for (I think, feel free to try out).
  ;; The command only inserting a newline without indenting (`newline`) is bound
  ;; to `S-<return>`, i.e., pressing return while holding the Shift key.
  ;; Can disable by setting to `nil`.
  ;; (Even when disabled, can still bind any of these commands to keys of your choice)
  (setopt ece-enable-indentation-keybindings t)
  ;; Enable completion for EasyCrypt keywords (depends on `cape`, see above).
  ;; This essentially adds all keywords in EasyCrypt to the `cape-keyword-list` (but
  ;; only for EasyCrypt mode, of course), after which the `cape-keyword`
  ;; completion-at-point function will return completions for these keywords.
  ;; However, to actually use this, you will need to tell Emacs you want to use
  ;; `cape-keyword` as a completion-at-point function (by, e.g., locally adding it to
  ;; `completion-at-point-functions`). This is not done here, as this
  ;; minimal intialization file tries to not violate or overwrite any
  ;; already existing settings you may have w.r.t. completions.
  ;; For a complete setup, see the more elaborate example initialization files provided.
  ;; Can disable by setting to `nil`.
  (setopt ece-enable-keywords-completion t)
  ;; Enable code templates (snippets) for EasyCrypt (depends on `tempel`, see above).
  ;; This essentially loads all templates in `easycrypt-ext-templates.eld` into `tempel`,
  ;; allowing you to insert, expand, or complete templates using
  ;; `tempel-insert`, `tempel-expand`, or `tempel-complete`, respectively.
  ;; You can bind these commands to keybindings, but also use `tempel-expand` and
  ;; `tempel-complete` as completion-at-point functions. As for `cape-keyword`
  ;; (see `ece-enable-keywords-completion` above), we don't set this up to not intrude on
  ;; any of your already-existing settings for completions.
  ;; For a complete setup, see the more elaborate example initialization files provided.
  ;; Can disable by setting to `nil`.
  (setopt ece-enable-templates t)
  ;; Enable keybindings for inserting EasyCrypt (regular) templates (depends on `tempel, see above).
  ;; Specifically, this gives you a whole map of (keybindings inserting) templates that you can
  ;; access via `C-c l t` (i.e., `Control + c`, followed by `l`, followed by `t`).
  ;; For example, `C-c l t e` will insert a nicely formatted `equiv` expression.
  ;; Can disable by setting to `nil`.
  ;; (Even when disabled, can bind this map to any other keybinding. The map's variable name is
  ;; `ece-template-map` with prefix commmand `'ece-template-map-prefix`)
  (setopt ece-enable-templates-keybindings t)
  ;; Enable informative code templates for EasyCrypt (depends on `tempel`, see above).
  ;; Analogous to `ece-enable-templates`, but for the (informative) templates defined in
  ;; `easycrypt-ext-templates-info.eld`.
  (setopt ece-enable-templates-info t)
  ;; Enable informative code templates for EasyCrypt (depends on `tempel`, see above).
  (setopt ece-enable-auxiliary-functionality-keybindings t)
  ;; Disable EasyCrypt theme extension (depends on `doom-themes`, see above).
  ;; Can be set to 'dark, 'light, or nil (for a dark, a light, or no theme, respectively.)
  ;; Note that, in Emacs, themes are applied globally, i.e., to all buffers in the current
  ;; Emacs session. Thus, if you don't want your theme to be overwritten in other buffers
  ;; (or at all), then you can keep this disabled (by setting to `nil`).
  ;; (The themes should also look good in most other situations, though, so you might
  ;; try it out.)
  (setopt ece-enable-theme nil)

  :config
  ;; Enable mode to make use of repeat maps, which allow you to
  ;; repeat certain commands quickly after issuing them once.
  ;; Used for processing/undoing proof steps, and browsing through buffer history.
  ;; Note that this is a global (minor) mode, i.e., it will apply to all
  ;; buffers managed by the current Emacs session. Thus, if you don't want
  ;; this behavior in other buffers (in the same session as you are doing EasyCrypt),
  ;; then you can disable this by changing the 1 to -1 (or remove it the whole form).
  (repeat-mode 1))
