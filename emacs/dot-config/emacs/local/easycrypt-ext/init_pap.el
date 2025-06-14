;; -*- lexical-binding: t -*-
;; Environment
(defconst ECE_DIR (file-name-as-directory (file-name-concat user-emacs-directory "local/easycrypt-ext/"))
  "Directory where `easycrypt-ext` package is located. By default it is the
local/easycrypt-ext/ directory, relative to your emacs configuration directory.
You can find this directory by launching Emacs, pressing `C-h v' (i.e., `Control
+ h' followed by `v'), typing `user-emacs-directory', and press Return (i.e.,
Enter). You can replace the above `(file-name-as-directory (file-name-concat ...))'
form with a string containing the absolute path to the directory as well." )

;;; Add ECE_DIR to the load path, so we can, well, load it
(add-to-list 'load-path ECE_DIR)

;; Package system
;;; Load package system
(require 'package)

;;; Add MELPA package archive
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Initialize package system
(package-initialize)

;;; Install `use-package' if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Refresh package contents if still needed
(unless package-archive-contents
  (package-refresh-contents))

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
  ;; CONSIDER
  ;; Let completion pop-up show up automatically while you type
  ;; (setopt corfu-auto t) ; make pop-up automatic
  ;; (setopt corfu-on-exact-match nil) ; don't automatically complete on single match

  :config
  ;; CONSIDER
  ;; Change completion keybindings to not use up/down arrow and return/enter
  ;; for performing actions in the pop-up. This may especially be
  ;; worthwhile if you have the pop-up show up automatically.
  ;; (keymap-unset corfu-map "RET") ; Don't use return/enter for inserting a completion candidate
  ;; (keymap-unset corfu-map "<up>") ; Don't use up arrow for going up in the pop-up
  ;; (keymap-unset corfu-map "<remap> <previous-line>") ; Don't use default "previous line command" for going up in the pop-up
  ;; (keymap-unset corfu-map "<down>") ; Don't use down arrow for going down in the pop-up
  ;; (keymap-unset corfu-map "<remap> <next-line>") ; Don't use default "next line command" for going down in the pop-up
  ;; (keymap-set corfu-map "C-p" #'corfu-previous) ; Example: Use `Control + p' for going up in the pop-up
  ;; (keymap-set corfu-map "C-n" #'corfu-next) ; Example: Use `Control + n' for going down in the pop-up
  ;; (keymap-set corfu-map "C-v" #'corfu-insert) ; Example: Use `Control + v' for inserting a completion candidate
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

  :hook ((easycrypt-mode . easycrypt-ext-mode)
         (easycrypt-goals-mode . easycrypt-ext-goals-mode)
         (easycrypt-response-mode . easycrypt-ext-response-mode))

  :init
  ;; Enable enhanced (but still ad-hoc) indentation for EasyCrypt.
  ;; See documentation of indentation-related functions in `easycrypt-ext.el'
  ;; for details on the indentation behavior (a good starting point is `ece--indent-level').
  ;; Can disable by setting to `nil`.
  (setopt ece-enable-indentation t)
  ;; Enable completion for EasyCrypt keywords (depends on `cape', see above).
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
  (setopt ece-enable-keyword-completion t)
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
  ;; Enable informative code templates for EasyCrypt (depends on `tempel', see above).
  ;; Analogous to `ece-enable-templates', but for the (informative) templates defined in
  ;; `easycrypt-ext-templates-info.eld'.
  (setopt ece-enable-templates-info t)

  :config
  ;; Enable mode to make use of repeat maps, which allow you to
  ;; repeat certain commands quickly after issuing them once.
  ;; Used for processing/undoing proof steps, and browsing through buffer history.
  ;; Note that this is a global (minor) mode, i.e., it will apply to all
  ;; buffers managed by the current Emacs session. Thus, if you don't want
  ;; this behavior in other buffers (in the same session as you are doing EasyCrypt),
  ;; then you can disable this by changing the 1 to -1 (or remove it altogether).
  (repeat-mode 1))
