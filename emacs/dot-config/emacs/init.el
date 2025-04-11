;; -*- lexical-binding: t -*-
;; init.el
;; Environment
;;; Config
(defconst EMACS_CONFIG_DIR (file-name-as-directory
                      (if (getenv "XDG_CONFIG_HOME")
                                (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/")
                              user-emacs-directory))
  "Directory where Emacs configuration is stored.")

(defconst THEMES_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "themes/"))
  "Directory where (custom) themes are stored.")

(defconst FUNCS_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "funcs/"))
  "Directory where (custom) functions/functionalities are defined.")

(defconst TEMPLATES_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "templates/"))
  "Directory where (custom) templates are defined.")

(defconst MISC_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "misc/"))
  "Directory where (custom) miscellaneous configuration/settings are stored.")

(defconst CUSTOM_FILE (file-name-concat MISC_DIR "custom-set.el")
  "File where (automatically generated) customization settings are stored.")

;;; Data
(defconst EMACS_DATA_DIR (file-name-as-directory
                       (if (getenv "XDG_DATA_HOME")
                              (file-name-concat (getenv "XDG_DATA_HOME") "emacs/")
                            user-emacs-directory))
  "Directory where (additional) Emacs data is stored.")

(defconst BACKUPS_DIR (file-name-as-directory (file-name-concat EMACS_DATA_DIR "backups/"))
  "Directory where (automatically generated) backup files are stored.")

;;; Cache
(defconst EMACS_CACHE_DIR (file-name-as-directory
                         (if (getenv "XDG_CACHE_HOME")
                               (file-name-concat (getenv "XDG_CACHE_HOME") "emacs/")
                             user-emacs-directory))
  "Directory where Emacs cache is stored.")

(defconst AUTOSAVES_DIR (file-name-as-directory (file-name-concat EMACS_CACHE_DIR "autosaves/"))
  "Directory where auto-save files are stored.")

(defconst LOCKS_DIR (file-name-as-directory (file-name-concat EMACS_CACHE_DIR "locks/"))
  "Directory where lock files are stored.")


;; Bootstrap
;;; Directories
(unless (file-directory-p THEMES_DIR)
  (make-directory THEMES_DIR t))

(unless (file-directory-p FUNCS_DIR)
  (make-directory FUNCS_DIR t))

(unless (file-directory-p TEMPLATES_DIR)
  (make-directory TEMPLATES_DIR t))

(unless (file-directory-p MISC_DIR)
  (make-directory MISC_DIR t))

(unless (file-directory-p BACKUPS_DIR)
  (make-directory BACKUPS_DIR t))

(unless (file-directory-p AUTOSAVES_DIR)
  (make-directory AUTOSAVES_DIR t))

(unless (file-directory-p LOCKS_DIR)
  (make-directory LOCKS_DIR t))

;;; Custom file
(unless (file-exists-p CUSTOM_FILE)
  (make-empty-file CUSTOM_FILE))
(setopt custom-file CUSTOM_FILE)
(load CUSTOM_FILE)

;;; Load path/pointers
(add-to-list 'load-path FUNCS_DIR)
(add-to-list 'load-path MISC_DIR)
(setopt custom-theme-directory THEMES_DIR)

;;; Backups
(setopt backup-directory-alist `((".*" . ,BACKUPS_DIR))
        make-backup-files t
        vc-make-backup-files nil
        version-control t
        kept-old-versions 2
        kept-new-versions 3
        delete-old-versions t
        backup-by-copying nil
        backup-by-copying-when-linked t
        backup-by-copying-when-mismatch t)

;;; Auto-saves
(setopt auto-save-file-name-transforms `((".*" ,(file-name-concat AUTOSAVES_DIR "\\1") t))
        auto-save-visited-file-name nil
        auto-save-interval 50
        auto-save-timeout 20
        auto-save-default t
        delete-auto-save-files t
        auto-save-list-file-prefix nil)

;;; Locks
(setopt lock-file-name-transforms `((".*" ,(file-name-concat LOCKS_DIR "\\1") t))
        create-lockfiles t)

;;; Custom constants and functions/functionalities (without package dependencies)
(require 'custom-consts)
(require 'func-frames)
(require 'func-modes)
(require 'func-utils)

;;; Package system
(require 'package)

(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu" . 10)
                                     ("nongnu" . 5)
                                     ("melpa" . 1)))

(package-initialize)


;; Settings (general/UI)
;;; Launching
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message "// This buffer is for text that is not saved, and for Lisp evaluation \\\\")

;;; Frames/Windows
(setopt frame-resize-pixelwise t)
(setopt window-resize-pixelwise nil)

(setopt indicate-buffer-boundaries nil)
(setopt indicate-empty-lines nil)

(setopt switch-to-buffer-obey-display-actions t)
(setopt uniquify-buffer-name-style 'forward)
(setopt highlight-nonselected-windows nil)

(line-number-mode 1)
(column-number-mode 1)

(unless (daemonp)
  (when (fboundp 'setup-a-global-frame)
    (setup-a-global-frame)))

;;; (Mini)Buffers
(setopt minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setopt read-extended-command-predicate #'command-completion-default-include-p)
(setopt enable-recursive-minibuffers t)

(setopt resize-mini-windows 'grow-only)

(setopt echo-keystrokes 0.02)

;;; Cursor
(setopt x-stretch-cursor nil)
(setopt blink-matching-paren t)
(setopt cursor-in-non-selected-windows nil)

(blink-cursor-mode -1)

;;; Scrolling/Mouse
(setopt hscroll-margin 2)
(setopt hscroll-step 1)
(setopt scroll-conservatively 10)
(setopt scroll-margin 0)
(setopt scroll-preserve-screen-position t)
(setopt auto-window-vscroll nil)
(setopt fast-but-imprecise-scrolling t)

(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)
(setopt mouse-yank-at-point t)
(setopt mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
(setopt mouse-wheel-scroll-amount-horizontal 2)

(pixel-scroll-precision-mode 1)

;;; Editor/interaction
(setopt indent-tabs-mode nil)
(setopt tab-width 2)
(setopt tab-always-indent nil)

(setopt word-wrap t)
(setopt truncate-lines t)
(setopt truncate-partial-width-windows nil)

(setopt kill-do-not-save-duplicates t)

(setopt use-dialog-box nil)
(setopt use-short-answers t)
(setopt confirm-nonexistent-file-or-buffer nil)

(setopt show-trailing-whitespace nil)

(setopt ring-bell-function #'ignore)
(setopt visible-bell nil)

(setopt hl-line-sticky-flag nil)
(setopt global-hl-line-sticky-flag nil)

(setopt shift-select-mode t)

;;; Miscellaneous
(setq-default bidi-display-reordering 'left-to-right)
(setopt bidi-paragraph-direction 'left-to-right)

(setopt sentence-end-double-space nil)
(setopt x-underline-at-descent-line nil)

(savehist-mode 1)
(recentf-mode 1)


;; Bindings (general)
;;; Translations
(keymap-set function-key-map "C-S-<iso-lefttab>" "C-<backtab>")
(keymap-set function-key-map "M-S-<iso-lefttab>" "M-<backtab>")
(keymap-set function-key-map "C-M-S-<iso-lefttab>" "C-M-<backtab>")

;;; Movement
(keymap-global-set "M-H" #'windmove-left)
(keymap-global-set "M-J" #'windmove-down)
(keymap-global-set "M-K" #'windmove-up)
(keymap-global-set "M-L" #'windmove-right)

(keymap-global-set "<left>" #'left-char)
(keymap-global-set "<right>" #'right-char)

(keymap-global-set "C-<left>" #'backward-word)
(keymap-global-set "C-<right>" #'forward-word)

(keymap-global-set "C-a" #'move-beginning-of-line)
(keymap-global-set "C-e" #'move-end-of-line)

(keymap-global-set "<up>" #'previous-line)
(keymap-global-set "<down>" #'next-line)

(keymap-global-set "C-<up>" #'backward-paragraph)
(keymap-global-set "C-<down>" #'forward-paragraph)

(keymap-global-set "<home>" #'beginning-of-buffer)
(keymap-global-set "<end>" #'end-of-buffer)

(keymap-global-set "C-<prior>" #'scroll-other-window-down)
(keymap-global-set "C-<next>" #'scroll-other-window)

(keymap-global-set "C-<home>" #'beginning-of-buffer-other-window)
(keymap-global-set "C-<end>" #'end-of-buffer-other-window)

(keymap-global-set "C-b" #'switch-to-prev-buffer)
(keymap-global-set "C-f" #'switch-to-next-buffer)

(keymap-global-set "C-p" #'backward-sexp)
(keymap-global-set "C-n" #'forward-sexp)

(keymap-global-set "C-S-p" #'backward-sentence)
(keymap-global-set "C-S-n" #'forward-sentence)

(keymap-global-set "C-w" #'other-window)

;;; Selection
(keymap-global-set "C-SPC" #'set-mark-command)

;;; Manipulation
;;;; Copying
(keymap-global-set "C-t" #'kill-ring-save)
(keymap-global-set "C-S-t" #'clipboard-kill-ring-save)

;;;; Killing
(keymap-global-set "M-<backspace>" #'backward-kill-word)
(keymap-global-set "M-<deletechar>" #'kill-word)

(keymap-global-set "M-e" #'kill-line)
(keymap-global-set "M-l" #'kill-whole-line)

(keymap-global-set "M-b" #'backward-kill-sentence)
(keymap-global-set "M-f" #'kill-sentence)

(keymap-global-set "M-p" #'backward-kill-sexp)
(keymap-global-set "M-n" #'kill-sexp)

(keymap-global-set "M-P" #'backward-kill-sentence)
(keymap-global-set "M-N" #'kill-sentence)

(keymap-global-set "M-t" #'kill-region)
(keymap-global-set "M-T" #'clipboard-kill-region)

(keymap-global-set "M-;" #'undo)
(keymap-global-set "M-/" #'undo-redo)

;;;; Yanking
(keymap-global-set "M-y" #'yank)
(keymap-global-set "M-Y" #'clipboard-yank)
(keymap-global-set "C-M-y" #'yank-pop)

;;;; Deleting
(keymap-global-set "M-D" #'delete-region)

;;; Management
;;;; Quitting
(defvar-keymap a-quit-map
  :doc "Keymap for quitting (optionally saving and/or restarting)"
  :prefix 'a-quit-map-prefix
  "q" #'save-buffers-kill-terminal
  "Q" #'save-buffers-kill-terminal-silent
  "e" #'save-buffers-kill-emacs
  "E" #'save-buffers-kill-emacs-silent
  "t" #'save-buffers-kill-terminal
  "T" #'save-buffers-kill-terminal-silent
  "r" #'save-buffers-restart-emacs
  "R" #'save-buffers-restart-emacs-silent)

(keymap-global-set "C-x q" 'a-quit-map-prefix)

;;;; Killing
(defvar-keymap a-kill-map
  :doc "Keymap for killing"
  :prefix 'a-kill-map-prefix
  "f" #'delete-frame
  "F" #'delete-other-frames
  "w" #'delete-window
  "W" #'delete-other-windows
  "C-w" #'delete-windows-on
  "b" #'kill-current-buffer
  "B" #'kill-some-buffers
  "C-b" #'kill-buffer)

(keymap-global-set "C-x k" 'a-kill-map-prefix)

;;;; Frames
(defvar-keymap a-frame-map
  :doc "Keymap for frame management"
  :prefix 'a-frame-map-prefix
  "k" #'delete-frame
  "K" #'delete-other-frames
  "u" #'undelete-frame
  "c" #'clone-frame
  "m" #'make-frame-command
  "f" #'find-file-other-frame
  "g" #'other-frame
  "s" #'suspend-frame)

(keymap-global-set "C-x ^" 'a-frame-map-prefix)

;;;; Windows
(defvar-keymap a-window-map
  :doc "Keymap for window management"
  :prefix 'a-window-map-prefix
  "-" #'shrink-window
  "+" #'enlarge-window
  ">" #'shrink-window-horizontally
  "<" #'enlarge-window-horizontally
  "b" #'balance-windows
  "k" #'delete-window
  "K" #'delete-other-windows
  "C-k" #'delete-windows-on
  "s" #'split-window-horizontally
  "S" #'split-window-vertically
  "f" #'fit-window-to-buffer
  "g" #'other-window
  "t" #'tear-off-window
  "<left>" #'windmove-left
  "<down>" #'windmove-down
  "<up>" #'windmove-up
  "<right>" #'windmove-right)

(keymap-global-set "C-x w" 'a-window-map-prefix)

;;;; Buffers
(defvar-keymap a-buffer-map
  :doc "Keymap for buffer management"
  :prefix 'a-buffer-map-prefix
  "k" #'kill-current-buffer
  "K" #'kill-some-buffers
  "C-k" #'kill-buffer
  "g" #'switch-to-buffer
  "G" #'switch-to-buffer-other-window
  "M-g" #'switch-to-buffer-other-frame
  "m" #'switch-to-minibuffer
  "s" #'save-buffer
  "S" #'save-some-buffers
  "r" #'revert-buffer
  "R" #'revert-buffer-quick)

(keymap-global-set "C-x b" 'a-buffer-map-prefix)

;;; Navigation/searching
(keymap-global-set "C-x f" #'find-file)
(keymap-global-set "C-x F" #'find-file-other-window)
(keymap-global-set "C-x C-f" #'find-file-other-frame)
(keymap-global-set "C-x C-r" #'recentf-open)

;;;; Goto map
(defvar-keymap a-goto-map
  :doc "Keymap for navigation"
  :prefix 'a-goto-map-prefix
  "b" #'switch-to-buffer
  "B" #'switch-to-buffer-other-window
  "C-b" #'switch-to-buffer-other-frame
  "c" #'goto-char
  "f" #'other-frame
  "l" #'goto-line
  "i" #'imenu
  "w g" #'other-window
  "w h" #'windmove-left
  "w j" #'windmove-down
  "w k" #'windmove-up
  "w l" #'windmove-right)

(keymap-global-set "C-c g" 'a-goto-map-prefix)

;;;; Search map
(defvar-keymap a-search-map
  :doc "Keymap for searching"
  :prefix 'a-search-map-prefix
  "f" #'find-file
  "F" #'find-file-other-window
  "C-f" #'find-file-other-frame
  "r" #'recentf-open
  "R" #'find-file-read-only
  "i b" #'isearch-backward
  "i f" #'isearch-forward
  "i s" #'isearch-forward-symbol
  "i r" #'isearch-query-replace
  "i w" #'isearch-forward-word
  "i ." #'isearch-forward-symbol-at-point)

(keymap-global-set "C-c s" 'a-search-map-prefix)


;; Packages
;;; General
(setopt use-package-always-ensure nil
        use-package-always-defer nil
        use-package-always-pin nil
        use-package-always-demand nil)

(when (and (not (package-installed-p 'vc-use-package)) (< emacs-major-version 30))
  (package-vc-install
   '(vc-use-package :url "https://github.com/slotThe/vc-use-package"
                    :vc-backend Git))
  (require 'vc-use-package))

;;; Base/Built-in
(use-package isearch
  :init
  (setopt search-exit-option t)
  (setopt isearch-repeat-on-direction-change t
          isearch-lazy-count t
          isearch-lax-whitespace t
          isearch-allow-scroll 'unlimited
          isearch-allow-motion t)
  (setopt lazy-count-prefix-format nil
          lazy-count-suffix-format " [%s of %s]")
  :bind (:map isearch-mode-map
              ("C-w" . nil)
              ("C-v" . #'isearch-exit)
              ("C-t" . #'isearch-yank-word-or-char)
              ("M-y" . #'isearch-yank-kill)))

(use-package dired
  :init
  (setopt dired-listing-switches (purecopy "-lahF")
          dired-maybe-use-globstar t
          dired-mouse-drag-files t
          dired-always-read-filesystem t
          dired-mark-region 'file
          dired-movement-style 'bounded
          dired-auto-revert-buffer #'dired-directory-changed-p
          dired-recursive-deletes 'top
          dired-switches-in-mode-line 'as-is
          dired-recursive-copies 'top)
  :bind (:map dired-mode-map
              ("C-v" . #'dired-find-file)
              ("RET" . #'dired-find-file)
              ("<return>" . #'dired-find-file)
              ("C-o" . #'dired-display-file)
              ("C-<up>" . #'dired-prev-dirline)
              ("C-<down>" . #'dired-next-dirline)
              ("C-p" . #'dired-prev-marked-file)
              ("C-n" . #'dired-next-marked-file)
              ("C-<prior>" . #'dired-up-directory)))

;;; Helpers
(use-package which-key
  :ensure t
  :demand t
  :bind (:map which-key-mode-map
              ("C-x <f3>" . which-key-C-h-dispatch))
  :hook (which-key-init-buffer . setup-a-mini-mix-mode)
  :config
  (setopt which-key-idle-delay 0.5
          which-key-popup-type 'side-window
          which-key-side-window-location 'bottom
          which-key-side-window-max-height 0.25
          which-key-max-description-length 0.20
          which-key-add-column-padding 2
          which-key-show-prefix 'left
          which-key-show-remaining-keys t
          which-key-preserve-window-configuration t
          which-key-sort-uppercase-first nil
          which-key-sort-order 'which-key-key-order-alpha
          which-key-use-C-h-commands nil
          which-key-show-early-on-C-h nil
          which-key-paging-prefixes '("C-x")
          which-key-paging-key "<f3>")
  (which-key-mode 1))

(use-package easy-kill
  :ensure t
  :demand t
  :bind (("<remap> <kill-ring-save>" . #'easy-kill)
         ("<remap> <mark-word>" . #'easy-mark)
         :map easy-kill-base-map
         ("<remap> <kill-ring-save>" . #'easy-kill-cycle)
         ("<" . #'easy-kill-shrink)
         (">" . #'easy-kill-expand)
         ("a" . #'easy-kill-append)
         ("k" . #'easy-kill-region)
         ("r" . #'easy-delete-region)
         ("q" . #'easy-kill-abort)
         ("p" . #'easy-kill-exchange-point-and-mark))
  :config
  (setopt easy-kill-alist '((?w word           " ")
                            (?s sexp           "\n")
                            (?h list           "\n")
                            (?f filename       "\n")
                            (?d defun          "\n\n")
                            (?D defun-name     " ")
                            (?l line           "\n")
                            (?b buffer-file-name)))
  (setopt easy-kill-cycle-ignored '(filename defun-name buffer-file-name))
  (setopt easy-kill-try-things '(url email word line))
  (setopt easy-mark-try-things '(url email word sexp)))

(use-package expand-region
  :ensure t
  :pin melpa
  :bind ("C->" . #'er/expand-region)
  :config
  (setopt expand-region-fast-keys-enabled t)
  (setopt expand-region-contract-fast-key "<")
  (setopt expand-region-reset-fast-key "r"))

(use-package undo-tree
  :ensure t
  :demand t
  :bind (:map undo-tree-map
              ("<remap> <undo-redo>" . undo-tree-redo)
              :map undo-tree-visualizer-mode-map
              ("C-f" . nil)
              ("C-b" . nil)
              ("h" . #'undo-tree-visualizer-scroll-left)
              ("j" . #'undo-tree-visualizer-scroll-down)
              ("k" . #'undo-tree-visualizer-scroll-up)
              ("l" . #'undo-tree-visualizer-scroll-right)
              :map undo-tree-visualizer-selection-mode-map
              ("C-f" . nil)
              ("C-b" . nil)
              ("C-v" . #'undo-tree-visualizer-set))
  :config
  ;; Create and store undo history directory
  (defconst UNDO_DIR (file-name-as-directory (file-name-concat EMACS_DATA_DIR "undos/"))
    "Directory where (automatically generated) undo (history) files are stored.")
  (unless (file-directory-p UNDO_DIR)
    (make-directory UNDO_DIR t))
  (setopt undo-tree-history-directory-alist `(("." . ,UNDO_DIR)))
  (setopt undo-tree-auto-save-history t)

  (setopt undo-tree-mode-lighter " UT")
  (setopt undo-tree-incompatible-major-modes '(term-mode image-mode doc-view-mode pdf-view-mode))
  (setopt undo-tree-enable-undo-in-region t)
  (setopt undo-tree-visualizer-diff t)

  (global-undo-tree-mode 1))

(use-package marginalia
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
              ("C-r" . marginalia-cycle))
  :config
  (setopt marginalia-field-width 100)
  (marginalia-mode 1))

(use-package jinx
  :ensure t
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode)
         (conf-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)
         :map jinx-overlay-map
         ("C-n" . jinx-next)
         ("C-p" . jinx-previous)
         :map jinx-repeat-map
         ("C-n" . jinx-next)
         ("C-p" . jinx-previous)
         :map jinx-correct-map
         ("C-n" . jinx-next)
         ("C-p" . jinx-previous))
  :config
  (setopt jinx-languages "en_US")
  (setopt jinx-include-faces '((prog-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (conf-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (yaml-mode . conf-mode)
                               (yaml-ts-mode . conf-mode))))


;;; Completion
(use-package orderless
  :ensure t
  :demand t
  :config
  (setopt orderless-matching-styles (list #'orderless-literal #'orderless-flex #'orderless-regexp)
          orderless-smart-case t
          orderless-expand-substring 'prefix)
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-define-completion-style orderless-flex-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-flex)))
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal))))

(use-package vertico
  :ensure t
  :demand t
  :bind (:map vertico-map
              ("C-o" . vertico-insert)
              ("C-v" . vertico-exit)
              ("C-M-v" . vertico-exit-input)
              ("TAB" . minibuffer-complete)
              ("<tab>" . minibuffer-complete)
              ("C-?" . minibuffer-completion-help)
              ("C-p" . previous-history-element)
              ("C-n" . next-history-element)
              ("C-<up>" . previous-history-element)
              ("C-<down>" . next-history-element)
              ("<prior>" . vertico-previous-group)
              ("<next>" . vertico-next-group)
              ("<home>" . vertico-first)
              ("<end>" . vertico-last))
  :config
  (setopt vertico-count 15
          vertico-preselect 'first
          vertico-scroll-margin 2
          vertico-cycle nil)
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :ensure nil ; included with vertico
  :bind (:map vertico-map
              ("C-d" . vertico-directory-enter)
              ("<backspace>" . vertico-directory-delete-char)
              ("M-<backspace>" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :after vertico
  :ensure nil ; included with vertico
  :config
  (keymap-set vertico-mouse-map "<mouse-1>" (vertico-mouse--click "C-v"))
  (keymap-set vertico-mouse-map "<mouse-3>" (vertico-mouse--click "C-o"))
  (vertico-mouse-mode 1))

(use-package corfu
  :ensure t
  :demand t
  :bind (:map corfu-map
              ;; Always
              ("C-o" . corfu-complete)
              ("TAB" . corfu-complete)
              ("<tab>" . corfu-complete)
              ("C-v" . corfu-send)
              ("S-SPC" . corfu-insert-separator)
              ;; Auto mode
              ("<remap> <beginning-of-buffer>" . nil)
              ("<remap> <end-of-buffer>" . nil)
              ("<remap> <previous-line>" . nil)
              ("<remap> <next-line>" . nil)
              ("<remap> <move-beginning-of-line>" . nil)
              ("<remap> <move-end-of-line>" . nil)
              ("RET" . nil)
              ("<up>" . nil)
              ("<down>" . nil)
              ("C-p" . corfu-previous)
              ("C-n" . corfu-next)
              ("C-<prior>" . corfu-scroll-up)
              ("C-<next>" . corfu-scroll-down)
              ("C-<home>" . corfu-first)
              ("C-<end>" . corfu-last))
  :config
  (setopt corfu-count 10
          corfu-scroll-margin 2
          corfu-min-width 15
          corfu-max-width 80
          corfu-cycle nil
          corfu-on-exact-match 'insert
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match 'separator
          corfu-left-margin-width 0.5
          corfu-right-margin-width 0.5
          corfu-bar-width 0.25
          corfu-auto-prefix 3
          corfu-auto-delay 0.25
          ;; Auto mode
          corfu-preselect 'valid
          corfu-auto t)
  (setopt text-mode-ispell-word-completion nil)
  (global-corfu-mode 1))

(use-package cape
  :ensure t
  :demand t
  :bind ("C-c e" . cape-prefix-map)
  :config
  (setopt cape-dict-limit 50
          cape-dabbrev-check-other-buffers #'cape--buffers-major-mode
          cape-file-prefix '("file:" "f:")
          cape-file-directory-must-exist t)

  (defalias 'cape-abbrev-prefix-2 (cape-capf-prefix-length #'cape-abbrev 2))
  (defalias 'cape-dabbrev-prefix-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (defalias 'cape-line-prefix-3 (cape-capf-prefix-length #'cape-line 3))
  (defalias 'cape-dict-prefix-2 (cape-capf-prefix-length #'cape-dict 2))
  (defalias 'cape-keyword-prefix-2 (cape-capf-prefix-length #'cape-keyword 2))
  (defalias 'cape-file-prefix-2 (cape-capf-prefix-length #'cape-file 2))
  (defalias 'cape-history-prefix-2 (cape-capf-prefix-length #'cape-history 2))

  (defun setup-a-cape-text-mode ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-abbrev-prefix-2
                                       #'cape-dict-prefix-2
                                       #'cape-dabbrev-prefix-2))))
  (defun setup-a-cape-mix-mode ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-abbrev-prefix-2
                                       #'cape-keyword-prefix-2 #'cape-dict-prefix-2
                                       #'cape-dabbrev-prefix-2))))
  (defun setup-a-cape-code-mode ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-abbrev-prefix-2
                                       #'cape-keyword-prefix-2
                                       #'cape-dabbrev-prefix-2))))
  (defun setup-a-cape-minibuffer ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-abbrev-prefix-2
                                       #'cape-history-prefix-2 #'cape-file-prefix-2
                                       #'cape-dabbrev-prefix-2))))

  ;; Global
  (add-hook 'completion-at-point-functions (cape-capf-super #'cape-abbrev-prefix-2 #'cape-dabbrev-prefix-2))

  ;; Local
  (add-hook 'text-mode-hook #'setup-a-cape-text-mode)
  (add-hook 'tex-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'TeX-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'conf-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'prog-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'minibuffer-setup-hook #'setup-a-cape-minibuffer))

(use-package cape-keyword
  :after cape
  :ensure nil ; provided by cape
  :config
  (add-to-list 'cape-keyword-list (cons 'easycrypt-mode ec-keyword-list)))

(use-package tempel
  :ensure t
  :demand t
  :bind (:map tempel-map
              ("<remap> <beginning-of-buffer>" . nil)
              ("<remap> <end-of-buffer>" . nil)
              ("<remap> <backward-paragraph>" . nil)
              ("<remap> <forward-paragraph>" . nil)
              ("C-<backtab>" . tempel-previous)
              ("C-<tab>" . tempel-next)
              ("<prior>" . tempel-previous)
              ("<next>" . tempel-next)
              ("<home>" . tempel-beginning)
              ("<end>" . tempel-end)
              ("M-k" . tempel-kill)
              ("C-v" . tempel-done)
              ("C-q" . tempel-abort))
  :config
  (defvar-keymap a-tempel-map
    :doc "Keymap for tempel (outside templates)"
    :prefix 'a-tempel-map-prefix
    "c" #'tempel-complete
    "e" #'tempel-expand
    "i" #'tempel-insert)
  (keymap-global-set "C-c t" 'a-tempel-map-prefix)
  ;; Create and store templates directory
  (defconst TEMPEL_DIR (file-name-as-directory (file-name-concat TEMPLATES_DIR "tempel/"))
    "Directory where tempel templates are stored.")
  (unless (file-directory-p TEMPEL_DIR)
    (make-directory TEMPEL_DIR t))
  (setopt tempel-path (file-name-concat TEMPEL_DIR "*.eld"))
  (global-tempel-abbrev-mode 1))

;;; Actions
(use-package crux
  :ensure t
  :demand t
  :pin melpa
  :bind (("<remap> <kill-line>" . crux-smart-kill-line)
         ("<remap> <kill-whole-line>" . crux-kill-whole-line)
         ("<remap> <move-beginning-of-line>" . crux-move-beginning-of-line)
         ("M-a" . crux-kill-line-backwards)
         ("M-d" . crux-duplicate-current-line-or-region)
         :map a-kill-map
         ("B" . crux-kill-other-buffers)
         :map a-buffer-map
         ("c" . crux-create-scratch-buffer)
         ("d" . crux-kill-buffer-truename)
         ("K" . crux-kill-other-buffers)
         ("p" . crux-switch-to-previous-buffer)
         :map a-window-map
         ("e" . crux-transpose-windows))
  :config
  (defvar-keymap a-crux-map
    :doc "Keymap for crux actions"
    :prefix 'a-crux-map-prefix
    "RET" #'crux-smart-open-line
    "<return>" #'crux-smart-open-line
    "S-RET" #'crux-smart-open-line-above
    "S-<return>" #'crux-smart-open-line-above
    "c" #'crux-copy-file-preserve-attributes
    "C" #'crux-cleanup-buffer-or-region
    "d" #'crux-duplicate-current-line-or-region
    "D" #'crux-duplicate-and-comment-current-line-or-region
    "e" #'crux-eval-and-replace
    "f f" #'crux-recentf-find-file
    "f d" #'crux-recentf-find-directory
    "f i" #'crux-find-user-init-file
    "f c" #'crux-find-user-custom-file
    "f s" #'crux-find-shell-init-file
    "f l" #'crux-find-current-directory-dir-locals-file
    "F" #'crux-recentf-find-file
    "k" #'crux-delete-file-and-buffer
    "i" #'crux-indent-rigidly-and-copy-to-clipboard
    "j" #'crux-top-join-line
    "J" #'crux-kill-and-join-forward
    "r" #'crux-rename-file-and-buffer
    "s" #'crux-sudo-edit
    "u" #'crux-view-url
    "x" #'crux-visit-term-buffer
    "X" #'crux-visit-shell-buffer
    "C-u" #'crux-upcase-region
    "C-l" #'crux-downcase-region
    "C-c" #'crux-captialize-region)
  (keymap-global-set "C-c x" 'a-crux-map-prefix))

(use-package ace-window
  :ensure t
  :bind (("C-w" . ace-window)
         ("C-S-w" . other-window)
         :map a-goto-map
         ("W" . ace-window))
  :config
  (setopt aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (setopt aw-scope 'visible
          aw-minibuffer-flag nil
          aw-ignore-current nil
          aw-background t
          aw-dispatch-always t)
  (setq-default aw-dispatch-alist
                '((?k aw-delete-window "Kill window")
                  (?K delete-other-windows "Kill other windows")
                  (?s aw-swap-window "Swap buffers between windows")
                  (?m aw-move-window "Move current buffer to window")
                  (?y aw-copy-window "Copy current buffer to window")
                  (?g aw-switch-buffer-in-window "Select buffer in window")
                  (?G aw-switch-buffer-other-window "Select buffer in other window")
                  (?r aw-flip-window)
                  (?x aw-execute-command-other-window "Execute command in other window")
                  (?w aw-split-window-fair "Split window fairly")
                  (?h aw-split-window-horz "Split window horizontally")
                  (?v aw-split-window-vert "Split window vertically")
                  (?t aw-transpose-frame "Transpose frames")
                  (?? aw-show-dispatch-help))))

(use-package avy
  :ensure t
  :pin melpa
  :bind (("C-j" . avy-goto-char)
         :map isearch-mode-map
         ("C-j" . nil)
         ("C-S-j" . #'avy-isearch)
         :map minibuffer-mode-map
         ("C-j" . nil))
  :config
  (defvar-keymap an-avy-map
    :doc "Keymap for avy"
    :prefix 'an-avy-map-prefix
    "c" #'avy-goto-char
    "C" #'avy-goto-char-2
    "w" #'avy-goto-word-1
    "W" #'avy-goto-word-0
    "s" #'avy-goto-subword-1
    "S" #'avy-goto-subword-0
    "l" #'avy-goto-line
    "C-l" #'avy-kill-ring-save-whole-line
    "C-t" #'avy-kill-ring-save-region
    "M-l" #'avy-kill-whole-line
    "M-t" #'avy-kill-region)
  (keymap-global-set "C-c a" 'an-avy-map-prefix)
  (setopt avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?r ?u ?i)
          avy-style 'at-full
          avy-all-windows t
          avy-case-fold-search t
          avy-single-candidate-jump nil)
  (setq-default avy-dispatch-alist '((?q . avy-action-kill-move)
                                     (?Q . avy-action-kill-stay)
                                     (?m . avy-action-mark)
                                     (?t . avy-action-copy)
                                     (?y . avy-action-yank)
                                     (?Y . avy-action-teleport)
                                     (?z . avy-action-zap-to-char)
                                     (?i . avy-action-ispell))))

(use-package consult
  :ensure t
  :demand t
  :bind (("C-l" . consult-line)
         ("C-;" . consult-goto-line)
         :map a-buffer-map
         ("g" . consult-buffer)
         ("G" . consult-buffer-other-window)
         ("M-g" . consult-buffer-other-frame)
         :map a-goto-map
         ("b" . consult-buffer)
         ("B" . consult-buffer-other-window)
         ("C-b" . consult-buffer-other-frame)
         ("e" . consult-compile-error)
         ("d" . consult-flymake)
         ("l" . consult-goto-line)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map a-search-map
         ("f" . find-file)
         ("F" . find-file-other-window)
         ("C-f" . consult-fd)
         ("M-f" . consult-find)
         ("r" . consult-recent-file)
         ("c" . consult-locate)
         ("g" . consult-ripgrep)
         ("G" . consult-git-grep)
         ("M-g" . consult-grep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("h" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s h" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :config
  (defvar-keymap a-consult-map
    :doc "Keymap for consult (misc)"
    :prefix 'a-consult-map-prefix
    "x" #'consult-mode-command
    "h" #'consult-history
    "k" #'consult-kmacro
    "m" #'consult-man
    "i" #'consult-info
    ":" #'consult-complex-command)
  (keymap-global-set "C-c c" 'a-consult-map-prefix)
  (setopt consult-narrow-key "C-<"
          consult-widen-key "C->")
  (setopt consult-async-refresh-delay 0.1
          consult-async-input-thottle 0.3
          consult-async-input-debounce 0.1
          consult-async-min-input 2)
  (setopt xref-show-xrefs-function #'consult-xref))

(use-package embark
  :ensure t
  :pin melpa
  :bind (("C-," . embark-act)
         ("C-." . embark-dwim)
         ("C-h C-b" . embark-bindings)
         :map embark-general-map
         ("t" . embark-copy-as-kill)
         ("C-o" . embark-select)
         :map embark-file-map
         ("F" . find-file-other-window)
         ("C-f" . find-file-other-frame)
         ("l" . find-file-literally)
         :map embark-library-map
         ("F" . find-library-other-window)
         ("C-f" . find-library-other-frame)
         :map embark-buffer-map
         ("g" . switch-to-buffer)
         ("G" . switch-to-buffer-other-window)
         ("M-g" . switch-to-buffer-other-frame)
         :map embark-identifier-map
         ("f" . xref-find-definitions)
         ("F" . xref-find-definitions-other-window)
         ("C-f" . xref-find-definitions-other-frame)
         :map embark-symbol-map
         ("f" . embark-find-definition)
         :map embark-package-map
         ("f" . describe-package)
         :map embark-become-file+buffer-map
         ("F" . find-file-other-window)
         ("B" . switch-to-buffer-other-window))
  :config
  (keymap-unset embark-general-map "SPC")
  (setopt embark-confirm-act-all t)
  (setq-default prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after embark
  :after consult)

;;; Tools
(use-package projectile
  :ensure t
  :demand t
  :init
  (setopt projectile-mode-line-prefix " Ptile"
          projective-keymap-prefix "C-c p")
  :bind ("C-c p" . projectile-command-map)
  :config
  ;; Create and store projectile known projects file
  (defconst PTILE_FILE (file-name-concat EMACS_DATA_DIR "projectile-bookmarks.eld")
    "File where projectile's known projects are stored.")
  (setopt projectile-known-projects-file PTILE_FILE)
  (setopt projectile-enable-caching t
          projectile-sort-order 'recently-active
          projectile-dirconfig-comment-prefix ?\#
          projectile-find-dir-includes-top-level t
          projectile-enable-idle-timer nil
          projectile-current-project-on-switch 'move-to-end)
  (projectile-mode 1))

(use-package diff-hl
  :ensure t
  :demand t
  :pin melpa
  :bind (:map diff-hl-command-map
              ("g" . diff-hl-diff-goto-hunk)
              ("r" . diff-hl-revert-hunk)
              ("p" . diff-hl-previous-hunk)
              ("n" . diff-hl-next-hunk)
              ("o" . diff-hl-show-hunk)
              ("C-p" . diff-hl-show-previous-hunk)
              ("C-n" . diff-hl-show-next-hunk)
              ("s" . diff-hl-stage-dwim))
  :config
  (setopt diff-hl-command-prefix (kbd "C-x v"))
  (setopt diff-hl-global-modes '(not term-mode image-mode doc-view-mode pdf-view-mode))
  (setopt diff-hl-update-async t)

  (setq-default diff-hl-lighter " DiffHL")
  (global-diff-hl-mode 1))

(use-package magit
  :ensure t
  :init
  (setopt magit-define-global-key-bindings nil)
  :bind (("C-x m" . magit-status)
         ("C-c m" . magit-dispatch)
         ("C-c f" . magit-file-dispatch)
         :map magit-mode-map
         ("C-v" . magit-visit-thing)
         ("C-t" . magit-copy-section-value)
         ("C-S-t" . magit-copy-buffer-revision)
         ("C-c C-t" . magit-copy-thing)
         ("C-w" . nil)
         ("M-w" . nil)
         ("C-c C-w" . nil)
         :map magit-diff-section-map
         ("C-v" . magit-diff-visit-worktree-file)
         ("C-j" . nil)
         :map magit-module-section-map
         ("C-v" . magit-submodule-visit)
         ("C-j" . nil)
         :map with-editor-mode-map
         ("C-c C-v" . with-editor-finish)
         ("C-c C-q" . with-editor-cancel))
  :config
  (setopt magit-verbose-messages t)
  (setopt magit-auto-revert-mode t
          magit-auto-revert-immediately t
          magit-auto-revert-tracked-only t
          auto-revert-use-notify t
          auto-revert-stop-on-user-input t
          auto-revert-verbose t
          auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (setopt magit-delete-by-moving-to-trash t)
  (setopt git-commit-major-mode #'log-edit-mode)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)
  (magit-wip-mode 1))

(use-package tex
  :ensure auctex
  :hook
  (Tex-language-en . (lambda () (jinx-languages "en_US")))
  (Tex-language-nl . (lambda () (jinx-languages "nl")))
  :config
  (setopt TeX-view-program-selection
          '(((output-dvi has-no-display-manager) "dvi2tty")
            ((output-dvi style-pstricks) "dvips and gv")
            (output-dvi "xdvi")
            (output-pdf "PDF Tools")
            (output-html "xdg-open")))
  (setopt TeX-file-line-error t
          TeX-display-help t
          TeX-PDF-mode t
          TeX-auto-save t
          TeX-parse-self t
          TeX-master nil
          TeX-save-query t
          TeX-auto-regexp-list 'TeX-auto-full-regexp-list
          TeX-auto-untabify t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package pdf-tools
  :ensure t
  :init
  (setopt pdf-tools-handle-upgrades nil)
  (pdf-loader-install)
  :hook
  ((pdf-view-mode . pdf-view-themed-minor-mode)
   (pdf-tools-enabled . (lambda ()
                          (keymap-set pdf-annot-edit-contents-minor-mode-map
                                      "C-c C-v"
                                      #'pdf-annot-edit-contents-commit)))
   (pdf-tools-enabled . (lambda ()
                          (keymap-set pdf-sync-minor-mode-map "<double-mouse-1>" nil))))
  :bind (:map pdf-view-mode-map
              ("q" . kill-this-buffer)
              ("<down>" . pdf-view-next-line-or-next-page)
              ("<up>" . pdf-view-previous-line-or-previous-page)
              ("n" . pdf-view-next-page-command)
              ("p" . pdf-view-previous-page-command)
              ("<next>" . pdf-view-next-page-command)
              ("<prior>" . pdf-view-previous-page-command)
              ("C-n" . pdf-view-scroll-down-or-next-page)
              ("C-p" . pdf-view-scroll-up-or-previous-page)
              ("SPC" . pdf-view-scroll-down-or-next-page)
              ("DEL" . pdf-view-scroll-up-or-previous-page)
              ("<backspace>" . pdf-view-scroll-up-or-previous-page)
              ("<end>" . pdf-view-last-page)
              ("<home>" . pdf-view-first-page)
              ("C-l" . pdf-goto-label)
              ("C-;" . pdf-goto-page)
              ("z" . pdf-view-enlarge)
              ("Z" . pdf-view-shrink)
              ("0" . pdf-view-scale-reset)
              ("r" . pdf-view-rotate)
              ("R" . revert-buffer)
              ("a w" . pdf-view-fit-width-to-window)
              ("a h" . pdf-view-fit-height-to-window)
              ("a p" . pdf-view-fit-page-to-window)
              ("m" . pdf-view-position-to-register)
              ("M" . pdf-view-jump-to-register)
              ("v d" . pdf-view-dark-minor-mode)
              ("v m" . pdf-view-midnight-minor-mode)
              ("v t" . pdf-view-themed-minor-mode)
              ("v p" . pdf-view-printer-minor-mode))
  :config
  (setopt pdf-view-display-size 'fit-page)
  (setopt pdf-view-use-unicode-ligther nil)
  (add-to-list 'pdf-view-incompatible-modes 'display-line-numbers-mode))

(use-package org
  :ensure t
  :init
  (setopt org-replace-disputed-keys t)
  (setopt org-return-follows-link t)
  :hook (org-mode . setup-a-mix-mode)
  :bind (:map org-mode-map
              ("C-j" . nil)
              ("S-RET" . #'org-return-and-maybe-indent)
              :map org-read-date-minibuffer-local-map
              ("C-v" . nil)
              ("C-<" . #'org-calendar-scroll-three-months-left)
              ("C->" . #'org-calendar-scroll-three-months-right))
  :config
  ;; Create and store templates directory
  (defconst ORG_DIR (file-name-as-directory
                     (if (getenv "XDG_DATA_HOME")
                         (file-name-concat (getenv "XDG_DATA_HOME") "org/")
                       "~/org/"))
    "Directory used as default location for org files.")
  (unless (file-directory-p ORG_DIR)
    (make-directory ORG_DIR t))
  (setopt org-default-notes-file (file-name-concat ORG_DIR ".notes"))
  (setopt org-support-shift-select t))

;;;; Note, proof-general itself might never actually be loaded
;;;; because individual proof assistants only trigger their own mode
;;;; and might only load proof.el or proof-site.el instead of proof-general.el
;;;; As such, we simply put (almost) all necessary config in :init directly
(use-package proof-general
  :ensure t
  :pin melpa
  :init
  ;;;;; Options
  ;;;;;; General
  (setopt proof-splash-enable nil
          proof-toolbar-enable nil)
  (setopt proof-delete-empty-windows t
          proof-output-tooltips t)
  (setopt proof-electric-terminator-enable t
          proof-sticky-errors t
          proof-prog-name-ask nil
          proof-minibuffer-messages t
          proof-next-command-insert-space nil
          proof-keep-response-history t
          pg-input-ring-size 32
          proof-follow-mode 'locked
          proof-auto-action-when-deactivating-scripting 'retract)
  (setopt bufhist-ring-size 32)
  ;;;;;; EasyCrypt
  (setopt easycrypt-script-indent nil
          easycrypt-one-command-per-line nil)
  (setopt easycrypt-prog-name "easycrypt")
  ;;;;; Hooks
  ;;;;;; General
  (defun setup-a-bufhist-map ()
    (keymap-set bufhist-mode-map "C-p" #'bufhist-prev)
    (keymap-set bufhist-mode-map "C-n" #'bufhist-next)
    (keymap-set bufhist-mode-map "C-<prior>" #'bufhist-prev)
    (keymap-set bufhist-mode-map "C-<next>" #'bufhist-next)
    (keymap-set bufhist-mode-map "C-<home>" #'bufhist-first)
    (keymap-set bufhist-mode-map "C-<end>" #'bufhist-last)
    (keymap-set bufhist-mode-map "M-c" #'bufhist-clear)
    (keymap-set bufhist-mode-map "M-d" #'bufhist-delete))
  (defun setup-a-proof-mode-map ()
    (keymap-set proof-mode-map "C-p" #'proof-undo-last-successful-command)
    (keymap-set proof-mode-map "C-n" #'proof-assert-next-command-interactive)
    (keymap-set proof-mode-map "C-<prior>" #'proof-goto-command-start)
    (keymap-set proof-mode-map "C-<next>" #'proof-goto-command-end)
    (keymap-set proof-mode-map "C-<end>" #'proof-goto-end-of-locked)
    (keymap-set proof-mode-map "C-c C-v" #'proof-goto-point)
    (keymap-set proof-mode-map "C-c C-d" #'proof-undo-and-delete-successful-command)
    (keymap-set proof-mode-map "C-c C-a" #'proof-goto-command-start)
    (keymap-set proof-mode-map "C-c C-e" #'proof-goto-command-end)
    (keymap-set proof-mode-map "C-c C-l" #'proof-goto-end-of-locked)
    (keymap-set proof-mode-map "C-c C-w" #'proof-display-some-buffers)
    (keymap-set proof-mode-map "C-c C-k" #'pg-response-clear-displays)
    (keymap-set proof-mode-map "C-c C-x" #'proof-minibuffer-cmd)
    (keymap-set proof-mode-map "C-c C-q" #'proof-shell-exit)
    (keymap-set proof-mode-map "M-p" #'pg-previous-matching-input-from-input)
    (keymap-set proof-mode-map "M-n" #'pg-next-matching-input-from-input)
    (keymap-set proof-mode-map "C-M-p" #'pg-previous-input)
    (keymap-set proof-mode-map "C-M-n" #'pg-next-input)
    (keymap-set proof-mode-map "C-c C-y p" #'pg-previous-matching-input-from-input)
    (keymap-set proof-mode-map "C-c C-y n" #'pg-next-matching-input-from-input)
    (keymap-set proof-mode-map "C-c C-y P" #'pg-previous-matching-input)
    (keymap-set proof-mode-map "C-c C-y N" #'pg-next-matching-input))
  (add-hook 'proof-mode-hook #'setup-a-proof-mode-map)
  (add-hook 'proof-mode-hook #'setup-a-bufhist-map)
  ;;;;;; EasyCrypt
  (add-hook 'easycrypt-mode-hook #'(lambda () (setq-local electric-indent-inhibit t))))

;;; Themes
;;;; Doom-themes general
(use-package doom-themes
  :ensure t
  :config
  (setopt doom-themes-enable-bold t
          doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)

  (doom-themes-set-faces nil
    '(vertico-current :foreground 'unspecified :background 'unspecified
                      :inherit 'highlight)
    '(vertico-mouse :foreground 'unspecified :background 'unspecified
                      :inherit 'lazy-highlight)
    '(corfu-border :foreground 'unspecified :background 'unspecified
                   :inherit 'vertical-border)
    '(corfu-bar :foreground 'unspecified :background 'unspecified
                :inherit 'scroll-bar)
    '(corfu-current :foreground 'unspecified :background 'unspecified
                    :inherit 'secondary-selection)
    '(tempel-default :foreground 'unspecified :background 'unspecified
                     :inherit 'lazy-highlight :slant 'italic)
    '(tempel-field :foreground 'unspecified :background 'unspecified
                   :inherit 'highlight)
    '(tempel-form :foreground 'unspecified :background 'unspecified
                  :inherit 'match)
    '(proof-mouse-highlight-face :inherit 'lazy-highlight)
    '(proof-region-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
    '(proof-command-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
    '(proof-active-area-face :inherit 'secondary-selection)))

;;;; Doom-themes specific
;;;;; Nord <3
(use-package doom-nord-theme
  :after doom-themes
  :ensure nil ; provided by doom-themes
  :demand t ; Use this theme
  :config
  (setopt doom-nord-brighter-modeline t
          doom-nord-brighter-comments nil
          doom-nord-comment-bg nil
          doom-nord-padded-modeline nil
          doom-nord-region-highlight 'snowstorm)

  (load-theme 'doom-nord t)
  (doom-themes-set-faces 'doom-nord
    '(trailing-whitespace :background magenta)
    '(aw-background-face :inherit 'avy-background-face)
    '(aw-leading-char-face :inherit 'avy-lead-face)
    '(proof-queue-face :background magenta)
    '(proof-locked-face :background base4)
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
    '(easycrypt-tactics-tacticals-face :inherit 'proof-tacticals-name-face)
    '(easycrypt-tactics-closing-face :foreground yellow)
    '(easycrypt-tactics-dangerous-face :foreground red))
  (enable-theme 'doom-nord))


;; Cross-package enhancements
;;; Custom functions/functionalities (with package dependencies)
(require 'func-pkgs)

;;; Avy + Embark
(add-to-list 'avy-dispatch-alist '(?, . avy-action-embark-act) t)
(add-to-list 'avy-dispatch-alist '(?. . avy-action-embark-dwim) t)

;; Corfu + Orderless
(add-hook 'corfu-mode-hook
          (lambda ()
            (setq-local completion-styles '(orderless-literal-only basic))))

;; Corfu + Vertico
(setopt global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map)))))

;; Hooks
;;; Frames/windows
(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'setup-a-client-frame)
  (add-hook 'after-make-frame-functions #'setup-a-frame))

(add-hook 'minibuffer-setup-hook #'setup-a-mini-mix-mode)

;;; Modes
(add-hook 'text-mode-hook #'setup-a-text-mode)

(add-hook 'log-edit-mode-hook #'setup-a-mix-mode)
(add-hook 'tex-mode-hook #'setup-a-mix-mode)
(add-hook 'TeX-mode-hook #'setup-a-mix-mode)
(add-hook 'conf-mode-hook #'setup-a-mix-mode)

(add-hook 'prog-mode-hook #'setup-a-code-mode)
