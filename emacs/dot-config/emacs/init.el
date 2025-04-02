;; -*- lexical-binding: t -*-
;; init.el
;; Environment
;;; Config
(defconst THEMES_DIR (file-name-as-directory
                      (if (getenv "XDG_CONFIG_HOME")
                          (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/themes/")
                        (file-name-concat user-emacs-directory "themes/")))
  "Directory where (custom) themes are stored.")

(defconst CUSTOM_FILE (if (getenv "XDG_CONFIG_HOME")
                          (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/custom-set.el")
                        (file-name-concat user-emacs-directory "custom-set.el"))
  "File where (automatically generated) customization settings are stored")

(defconst FUNCS_DIR (file-name-as-directory
                     (if (getenv "XDG_CONFIG_HOME")
                         (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/funcs/")
                       (file-name-concat user-emacs-directory "funcs/")))
  "Directory where (custom) functions are defined")

(defconst TEMPLATES_DIR (file-name-as-directory
                         (if (getenv "XDG_CONFIG_HOME")
                             (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/templates/")
                           (file-name-concat user-emacs-directory "templates/")))
  "Directory where (custom) templates are defined")

;;; Data
(defconst BACKUPS_DIR (file-name-as-directory
                       (if (getenv "XDG_DATA_HOME")
                           (file-name-concat (getenv "XDG_DATA_HOME") "emacs/backups/")
                         (file-name-concat user-emacs-directory "backups/")))
  "Directory where (automatically generated) backup files are stored.")

;;; Cache
(defconst AUTOSAVES_DIR (file-name-as-directory
                         (if (getenv "XDG_CACHE_HOME")
                             (file-name-concat (getenv "XDG_CACHE_HOME") "emacs/autosaves/")
                           (file-name-concat user-emacs-directory "autosaves/")))
  "Directory where auto-save files are stored.")

(defconst LOCKS_DIR (file-name-as-directory
                     (if (getenv "XDG_CACHE_HOME")
                         (file-name-concat (getenv "XDG_CACHE_HOME") "emacs/locks/")
                       (file-name-concat user-emacs-directory "locks/")))
  "Directory where lock files are stored.")


;; Bootstrap
;;; Directories
(unless (file-directory-p THEMES_DIR)
  (make-directory THEMES_DIR))

(unless (file-directory-p FUNCS_DIR)
  (make-directory FUNCS_DIR))

(unless (file-directory-p TEMPLATES_DIR)
  (make-directory TEMPLATES_DIR))

(unless (file-directory-p BACKUPS_DIR)
  (make-directory BACKUPS_DIR))

(unless (file-directory-p AUTOSAVES_DIR)
  (make-directory AUTOSAVES_DIR))

(unless (file-directory-p LOCKS_DIR)
  (make-directory LOCKS_DIR))

;;; Custom file
(unless (file-exists-p CUSTOM_FILE)
  (make-empty-file CUSTOM_FILE))
(setopt custom-file CUSTOM_FILE)
(load CUSTOM_FILE)

;;; Load path/pointers
(add-to-list 'load-path FUNCS_DIR)
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

;;; Custom functions (without package dependencies)
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
(windmove-default-keybindings 'meta)

(keymap-global-set "<left>" 'left-char)
(keymap-global-set "<right>" 'right-char)

(keymap-global-set "C-<left>" 'backward-word)
(keymap-global-set "C-<right>" 'forward-word)

(keymap-global-set "C-a" 'move-beginning-of-line)
(keymap-global-set "C-e" 'move-end-of-line)

(keymap-global-set "<up>" 'previous-line)
(keymap-global-set "<down>" 'next-line)

(keymap-global-set "C-<up>" 'backward-paragraph)
(keymap-global-set "C-<down>" 'forward-paragraph)

(keymap-global-set "<home>" 'beginning-of-buffer)
(keymap-global-set "<end>" 'end-of-buffer)

(keymap-global-set "C-<prior>" 'scroll-other-window-down)
(keymap-global-set "C-<next>" 'scroll-other-window)

(keymap-global-set "C-<home>" 'beginning-of-buffer-other-window)
(keymap-global-set "C-<end>" 'end-of-buffer-other-window)

(keymap-global-set "C-b" 'switch-to-prev-buffer)
(keymap-global-set "C-f" 'switch-to-next-buffer)

(keymap-global-set "C-p" 'backward-sentence)
(keymap-global-set "C-n" 'forward-sentence)

(keymap-global-set "C-w" 'other-window)

;;; Selection
(keymap-global-set "C-SPC" 'set-mark-command)

;;; Manipulation
;;;; Copying
(keymap-global-set "C-t" 'kill-ring-save)
(keymap-global-set "C-T" 'clipboard-kill-ring-save)

;;;; Killing
(keymap-global-set "M-<backspace>" 'backward-kill-word)
(keymap-global-set "M-<deletechar>" 'kill-word)

(keymap-global-set "M-e" 'kill-line)
(keymap-global-set "M-l" 'kill-whole-line)

(keymap-global-set "M-b" 'backward-kill-sexp)
(keymap-global-set "M-f" 'forward-kill-sexp)

(keymap-global-set "M-p" 'backward-kill-sentence)
(keymap-global-set "M-n" 'kill-sentence)

(keymap-global-set "M-t" 'kill-region)
(keymap-global-set "M-T" 'clipboard-kill-region)

(keymap-global-set "M-;" 'undo)
(keymap-global-set "M-/" 'undo-redo)

;;;; Yanking
(keymap-global-set "M-y" 'yank)
(keymap-global-set "M-Y" 'clipboard-yank)
(keymap-global-set "C-M-y" 'yank-pop)

;;;; Deleting
(keymap-global-set "M-d" 'delete-region)

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
  "B" #'kill-buffer
  "C-b" #'kill-some-buffers)

(keymap-global-set "C-x k" 'a-kill-map-prefix)

;;;; Frames
(defvar-keymap a-frame-map
  :doc "Keymap for frame management"
  :prefix 'a-frame-map-prefix
  "q" #'delete-frame
  "Q" #'delete-other-frames
  "u" #'undelete-frame
  "c" #'clone-frame
  "m" #'make-frame-command
  "f" #'find-file-other-frame
  "g" #'other-frame
  "s" #'suspend-frame)

(keymap-global-set "C-x f" 'a-frame-map-prefix)

;;;; Windows
(defvar-keymap a-window-map
  :doc "Keymap for window management"
  :prefix 'a-window-map-prefix
  "q" #'delete-window
  "Q" #'delete-other-windows
  "C-q" #'delete-windows-on
  "s" #'split-window-horizontally
  "S" #'split-window-vertically
  "f" #'fit-window-to-buffer
  "g" #'other-window
  "t" #'tear-off-window
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "<left>" #'windmove-left
  "<down>" #'windmove-down
  "<up>" #'windmove-up
  "<right>" #'windmove-right)

(keymap-global-set "C-x w" 'a-window-map-prefix)

;;;; Buffers
(defvar-keymap a-buffer-map
  :doc "Keymap for buffer management"
  :prefix 'a-buffer-map-prefix
  "q" #'kill-current-buffer
  "Q" #'kill-buffer
  "C-q" #'kill-some-buffers
  "s" #'save-buffer
  "S" #'save-some-buffers
  "g" #'switch-to-buffer
  "G" #'switch-to-buffer-other-window
  "M-g" #'switch-to-minibuffer)

(keymap-global-set "C-x b" 'a-buffer-map-prefix)

;;; Navigation/searching
(keymap-global-set "C-x C-f" #'find-file)
(keymap-global-set "C-x C-r" #'recentf-open)

;;;; Goto map
(defvar-keymap a-goto-map
  :doc "Keymap for navigation"
  :prefix 'a-goto-map-prefix
  "b" #'switch-to-buffer
  "B" #'switch-to-buffer-other-window
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
          isearch-allow-scroll t
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

(use-package marginalia
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
              ("C-r" . marginalia-cycle))
  :config
  (setopt marginalia-field-width 100)
  (marginalia-mode 1))

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
              ("C-M-p" . vertico-exit-input)
              ("TAB" . minibuffer-complete)
              ("<tab>" . minibuffer-complete)
              ("C-?" . minibuffer-completion-help)
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
  :ensure nil ; inlcuded with vertico
  :bind (:map vertico-map
              ("C-d" . vertico-directory-enter)
              ("<backspace>" . vertico-directory-delete-char)
              ("M-<backspace>" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


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
  :bind ("C-c p" . cape-prefix-map)
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
  (add-hook 'latex-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'conf-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'prog-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'minibuffer-setup-hook #'setup-a-cape-minibuffer))

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
  (defconst TEMPEL_DIR (file-name-as-directory (file-name-concat TEMPLATES_DIR "tempel/")))
  (unless (file-directory-p TEMPEL_DIR)
    (make-directory TEMPEL_DIR))
  (setopt tempel-path (file-name-concat TEMPEL_DIR "*.eld"))
  (global-tempel-abbrev-mode 1))

;;; Actions
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
         :map a-goto-map
         ("b" . consult-buffer)
         ("B" . consult-buffer-other-window)
         ("e" . consult-compile-error)
         ("d" . consult-flymake)
         ("l" . consult-goto-line)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map a-search-map
         ("f" . find-file)
         ("F" . consult-fd)
         ("C-f" . consult-find)
         ("r" . consult-recent-file)
         ("c" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-ripgrep)
         ("M-g" . consult-git-grep)
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
          consult-async-min-input 2))

(use-package embark
  :ensure t
  :pin melpa
  :bind (("C-," . embark-act)
         ("C-." . embark-dwim)
         ("C-h C-b" . embark-bindings)
         :map embark-general-map
         ("t" . embark-copy-as-kill)
         ("C-o" . embark-select)
         :map embark-buffer-map
         ("g" . switch-to-buffer)
         :map embark-identifier-map
         ("f" . xref-find-definitions)
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
         ("C-T" . magit-copy-buffer-revision)
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

;;; Themes
(use-package doom-themes
  :ensure t
  :config
  (setopt doom-themes-enable-bold t
          doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (setopt doom-nord-brighter-modeline t
          doom-nord-brighter-comments nil
          doom-nord-comment-bg nil
          doom-nord-padded-modeline nil
          doom-nord-region-highlight 'snowstorm)
  (load-theme 'doom-nord t)
  (doom-themes-set-faces nil
    '(trailing-whitespace :background magenta)
    '(vertico-current :foreground 'unspecified :background 'unspecified
                      :inherit 'highlight :extend t)
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
                  :inherit 'match)))

;; Cross-package enhancements
;;; Custom functions (with package dependencies)
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

(add-hook 'special-mode-hook #'setup-a-mix-mode)
(add-hook 'log-edit-mode-hook #'setup-a-mix-mode)
(add-hook 'tex-mode-hook #'setup-a-mix-mode)
(add-hook 'latex-mode-hook #'setup-a-mix-mode)
(add-hook 'conf-mode-hook #'setup-a-mix-mode)

(add-hook 'prog-mode-hook #'setup-a-code-mode)
