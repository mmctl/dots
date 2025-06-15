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

(defconst LOCAL_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "local/"))
  "Directory where (custom) local functionalities/packages are defined.")

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

(unless (file-directory-p LOCAL_DIR)
  (make-directory LOCAL_DIR t))

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
;;;; Add LOCAL_DIR and its sub-directories to load path, excluding hidden ones
(add-to-list 'load-path LOCAL_DIR)
(dolist (file (directory-files-recursively LOCAL_DIR "^[^.].*" t t))
  (when (file-directory-p file)
    (add-to-list 'load-path file)))

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

;;; Custom local functionalities
(require 'loc-frames)
(require 'loc-modes)
(require 'loc-utils)

;; Package system
(require 'package)

(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-archive-priorities '(("gnu" . 10)
                                     ("nongnu" . 5)
                                     ("melpa" . 1)))

(package-initialize t)

(unless package-archive-contents
  (package-refresh-contents))


;; Settings (general/UI)
;;; Launching
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message "// This buffer is for text that is not saved, and for Lisp evaluation \\\\")

;;; Frames/Windows
(setopt frame-resize-pixelwise t)
(setopt window-resize-pixelwise t)

(setopt indicate-buffer-boundaries nil)
(setopt indicate-empty-lines nil)

(setopt switch-to-buffer-obey-display-actions t)
(setopt uniquify-buffer-name-style 'forward)
(setopt highlight-nonselected-windows nil)

(line-number-mode 1)
(column-number-mode 1)

(setopt display-time-24hr-format t)
(setopt display-time-default-load-average nil)
(setopt display-time-default-load-average nil)

(display-time-mode 1)

(unless (daemonp)
  (loc-setup-global-frame))

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

(setopt fill-column 80)

(setopt kill-do-not-save-duplicates t)

(setopt use-dialog-box nil)
(setopt use-short-answers t)
(setopt confirm-nonexistent-file-or-buffer nil)

(setopt show-trailing-whitespace nil)

(setopt ring-bell-function #'ignore)
(setopt visible-bell nil)

(setopt hl-line-sticky-flag nil)
(setopt global-hl-line-sticky-flag nil)

(setopt cycle-spacing-actions '(delete-all-space
                                (just-one-space -)
                                (delete-space-before 0)
                                (delete-space-after 0)
                                restore))

(setopt shift-select-mode t)
(repeat-mode 1)


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
(keymap-global-set "C-M-<left>" #'windmove-left)
(keymap-global-set "C-M-<down>" #'windmove-down)
(keymap-global-set "C-M-<up>" #'windmove-up)
(keymap-global-set "C-M-<right>" #'windmove-right)

(keymap-global-set "M-S-<left>" #'windmove-swap-states-left)
(keymap-global-set "M-S-<down>" #'windmove-swap-states-down)
(keymap-global-set "M-S-<up>" #'windmove-swap-states-up)
(keymap-global-set "M-S-<right>" #'windmove-swap-states-right)

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

(keymap-global-set "C-w" #'other-window)

(keymap-global-set "M-m" #'pop-to-mark-command)
(keymap-global-set "M-M" #'pop-global-mark)

;;; Selection
(keymap-global-set "M-h" #'set-mark-command)

;;; Manipulation
;;;; Copying
(keymap-global-set "C-t" #'kill-ring-save)
(keymap-global-set "C-S-t" #'clipboard-kill-ring-save)

;;;; Exchanging/Joining
(keymap-global-set "M-w" #'exchange-word)
(keymap-global-set "M-W" #'exchange-word-backward)

(keymap-global-set "M-j" #'join-line-stay)
(keymap-global-set "M-J" #'join-line-forward-stay)

;;;; Killing
(keymap-global-set "M-<backspace>" #'backward-kill-word)
(keymap-global-set "M-<delete>" #'kill-word)

(keymap-global-set "M-a" #'backward-kill-line)
(keymap-global-set "M-e" #'kill-line)
(keymap-global-set "M-l" #'kill-whole-line)

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
(keymap-global-set "C-k" #'cycle-spacing)
(keymap-global-set "M-k" #'delete-all-space)

(keymap-global-set "M-A" #'backward-delete-line)
(keymap-global-set "M-E" #'forward-delete-line)
(keymap-global-set "M-D" #'delete-whole-line-or-region)

;;;; Replacing
(keymap-global-set "M-r" #'query-replace)
(keymap-global-set "M-R" #'query-replace-regexp)

;;; Miscellaneous
(keymap-global-set "M-c" #'comment-dwim)
(keymap-global-set "M-C" #'comment-line)

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
(defvar-keymap a-window-resize-repeat-map
  :doc "Keymap (repeatable) for resizing windows"
  :repeat (:hints ((shrink-window . "h: Shrink window height")
                   (enlarge-window . "H: Enlarge window height")
                   (shrink-window-horizontally . "w: Shrink window width")
                   (enlarge-window-horizontally . "W: Enlarge window width")))
  "h" #'shrink-window
  "H" #'enlarge-window
  "w" #'shrink-window-horizontally
  "W" #'enlarge-window-horizontally)

(defvar-keymap a-window-map
  :doc "Keymap for window management"
  :prefix 'a-window-map-prefix
  "h" #'shrink-window
  "H" #'enlarge-window
  "w" #'shrink-window-horizontally
  "W" #'enlarge-window-horizontally
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
  "i" #'imenu
  "l" #'goto-line
  "m" #'pop-to-mark-command
  "M" #'pop-global-mark
  "w" #'other-window)

(keymap-global-set "C-c g" 'a-goto-map-prefix)

;;;; Search map
(defvar-keymap a-search-replace-map
  :doc "Keymap for searching and replacing"
  :prefix 'a-search-replace-map-prefix
  "f" #'find-file
  "F" #'find-file-other-window
  "C-f" #'find-file-other-frame
  "o" #'recentf-open
  "O" #'find-file-read-only
  "r i" #'isearch-query-replace
  "r I" #'isearch-query-replace-regexp
  "r q" #'query-replace
  "r Q" #'query-replace-regexp
  "i b" #'isearch-backward
  "i f" #'isearch-forward
  "i s" #'isearch-forward-symbol
  "i r" #'isearch-query-replace
  "i R" #'isearch-query-replace-regexp
  "i w" #'isearch-forward-word
  "i ." #'isearch-forward-symbol-at-point)

(keymap-global-set "C-c s" 'a-search-replace-map-prefix)


;; Packages
;;; General
(setopt use-package-always-ensure nil
        use-package-always-defer nil
        use-package-always-pin nil
        use-package-always-demand nil)

;;; Base/Built-in
(use-package isearch
  :init
  ;; Setup and settings
  (setopt search-exit-option t)
  (setopt isearch-repeat-on-direction-change t
          isearch-lazy-count t
          isearch-lax-whitespace t
          isearch-allow-scroll 'unlimited
          isearch-allow-motion t)
  (setopt lazy-count-prefix-format nil
          lazy-count-suffix-format " [%s of %s]")

  :config
  ;; Keybindings
  (keymap-unset isearch-mode-map "C-w")
  (keymap-set isearch-mode-map "C-v" #'isearch-exit)
  (keymap-set isearch-mode-map "C-t" #'isearch-yank-word-or-char)
  (keymap-set isearch-mode-map "M-y" #'isearch-yank-kill)
  (keymap-set isearch-mode-map "M-r" #'isearch-query-replace)
  (keymap-set isearch-mode-map "M-R" #'isearch-query-replace-regexp))

(use-package dired
  :init
  ;; Setup and settings
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

  :config
  ;; Keybindings
  (keymap-set dired-mode-map "C-v" #'dired-find-file)
  (keymap-set dired-mode-map "RET" #'dired-find-file)
  (keymap-set dired-mode-map "<return>" "RET")
  (keymap-set dired-mode-map "C-o" #'dired-display-file)
  (keymap-set dired-mode-map "C-<up>" #'dired-prev-dirline)
  (keymap-set dired-mode-map "C-<down>" #'dired-next-dirline)
  (keymap-set dired-mode-map "C-p" #'dired-prev-marked-file)
  (keymap-set dired-mode-map "C-n" #'dired-next-marked-file)
  (keymap-set dired-mode-map "C-q" #'dired-up-directory))

(use-package dabbrev
  :init
  ;; Setup and settings
  (setopt dabbrev-upcase-means-case-search t
          dabbrev-case-distinction nil
          dabbrev-case-replace nil))

;;; Helpers
(use-package which-key
  :ensure t

  :init
  ;; Setup and settings
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

  :config
  ;; Keybindings
  (keymap-set which-key-mode-map "C-x <f3>" #'which-key-C-h-dispatch)

  ;; Hooks
  (add-hook 'which-key-init-buffer-hook #'loc-setup-mini-mix-mode)

  ;; Activation
  (which-key-mode 1))

(use-package goggles
  :ensure t

  :hook (prog-mode text-mode)

  :init
  ;; Setup and settings (before load)
  (setopt goggles-pulse t))

(use-package easy-kill
  :ensure t

  :bind (("<remap> <kill-ring-save>" . #'easy-kill)
         ("<remap> <mark-word>" . #'easy-mark))

  :init
  ;; Setup and settings (before load)
  (setopt easy-kill-alist '((?w word           " ")
                            (?s sexp           "\n")
                            (?h list           "\n")
                            (?f filename       "\n")
                            (?d defun          "\n\n")
                            (?D defun-name     " ")
                            (?l line           "\n")
                            (?b buffer-file-name)))
  (setopt easy-kill-cycle-ignored '(list filename defun defun-name buffer-file-name)
          easy-kill-try-things '(url email word line)
          easy-mark-try-things '(url email word sexp))

  :config
  ;; Keybindings
  (keymap-set easy-kill-base-map "<remap> <kill-ring-save>" #'easy-kill-cycle)
  (keymap-set easy-kill-base-map "<" #'easy-kill-shrink)
  (keymap-set easy-kill-base-map ">" #'easy-kill-expand)
  (keymap-set easy-kill-base-map "a"  #'easy-kill-append)
  (keymap-set easy-kill-base-map "k"  #'easy-kill-region)
  (keymap-set easy-kill-base-map "K"  #'easy-kill-delete-region)
  (keymap-set easy-kill-base-map "q"  #'easy-kill-abort)
  (keymap-set easy-kill-base-map "p"  #'easy-kill-exchange-point-and-mark))

(use-package expand-region
  :ensure t
  :pin melpa

  :bind ("C->" . #'er/expand-region)

  :init
  ;; Setup and settings
  (setopt expand-region-fast-keys-enabled t
          expand-region-contract-fast-key "<"
          expand-region-reset-fast-key "r"))

(use-package undo-tree
  :ensure t

  :init
  ;; Setup and settings
  ;;; Create and store undo history directory
  (defconst UNDO_DIR (file-name-as-directory (file-name-concat EMACS_DATA_DIR "undos/"))
    "Directory where (automatically generated) undo (history) files are stored.")
  (unless (file-directory-p UNDO_DIR)
    (make-directory UNDO_DIR t))
  (setopt undo-tree-history-directory-alist `(("." . ,UNDO_DIR)))

  (setopt undo-tree-mode-lighter " UT")
  (setopt undo-tree-incompatible-major-modes '(term-mode image-mode doc-view-mode pdf-view-mode))
  (setopt undo-tree-auto-save-history t
          undo-tree-enable-undo-in-region t
          undo-tree-visualizer-diff t)

  :config
  ;; Keybindings
  (keymap-set undo-tree-map "<remap> <undo-redo>" #'undo-tree-redo)

  (keymap-unset undo-tree-visualizer-mode-map "C-f")
  (keymap-unset undo-tree-visualizer-mode-map "C-b")
  (keymap-set undo-tree-visualizer-mode-map "h" #'undo-tree-visualizer-scroll-left)
  (keymap-set undo-tree-visualizer-mode-map "j" #'undo-tree-visualizer-scroll-down)
  (keymap-set undo-tree-visualizer-mode-map "k" #'undo-tree-visualizer-scroll-up)
  (keymap-set undo-tree-visualizer-mode-map "l" #'undo-tree-visualizer-scroll-right)

  (keymap-unset undo-tree-visualizer-selection-mode-map "C-f")
  (keymap-unset undo-tree-visualizer-selection-mode-map "C-b")
  (keymap-set undo-tree-visualizer-selection-mode-map "C-v" #'undo-tree-visualizer-set)

  ;; Activation
  (global-undo-tree-mode 1))

(use-package marginalia
  :ensure t

  :hook minibuffer-setup

  :init
  ;; Setup and settings
  (setopt marginalia-field-width 100)

  :config
  ;; Keybindings
  (keymap-set minibuffer-mode-map "C-r" #'marginalia-cycle)
  (keymap-set minibuffer-local-map "C-r" #'marginalia-cycle))

(use-package jinx
  :ensure t

  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :hook (text-mode prog-mode conf-mode)

  :init
  ;; Setup and settings
  (setopt jinx-languages "en_US")
  (setopt jinx-include-faces '((prog-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (conf-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (yaml-mode . conf-mode)
                               (yaml-ts-mode . conf-mode)))

  :config
  ;; Keybindings
  (keymap-set jinx-overlay-map "C-p" #'jinx-previous)
  (keymap-set jinx-overlay-map "C-n" #'jinx-next)
  (keymap-set jinx-repeat-map "C-p" #'jinx-previous)
  (keymap-set jinx-repeat-map "C-n" #'jinx-next)
  (keymap-set jinx-correct-map "C-p" #'jinx-previous)
  (keymap-set jinx-correct-map "C-n" #'jinx-next))

(use-package anzu
  :ensure t

  :init
  ;; Setup and settings
  (setopt anzu-input-idle-delay 0.1)
  (setopt anzu-replace-at-cursor-thing 'sexp)
  (setopt anzu-replace-to-string-separator "")

  :config
  ;; Keybindings
  (keymap-global-set "<remap> <query-replace>" #'anzu-query-replace)
  (keymap-global-set "<remap> <query-replace-regexp>" #'anzu-query-replace-regexp)
  (keymap-global-set "<remap> <isearch-query-replace>" #'anzu-isearch-query-replace)
  (keymap-global-set "<remap> <isearch-query-replace-regexp>" #'anzu-isearch-query-replace-regexp)

  (keymap-set isearch-mode-map "<remap> <isearch-query-replace>" #'anzu-isearch-query-replace)
  (keymap-set isearch-mode-map "<remap> <isearch-query-replace-regexp>" #'anzu-isearch-query-replace-regexp)

  (keymap-set a-search-replace-map "r ." #'anzu-query-replace-at-cursor)
  (keymap-set a-search-replace-map "r t" #'anzu-query-replace-at-cursor-thing)
  (keymap-set a-search-replace-map "r T" #'anzu-replace-at-cursor-thing)

  ;; Activation
  (global-anzu-mode 1))

;;; Completion
(use-package orderless
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt orderless-smart-case t
          orderless-expand-substring 'prefix)
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion))))

  :config
  ;; Setup and settings (after load)
  (setopt orderless-matching-styles (list #'orderless-literal #'orderless-flex #'orderless-regexp))

  ;; Custom functionality
  (orderless-define-completion-style orderless-flex-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-flex)))
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal))))


(use-package vertico
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt vertico-count 15
          vertico-preselect 'first
          vertico-scroll-margin 2
          vertico-cycle nil)

  :config
  ;; Keybindings
  (keymap-set vertico-map "C-o" #'vertico-insert)
  (keymap-set vertico-map "C-v" #'vertico-exit)
  (keymap-set vertico-map "C-M-v" #'vertico-exit-input)
  (keymap-set vertico-map "TAB" #'minibuffer-complete)
  (keymap-set vertico-map "<tab>" "TAB")
  (keymap-set vertico-map "C-?" #'minibuffer-completion-help)
  (keymap-set vertico-map "C-p" #'previous-history-element)
  (keymap-set vertico-map "C-n" #'next-history-element)
  (keymap-set vertico-map "C-<up>" #'vertico-previous-group)
  (keymap-set vertico-map "C-<down>" #'vertico-next-group)
  (keymap-set vertico-map "C-<home>" #'vertico-first)
  (keymap-set vertico-map "C-<end>" #'vertico-last)

  ;; Activation
  (vertico-mode 1))

(use-package vertico-directory
  :ensure nil ; Provided by vertico

  :after vertico

  :config
  ;; Keybindings
  (keymap-set vertico-map "C-d" #'vertico-directory-enter)
  (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-<backspace>" #'vertico-directory-delete-word)

  ;; Hooks
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy))

(use-package vertico-mouse
  :ensure nil ; Provided by vertico

  :after vertico

  :hook vertico-mode

  :config
  ;; Keybindings
  (keymap-set vertico-mouse-map "<mouse-1>" (vertico-mouse--click "C-v"))
  (keymap-set vertico-mouse-map "<mouse-3>" (vertico-mouse--click "C-o")))

(use-package vertico-quick
  :ensure nil ; Provided by vertico

  :after vertico

  :init
  ;; Setup and settings (before load)
  (setopt vertico-quick1 "asdfjkl")
  (setopt vertico-quick2 "gerhui")

  :config
  ;; Keybindings
  (keymap-set vertico-map "C-y" #'vertico-quick-exit)
  (keymap-set vertico-map "C-S-y" #'vertico-quick-insert)
  (keymap-set vertico-map "C-S-j" #'vertico-quick-jump))

(use-package corfu
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt corfu-count 10
          corfu-scroll-margin 2
          corfu-min-width 15
          corfu-max-width 80
          corfu-cycle nil
          corfu-preview-current nil
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match 'separator
          corfu-left-margin-width 0.5
          corfu-right-margin-width 0.5
          corfu-bar-width 0.25
          corfu-auto-prefix 2
          corfu-auto-delay 0.25
          ;; Auto mode
          corfu-on-exact-match nil
          corfu-preselect 'valid
          corfu-auto t)
  (setopt text-mode-ispell-word-completion nil)

  :config
  ;; Keybindings
  ;;; All modes
  (keymap-set corfu-map "C-o" #'corfu-complete)
  (keymap-set corfu-map "TAB" #'corfu-complete)
  (keymap-set corfu-map "<tab>" "TAB")
  (keymap-set corfu-map "C-v" #'corfu-send)
  (keymap-set corfu-map "S-SPC" #'corfu-insert-separator)
  ;;; Auto mode
  (keymap-unset corfu-map "<remap> <beginning-of-buffer>")
  (keymap-unset corfu-map "<remap> <end-of-buffer>")
  (keymap-unset corfu-map "<remap> <previous-line>")
  (keymap-unset corfu-map "<remap> <next-line>")
  (keymap-unset corfu-map "<remap> <move-beginning-of-line>")
  (keymap-unset corfu-map "<remap> <move-end-of-line>")
  (keymap-unset corfu-map "RET")
  (keymap-unset corfu-map "<up>")
  (keymap-unset corfu-map "<down>")
  (keymap-set corfu-map "M-p" #'corfu-previous)
  (keymap-set corfu-map "M-n" #'corfu-next)
  (keymap-set corfu-map "M-<prior>" #'corfu-scroll-up)
  (keymap-set corfu-map "M-<next>" #'corfu-scroll-down)
  (keymap-set corfu-map "M-<home>" #'corfu-first)
  (keymap-set corfu-map "M-<end>" #'corfu-last)

  ;; Activation
  (global-corfu-mode 1))

(use-package corfu-quick
  :ensure nil ; Provided by Corfu

  :after corfu

  :init
  ;; Setup and settings (before load)
  (setopt corfu-quick1 "asdfjkl")
  (setopt corfu-quick2 "gerhui")

  :config
  ;; Keybindings
  (keymap-set corfu-map "C-y" #'corfu-quick-insert)
  (keymap-set corfu-map "C-S-y" #'corfu-quick-complete)
  (keymap-set corfu-map "C-S-j" #'corfu-quick-jump))


(use-package cape
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt cape-dict-limit 50
          cape-dabbrev-check-other-buffers #'cape-same-mode-buffers
          cape-file-prefix '("file:" "f:")
          cape-file-directory-must-exist t)

  ;; Keybindings
  (keymap-global-set "C-c y" #'cape-prefix-map)

  :config
  ;; Custom functionality
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
                                       #'cape-keyword-prefix-2
                                       #'cape-dabbrev-prefix-2)
                      #'cape-dict-prefix-2)))
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

  ;; Hooks
  (add-hook 'completion-at-point-functions (cape-capf-super #'cape-abbrev-prefix-2 #'cape-dabbrev-prefix-2))

  (add-hook 'text-mode-hook #'setup-a-cape-text-mode)
  (add-hook 'tex-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'TeX-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'conf-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'prog-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'minibuffer-setup-hook #'setup-a-cape-minibuffer))

(use-package tempel
  :ensure t

  :preface
  ;; Keymaps
  (defvar-keymap a-tempel-map
    :doc "Keymap for tempel (outside templates)"
    :prefix 'a-tempel-map-prefix)
  (keymap-global-set "C-c t" 'a-tempel-map-prefix)

  :bind (("M-o" . tempel-complete)
         ("M-v" . tempel-expand)
         (:map a-tempel-map
               ("c" . tempel-complete)
               ("e" . tempel-expand)
               ("i" . tempel-insert)))

  :init
  ;; Setup and settings (before load)
  ;;; Create and store templates directory
  (defconst TEMPEL_DIR (file-name-as-directory (file-name-concat TEMPLATES_DIR "tempel/"))
    "Directory where tempel templates are stored.")
  (unless (file-directory-p TEMPEL_DIR)
    (make-directory TEMPEL_DIR t))
  (setopt tempel-path (file-name-concat TEMPEL_DIR "*.eld"))

  (setopt tempel-mark #(" " 0 1 (display (space :width (3)) face tempel-field)))

  :config
  ;; Keybindings
  (tempel-key "f" fixme a-tempel-map)
  (tempel-key "t" todo a-tempel-map)

  (keymap-unset tempel-map "<remap> <beginning-of-buffer>")
  (keymap-unset tempel-map "<remap> <end-of-buffer>")
  (keymap-unset tempel-map "<remap> <backward-paragraph>")
  (keymap-unset tempel-map "<remap> <forward-paragraph>")
  (keymap-set tempel-map "C-<backspace>" #'tempel-previous)
  (keymap-set tempel-map "C-SPC" #'tempel-next)
  (keymap-set tempel-map "<prior>" #'tempel-previous)
  (keymap-set tempel-map "<next>" #'tempel-next)
  (keymap-set tempel-map "<home>" #'tempel-beginning)
  (keymap-set tempel-map "<end>" #'tempel-end)
  (keymap-set tempel-map "M-k" #'tempel-kill)
  (keymap-set tempel-map "C-v" #'tempel-done)
  (keymap-set tempel-map "C-q" #'tempel-abort)

  (defun a-tempel-placeholder-form-as-lit (elt)
    "Define slight adjustment of regular placeholder element
so that a prompt form evaluating to a string is inserted as
default value in the same way as a literal string prompt."
    (pcase elt
      (`(pfl ,prompt . ,rest)
       (let ((evprompt (eval prompt)))
         (if (stringp evprompt)
             `(p ,evprompt ,@rest)
           `('p ,prompt ,@rest))))))
  (add-to-list 'tempel-user-elements #'a-tempel-placeholder-form-as-lit)

  (defun a-tempel-include (elt)
    "Define `include' element (taken and slightly adjusted from TempEL github repo)
that allows to include other templates by their name."
    (when (eq (car-safe elt) 'i)
      (when-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template))))
  (add-to-list 'tempel-user-elements #'a-tempel-include))

;;; Actions
(use-package move-text
  :ensure t

  :bind (("M-u" . move-text-up)
         ("M-U" . move-text-down)))

(use-package crux
  :ensure t
  :pin melpa

  :preface
  ;; Keymaps
  (defvar-keymap a-crux-map
    :doc "Keymap for crux actions"
    :prefix 'a-crux-map-prefix)
  (keymap-global-set "C-c x" 'a-crux-map-prefix)

  :bind (("<remap> <move-beginning-of-line>" . crux-move-beginning-of-line)
         ("<remap> <kill-whole-line>" . crux-kill-whole-line)
         ("M-d" . crux-duplicate-current-line-or-region)
         (:map a-crux-map
               ("RET" . crux-smart-open-line)
               ("<return>" . "RET")
               ("S-<return>" . crux-smart-open-line-above)
               ("c" . crux-copy-file-preserve-attributes)
               ("C" . crux-cleanup-buffer-or-region)
               ("d" . crux-duplicate-current-line-or-region)
               ("D" . crux-duplicate-and-comment-current-line-or-region)
               ("e" . crux-eval-and-replace)
               ("f f" . crux-recentf-find-file)
               ("f d" . crux-recentf-find-directory)
               ("f i" . crux-find-user-init-file)
               ("f c" . crux-find-user-custom-file)
               ("f s" . crux-find-shell-init-file)
               ("f l" . crux-find-current-directory-dir-locals-file)
               ("F" . crux-recentf-find-file)
               ("i" . crux-indent-rigidly-and-copy-to-clipboard)
               ("j" . crux-top-join-line)
               ("J" . crux-kill-and-join-forward)
               ("k" . crux-delete-file-and-buffer)
               ("l" . crux-kill-whole-line)
               ("r" . crux-rename-file-and-buffer)
               ("s" . crux-sudo-edit)
               ("u" . crux-view-url)
               ("x" . crux-visit-term-buffer)
               ("X" . crux-visit-shell-buffer)
               ("C-u" . crux-upcase-region)
               ("C-l" . crux-downcase-region)
               ("C-c" . crux-captialize-region))
         (:map a-kill-map
               ("B" . crux-kill-other-buffers))
         (:map a-buffer-map
               ("c" . crux-create-scratch-buffer)
               ("d" . crux-kill-buffer-truename)
               ("K" . crux-kill-other-buffers)
               ("p" . crux-switch-to-previous-buffer))
         (:map a-window-map
               ("z" . crux-transpose-windows))))

(use-package ace-window
  :ensure t

  :bind ("C-w" . ace-window)

  :init
  ;; Setup and settings (before load)
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
                  (?? aw-show-dispatch-help)))

  :config
  ;; Keybindings
  (keymap-global-set "C-S-w" #'other-window)

  (keymap-set a-goto-map "w" #'ace-window)
  (keymap-set a-goto-map "W" #'other-window))

(use-package avy
  :ensure t
  :pin melpa

  :preface
  (defvar-keymap an-avy-map
    :doc "Keymap for avy"
    :prefix 'an-avy-map-prefix)
  (keymap-global-set "C-c j" 'an-avy-map-prefix)

  :bind ((:map an-avy-map
               ("c" . avy-goto-char)
               ("C" . avy-goto-char-2)
               ("t" . avy-goto-char-timer)
               ("w" . avy-goto-word-1)
               ("W" . avy-goto-word-0)
               ("s" . avy-goto-subword-1)
               ("S" . avy-goto-subword-0)
               ("l" . avy-goto-line)
               ("C-l" . avy-kill-ring-save-whole-line)
               ("C-t" . avy-kill-ring-save-region)
               ("M-l" . avy-kill-whole-line)
               ("M-t" . avy-kill-region))
         (:map isearch-mode-map
               ("C-S-j" . avy-isearch)))
  :bind* ("C-j" . avy-goto-char)

  :init
  ;; Setup and settings (before load)
  (setopt avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?r ?u ?i)
          avy-style 'at-full
          avy-all-windows 'all-frames
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

  :preface
  (defvar-keymap a-consult-map
    :doc "Keymap for consult (misc)"
    :prefix 'a-consult-map-prefix)
  (keymap-global-set "C-c w" 'a-consult-map-prefix)

  :bind (("C-l" . consult-line)
         ("C-;" . consult-goto-line)
         ("C-M-y" . consult-yank-pop)
         (:map a-consult-map
               ("x" . consult-mode-command)
               ("h" . consult-history)
               ("k" . consult-kmacro)
               ("m" . consult-man)
               ("i" . consult-info)
               (":" . consult-complex-command))
         (:map a-buffer-map
               ("g" . consult-buffer)
               ("G" . consult-buffer-other-window)
               ("M-g" . consult-buffer-other-frame))
         (:map a-goto-map
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
               ("I" . consult-imenu-multi))
         (:map a-search-replace-map
               ("f" . find-file)
               ("F" . find-file-other-window)
               ("C-f" . consult-fd)
               ("M-f" . consult-find)
               ("o" . consult-recent-file)
               ("c" . consult-locate)
               ("g" . consult-ripgrep)
               ("G" . consult-git-grep)
               ("M-g" . consult-grep)
               ("l" . consult-line)
               ("L" . consult-line-multi)
               ("k" . consult-keep-lines)
               ("u" . consult-focus-lines)
               ("h" . consult-isearch-history))
         (:map isearch-mode-map
               ("M-s h" . consult-isearch-history)
               ("M-s l" . consult-line)
               ("M-s L" . consult-line-multi))
         (:map minibuffer-local-map
               ("M-s" . consult-history)
               ("M-r" . consult-history)))

  :init
  ;; Setup and settings (before load)
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

  :bind (("M-," . embark-act)
         ("M-." . embark-dwim)
         ("C-h C-b" . embark-bindings))

  :init
  ;; Setup and settings (before load)
  (setopt embark-confirm-act-all t)
  (setq-default prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Keybindings
  (keymap-unset embark-general-map "SPC")

  (keymap-set embark-general-map "t" #'embark-copy-as-kill)
  (keymap-set embark-general-map "C-o" #'embark-select)

  (keymap-set embark-file-map "F" #'find-file-other-window)
  (keymap-set embark-file-map "C-f" #'find-file-other-frame)
  (keymap-set embark-file-map "l" #'find-file-literally)

  (keymap-set embark-library-map "F" #'find-library-other-window)
  (keymap-set embark-library-map "C-f" #'find-library-other-window)

  (keymap-set embark-buffer-map "g" #'switch-to-buffer)
  (keymap-set embark-buffer-map "G" #'switch-to-buffer-other-window)
  (keymap-set embark-buffer-map "M-g" #'switch-to-buffer-other-frame)

  (keymap-set embark-identifier-map "f" #'xref-find-definitions)
  (keymap-set embark-identifier-map "F" #'xref-find-definitions-other-window)
  (keymap-set embark-identifier-map "C-f" #'xref-find-definitions-other-frame)

  (keymap-set embark-symbol-map "f" #'embark-find-definition)

  (keymap-set embark-package-map "f" #'describe-package)

  (keymap-set embark-become-file+buffer-map "F" #'find-file-other-window)
  (keymap-set embark-become-file+buffer-map "B" #'switch-to-buffer-other-window))

(use-package embark-consult
  :ensure t

  :after (embark consult))

;;; Tools
;;;; Project.el (built-in)
(use-package project
  :init
  ;; Setup and settings (before load)
  ;; Create and store file for known projects
  (defconst PROJECT_LIST_FILE (file-name-concat EMACS_DATA_DIR "projects.eld")
    "File where known project's are stored.")
  (setopt project-list-file PROJECT_LIST_FILE)
  (setopt project-mode-line t))

(use-package diff-hl
  :ensure t
  :pin melpa

  :init
  ;; Setup and settings (before load)
  (setopt diff-hl-global-modes '(not term-mode image-mode doc-view-mode pdf-view-mode))
  (setopt diff-hl-command-prefix (kbd "C-x v"))
  (setopt diff-hl-update-async t)
  (setq-default diff-hl-lighter " DiffHL")

  :config
  ;; Keybindings
  (keymap-set diff-hl-command-map "g" #'diff-hl-diff-goto-hunk)
  (keymap-set diff-hl-command-map "r" #'diff-hl-revert-hunk)
  (keymap-set diff-hl-command-map "p" #'diff-hl-previous-hunk)
  (keymap-set diff-hl-command-map "n" #'diff-hl-next-hunk)
  (keymap-set diff-hl-command-map "o" #'diff-hl-show-hunk)
  (keymap-set diff-hl-command-map "C-p" #'diff-hl-show-hunk-previous)
  (keymap-set diff-hl-command-map "C-n" #'diff-hl-show-hunk-next)
  (keymap-set diff-hl-command-map "s" #'diff-hl-stage-current-hunk)
  (keymap-set diff-hl-command-map "S" #'diff-hl-stage-dwim)
  (keymap-set diff-hl-command-map "m" #'diff-hl-stage-some)
  (keymap-set diff-hl-command-map "u" #'diff-hl-unstage-file)

  ;; Activation
  (global-diff-hl-mode 1))

(use-package magit
  :ensure t

  :bind (("C-x m" . magit-status)
         ("C-c m" . magit-dispatch)
         ("C-c f" . magit-file-dispatch))

  :init
  ;; Setup and settings (before load)
  (setopt magit-define-global-key-bindings nil)
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

  :config
  ;; Setup and settings (after load)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  ;; Keybindings
  (keymap-unset magit-mode-map "C-w" )
  (keymap-unset magit-mode-map "M-w")
  (keymap-unset magit-mode-map "C-c C-w")
  (keymap-set magit-mode-map "C-v" #'magit-visit-thing)
  (keymap-set magit-mode-map "C-t" #'magit-copy-section-value)
  (keymap-set magit-mode-map "C-S-t" #'magit-copy-buffer-revision)
  (keymap-set magit-mode-map "C-c C-t" #'magit-copy-thing)

  (keymap-set magit-diff-section-map "C-v" #'magit-diff-visit-worktree-file)

  (keymap-set magit-module-section-map "C-v" #'magit-submodule-visit)

  (keymap-set with-editor-mode-map "C-c C-v" #'with-editor-finish)
  (keymap-set with-editor-mode-map "C-c C-q" #'with-editor-cancel)

  ;; Activation
  (magit-wip-mode 1))

(use-package tex
  :ensure auctex

  :defer t

  :init
  ;; Setup and settings (before load)
  (setopt TeX-view-program-selection
          '(((output-dvi has-no-display-manager) "dvi2tty")
            ((output-dvi style-pstricks) "dvips and gv")
            (output-dvi "xdvi")
            (output-pdf "PDF Tools")
            (output-html "xdg-open")))

  (setopt TeX-PDF-mode t)

  (setopt TeX-master nil)

  (setopt TeX-parse-self t
          TeX-auto-regexp-list 'TeX-auto-full-regexp-list)

  (setopt TeX-file-line-error t
          TeX-display-help t)

  (setopt TeX-save-query t
          TeX-auto-save t
          TeX-auto-untabify t)

  (setopt TeX-source-correlate-method '((dvi . source-specials)
                                        (pdf . synctex)))

  :config
  ;; Hooks
  (add-hook 'TeX-language-en-hook (lambda () (jinx-languages "en_US")))
  (add-hook 'TeX-language-nl-hook (lambda () (jinx-languages "nl")))

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Activation
  (TeX-source-correlate-mode 1))

(use-package pdf-tools
  :ensure t

  :defer t

  :init
  ;; Setup and settings (before load)
  (setopt pdf-tools-handle-upgrades nil)
  (setopt pdf-view-display-size 'fit-page)
  (setopt pdf-view-use-unicode-ligther nil)

  (pdf-loader-install)

  :config
  ;; Setup and settings (after load)
  (add-to-list 'pdf-view-incompatible-modes 'display-line-numbers-mode)

  ;; Keybindings
  (keymap-set pdf-view-mode-map "q" #'kill-this-buffer)
  (keymap-set pdf-view-mode-map "<down>" #'pdf-view-next-line-or-next-page)
  (keymap-set pdf-view-mode-map "<up>" #'pdf-view-previous-line-or-previous-page)
  (keymap-set pdf-view-mode-map "n" #'pdf-view-next-page-command)
  (keymap-set pdf-view-mode-map "p" #'pdf-view-previous-page-command)
  (keymap-set pdf-view-mode-map "<next>" #'pdf-view-next-page-command)
  (keymap-set pdf-view-mode-map "<prior>" #'pdf-view-previous-page-command)
  (keymap-set pdf-view-mode-map "C-n" #'pdf-view-scroll-up-or-next-page)
  (keymap-set pdf-view-mode-map "C-p" #'pdf-view-scroll-down-or-previous-page)
  (keymap-set pdf-view-mode-map "SPC" #'pdf-view-scroll-up-or-next-page)
  (keymap-set pdf-view-mode-map "DEL" #'pdf-view-scroll-down-or-previous-page)
  (keymap-set pdf-view-mode-map "<backspace>" "DEL")
  (keymap-set pdf-view-mode-map "<end>" #'pdf-view-last-page)
  (keymap-set pdf-view-mode-map "<home>" #'pdf-view-first-page)
  (keymap-set pdf-view-mode-map "z" #'pdf-view-shrink)
  (keymap-set pdf-view-mode-map "Z" #'pdf-view-enlarge)
  (keymap-set pdf-view-mode-map "0" #'pdf-view-scale-reset)
  (keymap-set pdf-view-mode-map "r" #'pdf-view-rotate)
  (keymap-set pdf-view-mode-map "R" #'revert-buffer)
  (keymap-set pdf-view-mode-map "a c" #'pdf-view-center-in-window)
  (keymap-set pdf-view-mode-map "a l" #'pdf-view-align-left)
  (keymap-set pdf-view-mode-map "a r" #'pdf-view-align-right)
  (keymap-set pdf-view-mode-map "a w" #'pdf-view-fit-width-to-window)
  (keymap-set pdf-view-mode-map "a h" #'pdf-view-fit-height-to-window)
  (keymap-set pdf-view-mode-map "a p" #'pdf-view-fit-page-to-window)
  (keymap-set pdf-view-mode-map "m" #'pdf-view-position-to-register)
  (keymap-set pdf-view-mode-map "M" #'pdf-view-jump-to-register)
  (keymap-set pdf-view-mode-map "v d" #'pdf-view-dark-minor-mode)
  (keymap-set pdf-view-mode-map "v m" #'pdf-view-midnight-minor-mode)
  (keymap-set pdf-view-mode-map "v t" #'pdf-view-themed-minor-mode)
  (keymap-set pdf-view-mode-map "v p" #'pdf-view-printer-minor-mode)
  (keymap-set pdf-view-mode-map "C-l" #'pdf-view-goto-label)
  (keymap-set pdf-view-mode-map "C-;" #'pdf-view-goto-page)

  ;; Hooks
  (add-hook 'pdf-tools-enabled-hook #'(lambda ()
                                        (keymap-set pdf-annot-edit-contents-minor-mode-map
                                                    "C-c C-v"
                                                    #'pdf-annot-edit-contents-commit)))
  (add-hook 'pdf-tools-enabled-hook #'(lambda ()
                                        (keymap-unset pdf-sync-minor-mode-map "<double-mouse-1>"))))

(use-package org
  :ensure t

  :preface
  (defvar-keymap an-org-map
    :doc "Keymap for org"
    :prefix 'an-org-map-prefix)
  (keymap-global-set "C-c o" 'an-org-map-prefix)

  :bind (("C-c c" . org-capture)
         (:map an-org-map
               ("a" . org-agenda)
               ("c" . org-capture)
               ("l" . org-store-link)))

  :init
  ;; Setup and settings (before load)
  ;; Create and store org root directory
  (defconst ORG_DIR (file-name-as-directory
                     (if (getenv "XDG_DATA_HOME")
                         (file-name-concat (getenv "XDG_DATA_HOME") "org/")
                       "~/org/"))
    "Directory used as default location for org files.")
  (unless (file-directory-p ORG_DIR)
    (make-directory ORG_DIR t))

  ;; Create and store org calendar file
  (defconst ORG_CALENDAR_FILE (file-name-concat ORG_DIR "calendar.org")
    "Default file for calendar events created with org.")
  (unless (file-regular-p ORG_CALENDAR_FILE)
    (make-empty-file ORG_CALENDAR_FILE t))

  ;; Create and store org (default) notes file
  (defconst ORG_NOTES_FILE (file-name-concat ORG_DIR "notes.org")
    "Default file for notes created with org.")
  (unless (file-regular-p ORG_NOTES_FILE)
    (make-empty-file ORG_NOTES_FILE t))

  ;; Create and store org (default) todos file
  (defconst ORG_TODOS_FILE (file-name-concat ORG_DIR "todos.org")
    "Default file for todos created with org.")
  (unless (file-regular-p ORG_TODOS_FILE)
    (make-empty-file ORG_TODOS_FILE t))

  ;; Create and store org (default) todos file
  (defconst ORG_TODOS_FILE (file-name-concat ORG_DIR "todos.org")
    "Default file for todos created with org.")
  (unless (file-regular-p ORG_TODOS_FILE)
    (make-empty-file ORG_TODOS_FILE t))

  ;; Create and store org (default) meetings file
  (defconst ORG_MEETINGS_FILE (file-name-concat ORG_DIR "meetings.org")
    "Default file for todos created with org.")
  (unless (file-regular-p ORG_MEETINGS_FILE)
    (make-empty-file ORG_MEETINGS_FILE t))

  (setopt org-default-notes-file ORG_NOTES_FILE)

  (setopt org-replace-disputed-keys t
          org-disputed-keys '(([(shift up)] . [(meta p)])
                              ([(shift down)] . [(meta n)])
                              ([(shift left)] . [(meta b)])
                              ([(shift right)] . [(meta f)])
                              ([(control shift up)]	. [(control shift p)])
                              ([(control shift down)]	. [(control shift n)])
                              ([(control shift left)]	. [(control shift b)])
                              ([(control shift right)] . [(control shift f)])
                              ([(meta shift up)]	. [(meta shift p)])
                              ([(meta shift down)]	. [(meta shift n)])
                              ([(meta shift left)]	. [(meta shift b)])
                              ([(meta shift right)] . [(meta shift f)])))

  (setopt org-return-follows-link t)
  (setopt org-support-shift-select t)

  (setopt org-log-done 'time
          org-log-refile 'time)

  (setopt org-refile-allow-creating-parent-nodes 'confirm)

  (setopt org-tag-alist
          '((:startgroup)
            ("@work" . ?w) ("@home" . ?h) ("@online" . ?o) ("@elsewhere" . ?e)
            (:endgroup)
            (:startgroup) ("Project") (:grouptags) ("proj@.+" . ?p) (:endgroup)
            (:startgroup) ("Area") (:grouptags) ("area@.+" . ?a) (:endgroup)
            ("administration" . ?A) ("event" . ?E) ("family-and-friends". ?f) ("finance" . ?F)
            ("home" . ?H) ("matthias" . ?m) ("meeting" . ?M) ("miscellaneous". ?M) ("partner" . ?P)
            ("research" . ?r) ("resource" . ?R) ("study". ?s) ("teach" . ?t)
            ("travel" . ?T) ("work" . ?W)))

  (setopt org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE")))
  (setopt org-todo-keyword-faces
          '(("TODO" . (:inherit org-todo))
            ("IN-PROGRESS" . (:inherit org-cite))
            ("BLOCKED" . (:inherit org-warning))
            ("DONE" . (:inherit org-done))))

  (setopt org-capture-templates
          '(("n" "Note"
             entry (file+headline ORG_NOTES_FILE "Notes")
             "* %?\nCreated: %U"
             :empty-lines 0)
            ("N" "Note (tag + context)"
             entry (file+headline ORG_NOTES_FILE "Notes")
             "* %? %^g\nCreated: %U\n** Context: %a\n%i"
             :empty-lines 0)
            ("t" "Todo"
             entry (file+headline ORG_TODOS_FILE "Tasks")
             "* TODO [#B] %?\nCreated: %U"
             :empty-lines 0)
            ("T" "Todo (tag + context)"
             entry (file+headline ORG_TODOS_FILE "Tasks")
             "* TODO [#B] %? %^g\nCreated: %U\n** Context: %a\n%i"
             :empty-lines 0)
            ("d" "Todo with deadline"
             entry (file+headline ORG_TODOS_FILE "Tasks")
             "* TODO [#B] %?\nCreated: %U\nDeadline: %^T"
             :empty-lines 0)
            ("D" "Todo with deadline (tag + context)"
             entry (file+headline ORG_TODOS_FILE "Tasks")
             "* TODO [#B] %? %^g\nCreated: %U\nDeadline: %^T\n** Context: %a\n%i"
             :empty-lines 0)
            ("c" "Calendar event"
             entry (file+headline ORG_CALENDAR_FILE "Events")
             "* %?\nCreated: %U\nStart: %^T\nEnd: %^T"
             :empty-lines 0)
            ("C" "Calendar event (tag + context)"
             entry (file+headline ORG_CALENDAR_FILE "Events")
             "* %? %^g\nCreated: %U\nStart: %^T\nEnd: %^T\n** Context:\n%i"
             :empty-lines 0)
            ("m" "Meeting"
             entry (file+olp+datetree ORG_MEETINGS_FILE)
             "* %? :meeting:%^g\nCreated: %U\n** Notes:%i\n** Action Items:\n*** TODO [#B] "
             :tree-type week
             :clock-in t
             :clock-resume t
             :empty-lines 0)))

  :config
  ;; Setup and settings (after load)
  (add-to-list 'org-agenda-files ORG_TODOS_FILE)

  ;; Keybindings
  (keymap-set org-mode-map "S-<return>" #'org-return-and-maybe-indent)

  (with-eval-after-load 'org-capture
    (keymap-set org-capture-mode-map "C-c C-v" #'org-capture-finalize))

  (keymap-unset org-read-date-minibuffer-local-map "C-v")
  (keymap-unset org-read-date-minibuffer-local-map "M-v")
  (keymap-set org-read-date-minibuffer-local-map "C-<" #'org-calendar-scroll-three-months-left)
  (keymap-set org-read-date-minibuffer-local-map "C->" #'org-calendar-scroll-three-months-right)

  ;; Hooks
  (add-hook 'org-mode-hook #'loc-setup-mix-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-capture-mode-hook #'loc-setup-mix-mode))

(use-package markdown-mode
  :ensure t

  :defer t

  :mode ("README\\.md\\'" . gfm-mode)

  :init
  ;; Setup and settings (before load)
  (setopt markdown-enable-math t
          markdown-enable-html t
          markdown-enable-highlighting-syntax t)
  (setopt markdown-footnote-location 'immediately)
  (setopt markdown-gfm-use-electric-backquote nil)
  (setopt markdown-edit-code-block-default-mode 'prog-mode
          markdown-fontify-code-blocks-natively t
          markdown-fontify-code-block-default-mode 'python-mode)
  (setopt markdown-fontify-whole-heading-line t
          markdown-header-scaling t
          markdown-header-scaling t)
  (setopt markdown-special-ctrl-a/e t)

  :config
  ;; Keybindings
  (keymap-set markdown-mode-command-map "t" #'markdown-kill-ring-save)
  (keymap-set markdown-mode-command-map "z" #'markdown-table-transpose)

  (keymap-set markdown-view-mode-map "<prior>" #'scroll-up-command)
  (keymap-set markdown-view-mode-map "<next>" #'scroll-down-command)
  (keymap-set markdown-view-mode-map "<home>" #'beginning-of-buffer)
  (keymap-set markdown-view-mode-map "<end>" #'end-of-buffer))


;;; Developmentk
;;;; OCaml
(use-package tuareg
  :ensure t
  :pin melpa

  :defer t

  :init
  ;; Setup and settings (before load)
  (setopt tuareg-electric-indent nil
          tuareg-electric-close-vector nil)

  (setopt tuareg-highlight-all-operators t))

(use-package merlin
  :ensure t
  :pin melpa

  :hook (tuareg-mode caml-mode)

  :config
  ;; Keybindings
  (keymap-unset merlin-type-enclosing-map "C-<up>")
  (keymap-unset merlin-type-enclosing-map "C-<down>")
  (keymap-unset merlin-type-enclosing-map "C-w")

  (keymap-set merlin-type-enclosing-map "C-p" #'merlin-type-enclosing-go-up)
  (keymap-set merlin-type-enclosing-map "C-n" #'merlin-type-enclosing-go-down)
  (keymap-set merlin-type-enclosing-map "C-t" #'merlin-copy-enclosing))

;;;; Proof General (EasyCrypt)
;;;; Note, proof.el (which is provided by the proof-general package) is what is
;;;; actually loaded by the proof assistants, not proof-general.el.
;;;; Hence, we use `use-package proof :ensure proof-general` to
;;;; use deferred loading as usual
(use-package proof
  :ensure proof-general
  :pin melpa

  :defer t

  :init
  ;; Setup and settings (before load)
  ;;; General
  (setopt proof-splash-enable nil
          proof-toolbar-enable nil)
  (setopt proof-delete-empty-windows nil
          proof-shrink-windows-tofit nil
          proof-output-tooltips t)
  (setopt proof-electric-terminator-enable nil
          proof-sticky-errors t
          proof-disappearing-proofs t
          proof-prog-name-ask nil
          proof-minibuffer-messages t
          proof-next-command-insert-space nil
          proof-keep-response-history t
          pg-input-ring-size 32
          proof-follow-mode 'locked
          proof-auto-action-when-deactivating-scripting 'retract)
  (setopt bufhist-ring-size 32)
  ;;; EasyCrypt
  (setopt easycrypt-script-indent nil
          easycrypt-one-command-per-line nil)
  (setopt easycrypt-prog-name "easycrypt")

  :config
  ;; Setup and settings (after load)
  (defun setup-a-proof-response-mode ()
    (toggle-truncate-lines -1)
    (toggle-word-wrap 1))
  (defun setup-a-proof-goals-mode ()
    (toggle-truncate-lines -1)
    (toggle-word-wrap -1))

  ;; Keybindings
  (defvar-keymap a-proof-mode-process-repeat-map
    :doc "Keymap (repeatable) for processing proof commands"
    :repeat (:hints ((proof-undo-last-successful-command . "p/u: Undo last succesful command")
                     (proof-assert-next-command-interactive . "n: Assert next command")
                     (proof-undo-and-delete-last-successful-command . "d: Undo and delete last successful command")))
    "p" #'proof-undo-last-successful-command
    "u" #'proof-undo-last-successful-command
    "n" #'proof-assert-next-command-interactive
    "d" #'proof-undo-and-delete-last-successful-command)
  (defvar-keymap a-bufhist-repeat-map
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
    (keymap-unset proof-mode-map "M-a")
    (keymap-unset proof-mode-map "M-e")
    (keymap-unset proof-mode-map "C-M-<up>")
    (keymap-unset proof-mode-map "C-M-<down>")
    (keymap-set proof-mode-map "C-p" #'proof-undo-last-successful-command)
    (keymap-set proof-mode-map "C-n" #'proof-assert-next-command-interactive)
    (keymap-set proof-mode-map "C-<prior>" #'proof-goto-command-start)
    (keymap-set proof-mode-map "C-<next>" #'proof-goto-command-end)
    (keymap-set proof-mode-map "C-<end>" #'proof-goto-end-of-locked)
    (keymap-set proof-mode-map "C-c C-v" #'proof-goto-point)
    (keymap-set proof-mode-map "C-c C-d" #'proof-undo-and-delete-last-successful-command)
    (keymap-set proof-mode-map "C-c C-a" #'proof-goto-command-start)
    (keymap-set proof-mode-map "C-c C-e" #'proof-goto-command-end)
    (keymap-set proof-mode-map "C-c C-l" #'proof-goto-end-of-locked)
    (keymap-set proof-mode-map "C-c C-w" #'proof-layout-windows)
    (keymap-set proof-mode-map "C-c C-o" #'proof-display-some-buffers)
    (keymap-set proof-mode-map "C-c C-k" #'pg-response-clear-displays)
    (keymap-set proof-mode-map "C-c C-x" #'proof-minibuffer-cmd)
    (keymap-set proof-mode-map "C-c C-q" #'proof-shell-exit)
    (keymap-set proof-mode-map "M-P" #'pg-previous-matching-input-from-input)
    (keymap-set proof-mode-map "M-N" #'pg-next-matching-input-from-input)
    (keymap-set proof-mode-map "C-M-p" #'pg-previous-input)
    (keymap-set proof-mode-map "C-M-n" #'pg-next-input)
    (keymap-set proof-mode-map "C-M-S-p" #'pg-previous-matching-input)
    (keymap-set proof-mode-map "C-M-S-n" #'pg-next-matching-input))
  (defun setup-a-proof-response-mode-map ()
    (keymap-set proof-response-mode-map "C-q" #'bury-buffer)
    (keymap-set proof-response-mode-map "C-c C-d" #'proof-undo-and-delete-last-successful-command)
    (keymap-set proof-response-mode-map "C-c C-e" #'proof-next-error)
    (keymap-set proof-response-mode-map "C-c C-w" #'proof-layout-windows)
    (keymap-set proof-response-mode-map "C-c C-o" #'proof-display-some-buffers)
    (keymap-set proof-response-mode-map "C-c C-k" #'pg-response-clear-displays)
    (keymap-set proof-response-mode-map "C-c C-x" #'proof-minibuffer-cmd)
    (keymap-set proof-response-mode-map "C-c C-q" #'proof-shell-exit))
  (defun setup-a-proof-goals-mode-map ()
    (keymap-set proof-goals-mode-map "C-q" #'bury-buffer)
    (keymap-set proof-goals-mode-map "C-c C-d" #'proof-undo-and-delete-last-successful-command)
    (keymap-set proof-goals-mode-map "C-c C-e" #'proof-next-error)
    (keymap-set proof-goals-mode-map "C-c C-w" #'proof-layout-windows)
    (keymap-set proof-goals-mode-map "C-c C-o" #'proof-display-some-buffers)
    (keymap-set proof-goals-mode-map "C-c C-k" #'pg-response-clear-displays)
    (keymap-set proof-goals-mode-map "C-c C-x" #'proof-minibuffer-cmd)
    (keymap-set proof-goals-mode-map "C-c C-q" #'proof-shell-exit))

  ;; Hooks
  (add-hook 'proof-mode-hook #'setup-a-proof-mode-map)
  (add-hook 'proof-mode-hook #'setup-a-bufhist-map)

  (add-hook 'proof-response-mode-hook #'setup-a-proof-response-mode)
  (add-hook 'proof-response-mode-hook #'setup-a-proof-response-mode-map)

  (add-hook 'proof-goals-mode-hook #'setup-a-proof-goals-mode)
  (add-hook 'proof-goals-mode-hook #'setup-a-proof-goals-mode-map)

  ;; Custom functionality
  ;;; Remove bufhist buttons
  (defun silence-bufhist-insert-buttons (orig-fun &rest args)
    (setq-local bufhist-top-point (point-min)))

  (advice-add 'bufhist-insert-buttons :around #'silence-bufhist-insert-buttons))

;;; Themes
;;;; Doom-themes (general)
(use-package doom-themes
  :ensure t

  :defer t

  :init
  ;; Setup and settings (before load)
  (setopt doom-themes-enable-bold t
          doom-themes-enable-italic t)

  :config
  ;; Setup and settings (after load)
  (doom-themes-visual-bell-config)

  (doom-themes-set-faces nil
    '(vertico-current :foreground 'unspecified :background 'unspecified
                      :inherit 'highlight)
    '(vertico-mouse :foreground 'unspecified :background 'unspecified
                    :inherit 'lazy-highlight)
    '(vertico-quick1 :foreground 'unspecified :background 'unspecified
                     :inherit 'avy-lead-face)
    '(vertico-quick2 :foreground 'unspecified :background 'unspecified
                     :inherit 'avy-lead-face-1)
    '(corfu-border :foreground 'unspecified :background 'unspecified
                   :inherit 'vertical-border)
    '(corfu-bar :foreground 'unspecified :background 'unspecified
                :inherit 'scroll-bar)
    '(corfu-quick1 :foreground 'unspecified :background 'unspecified
                   :inherit 'avy-lead-face)
    '(corfu-quick2 :foreground 'unspecified :background 'unspecified
                   :inherit 'avy-lead-face-1)
    '(tempel-default :foreground 'unspecified :background 'unspecified
                     :inherit 'secondary-selection :slant 'italic)
    '(tempel-field :foreground 'unspecified :background 'unspecified
                   :inherit 'lazy-highlight)
    '(tempel-form :foreground 'unspecified :background 'unspecified
                  :inherit 'match)
    '(proof-mouse-highlight-face :inherit 'lazy-highlight)
    '(proof-region-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
    '(proof-command-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
    '(proof-active-area-face :inherit 'secondary-selection)))

;;;; Doom-themes specific
;;;;; Nord <3
(use-package doom-nord-theme
  ;; :disabled t ; Don't use this theme
  :demand t ; Use this theme

  :ensure nil ; Provided by doom-themes

  :init
  ;; Setup and settings (before load)
  (setopt doom-nord-brighter-modeline t
          doom-nord-brighter-comments nil
          doom-nord-comment-bg nil
          doom-nord-padded-modeline nil
          doom-nord-region-highlight 'snowstorm)

  :config
  ;; Setup and settings (after load)
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
    '(proof-highlight-dependency-name-face :foreground orange)
    '(proof-declaration-name-face :foreground cyan)
    '(proof-tacticals-name-face :foreground green)
    '(proof-tactics-name-face :foreground violet)
    '(proof-error-face :foreground red :weight 'semi-bold)
    '(proof-warning-face :foreground yellow :weight 'semi-bold)
    '(proof-debug-message-face :foreground orange)
    '(proof-boring-face :foreground base5)
    '(proof-eager-annotation-face :inherit 'proof-warning-face :weight 'normal)
    '(easycrypt-tactics-tacticals-face :inherit 'proof-tacticals-name-face)
    '(easycrypt-tactics-closing-face :foreground yellow)
    '(easycrypt-tactics-dangerous-face :foreground red))

  ;;; Dummy face definitions
  ;;; (applying theme settings that inherit from these faces
  ;;; without explicitly loading the packages that apply
  ;;; initially define them)
  (defface avy-lead-face '((t . (:inherit default)))
    "Dummy definition for avy-lead-face")
  (defface avy-lead-face-1 '((t . (:inherit default)))
    "Dummy definition for avy-lead-face-1")

  ;; Hooks
  (add-hook 'after-init-hook #'(lambda () (unless (daemonp) (enable-theme 'doom-nord)))))

(use-package doom-nord-light-theme
  :disabled t ; Don't use this theme
  ;; :demand t ; Use this theme

  :ensure nil ; Provided by doom-themes

  :after doom-themes

  :init
  ;; Setup and settings (before load)
  (setopt doom-nord-light-brighter-modeline t
          doom-nord-light-brighter-comments nil
          doom-nord-light-padded-modeline nil
          doom-nord-light-comment-bg t
          doom-nord-light-region-highlight 'frost)

  :config
  ;; Setup and settings (after load)
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
    '(proof-highlight-dependency-name-face :foreground orange)
    '(proof-declaration-name-face :foreground cyan)
    '(proof-tacticals-name-face :foreground green)
    '(proof-tactics-name-face :foreground violet)
    '(proof-error-face :foreground red :weight 'semi-bold)
    '(proof-warning-face :foreground yellow :weight 'semi-bold)
    '(proof-debug-message-face :foreground orange)
    '(proof-boring-face :foreground base5)
    '(proof-eager-annotation-face :inherit 'proof-warning-face :weight 'normal)
    '(easycrypt-tactics-tacticals-face :inherit 'proof-tacticals-name-face)
    '(easycrypt-tactics-closing-face :foreground yellow)
    '(easycrypt-tactics-dangerous-face :foreground red))

  ;; Hooks
  (add-hook 'after-init-hook #'(lambda () (unless (daemonp) (enable-theme 'doom-nord-light)))))

(use-package solaire-mode
  :ensure t

  :hook (after-init . solaire-global-mode))

(use-package nerd-icons
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t

  :hook after-init

  :init
  ;; Setup and settings (before load)
  (setopt doom-modeline-height (+ (frame-char-height) 4)
          doom-modeline-bar-width 4
          doom-modeline-width-limit 100
          doom-modeline-icon t
          doom-modeline-major-mode-icon nil
          doom-modeline-minor-modes t
          doom-modeline-enable-word-count t
          doom-modeline-buffer-encoding t
          doom-modeline-default-coding-system 'utf-8
          doom-modeline-total-line-number t
          doom-modeline-time t
          doom-modeline-vcs-max-length 20))

;; Local/cross-package enhancements
(use-package loc-avy
  :ensure nil ; Provided locally

  :after avy

  :commands (avy-action-embark-act avy-action-embark-dwim)

  :init
  ;; Setup and settings (before load of this package, but after load of packages listed in `:after`)
  (add-to-list 'avy-dispatch-alist '(?, . avy-action-embark-act) t)
  (add-to-list 'avy-dispatch-alist '(?. . avy-action-embark-dwim) t))


;;; Corfu + Orderless
(use-package corfu
  :after orderless

  :config
  ;; Hooks
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local completion-styles '(orderless-literal-only basic)))))

;;; Corfu + Vertico
(use-package corfu
  :after vertico

  :init
  ;; Setup and settings (before load of this package, but after load of packages listed in `:after')
  (setopt global-corfu-minibuffer
          (lambda ()
            (not (or (bound-and-true-p vertico--input)
                     (eq (current-local-map) read-passwd-map))))))

;;; Tempel + Cape
(use-package tempel
  :after cape

  :config
  ;; Custom functionality
  (defalias 'cape-tempel-complete-prefix-2 (cape-capf-prefix-length #'tempel-complete 2))

  (defun setup-a-cape-tempel-mix-mode ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-tempel-complete-prefix-2
                                       #'cape-abbrev-prefix-2
                                       #'cape-keyword-prefix-2
                                       #'cape-dabbrev-prefix-2)
                      #'cape-dict-prefix-2)))

  (defun setup-a-cape-tempel-code-mode ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-tempel-complete-prefix-2
                                       #'cape-abbrev-prefix-2
                                       #'cape-keyword-prefix-2
                                       #'cape-dabbrev-prefix-2))))

  (defun setup-a-cape-tempel-minibuffer ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-tempel-complete-prefix-2
                                       #'cape-abbrev-prefix-2
                                       #'cape-history-prefix-2 #'cape-file-prefix-2
                                       #'cape-dabbrev-prefix-2))))

  ;; Hooks (replace regular by tempel-including)
  (remove-hook 'tex-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'tex-mode-hook #'setup-a-cape-tempel-mix-mode)
  (remove-hook 'TeX-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'TeX-mode-hook #'setup-a-cape-tempel-mix-mode)
  (remove-hook 'conf-mode-hook #'setup-a-cape-mix-mode)
  (add-hook 'conf-mode-hook #'setup-a-cape-tempel-mix-mode)
  (remove-hook 'prog-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'prog-mode-hook #'setup-a-cape-tempel-code-mode)
  (remove-hook 'minibuffer-setup-hook #'setup-a-cape-minibuffer)
  (add-hook 'minibuffer-setup-hook #'setup-a-cape-tempel-minibuffer))

;;; EasyCrypt (extension)
(use-package easycrypt-ext
  :ensure nil ; Provided locally

  :after proof

  :hook ((easycrypt-mode . easycrypt-ext-mode)
         (easycrypt-goals-mode . easycrypt-ext-goals-mode)
         (easycrypt-response-mode . easycrypt-ext-response-mode))

  :init
  ;; Setup and settings (before load of this package, but after load of packages listed in `:after')
  (setopt ece-indentation t)
  (setopt ece-keyword-completion t)
  (setopt ece-templates t)
  (setopt ece-templates-info nil)

  :config
  ;; Keybindings
  (keymap-set easycrypt-ext-mode-map "C-c C-p" #'ece-print)
  (keymap-set easycrypt-ext-mode-map "C-c l p" #'ece-print)
  (keymap-set easycrypt-ext-mode-map "C-c l P" #'ece-print-prompt)
  (keymap-set easycrypt-ext-mode-map "C-c l l" #'ece-locate)
  (keymap-set easycrypt-ext-mode-map "C-c l L" #'ece-locate-prompt)
  (keymap-set easycrypt-ext-mode-map "C-c C-s" #'ece-search)
  (keymap-set easycrypt-ext-mode-map "C-c l s" #'ece-search)
  (keymap-set easycrypt-ext-mode-map "C-c l S" #'ece-search-prompt)
  (keymap-set easycrypt-ext-mode-map "C-c l o" 'ece-options-map-prefix)
  (keymap-set easycrypt-ext-mode-map "C-c C-t" 'ece-template-map-prefix)
  (keymap-set easycrypt-ext-mode-map "C-c l t" 'ece-template-map-prefix)
  (keymap-set easycrypt-ext-mode-map "C-c C-e" 'ece-exec-map-prefix)
  (keymap-set easycrypt-ext-mode-map "C-c l e" 'ece-exec-map-prefix)

  (keymap-set easycrypt-ext-goals-mode-map "C-c C-p" #'ece-print)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l p" #'ece-print)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l P" #'ece-prompt-print)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l l" #'ece-prompt-locate)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l L" #'ece-prompt-locate)
  (keymap-set easycrypt-ext-goals-mode-map "C-c C-s" #'ece-search)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l s" #'ece-search)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l S" #'ece-prompt-search)
  (keymap-set easycrypt-ext-goals-mode-map "C-c C-e" 'ece-exec-map-prefix)
  (keymap-set easycrypt-ext-goals-mode-map "C-c l e" 'ece-exec-map-prefix)

  (keymap-set easycrypt-ext-response-mode-map "C-c C-p" #'ece-print)
  (keymap-set easycrypt-ext-response-mode-map "C-c l p" #'ece-print)
  (keymap-set easycrypt-ext-response-mode-map "C-c l P" #'ece-print-prompt)
  (keymap-set easycrypt-ext-response-mode-map "C-c l l" #'ece-locate)
  (keymap-set easycrypt-ext-response-mode-map "C-c l L" #'ece-locate-prompt)
  (keymap-set easycrypt-ext-response-mode-map "C-c C-s" #'ece-search)
  (keymap-set easycrypt-ext-response-mode-map "C-c l s" #'ece-search)
  (keymap-set easycrypt-ext-response-mode-map "C-c l S" #'ece-search-prompt)
  (keymap-set easycrypt-ext-response-mode-map "C-c C-e" 'ece-exec-map-prefix)
  (keymap-set easycrypt-ext-response-mode-map "C-c l e" 'ece-exec-map-prefix))


;; Hooks
;;; Frames/windows
(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'loc-setup-client-frame)
  (add-hook 'after-make-frame-functions #'loc-setup-frame))

(add-hook 'minibuffer-setup-hook #'loc-setup-mini-mix-mode)

;;; Modes
(add-hook 'text-mode-hook #'loc-setup-text-mode)

(add-hook 'tex-mode-hook #'loc-setup-mix-mode)
(add-hook 'TeX-mode-hook #'loc-setup-mix-mode)
(add-hook 'markdown-mode-hook #'loc-setup-mix-mode)
(add-hook 'conf-mode-hook #'loc-setup-mix-mode)
(add-hook 'log-edit-mode-hook #'loc-setup-mix-mode)
(add-hook 'prog-mode-hook #'loc-setup-code-mode)
