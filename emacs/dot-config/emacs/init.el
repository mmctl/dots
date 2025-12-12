;; -*- lexical-binding: t; -*-
;; init.el
;;; Environment
;; Config
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

;; Data
(defconst EMACS_DATA_DIR (file-name-as-directory
                          (if (getenv "XDG_DATA_HOME")
                              (file-name-concat (getenv "XDG_DATA_HOME") "emacs/")
                            user-emacs-directory))
  "Directory where (additional) Emacs data is stored.")

(defconst BACKUPS_DIR (file-name-as-directory (file-name-concat EMACS_DATA_DIR "backups/"))
  "Directory where (automatically generated) backup files are stored.")

;; Cache
(defconst EMACS_CACHE_DIR (file-name-as-directory
                           (if (getenv "XDG_CACHE_HOME")
                               (file-name-concat (getenv "XDG_CACHE_HOME") "emacs/")
                             user-emacs-directory))
  "Directory where Emacs cache is stored.")

(defconst AUTOSAVES_DIR (file-name-as-directory (file-name-concat EMACS_CACHE_DIR "autosaves/"))
  "Directory where auto-save files are stored.")

(defconst LOCKS_DIR (file-name-as-directory (file-name-concat EMACS_CACHE_DIR "locks/"))
  "Directory where lock files are stored.")

;;; Bootstrap
;; Directories
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

;; Custom file
(unless (file-exists-p CUSTOM_FILE)
  (make-empty-file CUSTOM_FILE))
(setopt custom-file CUSTOM_FILE)
(load CUSTOM_FILE)

;; Load path/pointers
;; Add LOCAL_DIR and its sub-directories to load path, excluding hidden ones,
;; and generate autoloads if possible
(dolist (file (cons LOCAL_DIR (directory-files-recursively LOCAL_DIR "^[^.].*" t t)))
  (when (file-directory-p file)
    (add-to-list 'load-path file)
    (let ((fileal (expand-file-name (concat (file-name-nondirectory (directory-file-name file)) "-autoloads.el")
                                    file)))
      (loaddefs-generate file fileal)
      (load fileal))))

;; Byte (and, if possible, natively) compile all local Elisp files (in LOCAL_DIR and subdirectories)
(byte-recompile-directory LOCAL_DIR 0 nil t)
(when (native-comp-available-p)
  (native-compile-async (list LOCAL_DIR) t))

(add-to-list 'load-path MISC_DIR)

(setopt custom-theme-directory THEMES_DIR)

;; Backups
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

;; Auto-saves
(setopt auto-save-file-name-transforms `((".*" ,(file-name-concat AUTOSAVES_DIR "\\1") t))
        auto-save-visited-file-name nil
        auto-save-interval 50
        auto-save-timeout 20
        auto-save-default t
        delete-auto-save-files t
        auto-save-list-file-prefix nil)

;; Locks
(setopt lock-file-name-transforms `((".*" ,(file-name-concat LOCKS_DIR "\\1") t))
        create-lockfiles t)

;; Custom local functionalities
(require 'local-setup)
(require 'local-utils)

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


;;; Settings (general/UI)
;; Launching
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'emacs-lisp-mode)
(setopt initial-scratch-message "")

;; Frames/Windows
(setopt frame-resize-pixelwise t)
(setopt window-resize-pixelwise t)

(setopt switch-to-buffer-obey-display-actions t)
(setopt uniquify-buffer-name-style 'forward)
(setopt highlight-nonselected-windows nil)

(line-number-mode 1)
(column-number-mode 1)
(setopt mode-line-percent-position nil)

(setopt display-time-format "%a, %b %d | %H:%M")
(setopt display-time-day-and-date t)
(setopt display-time-24hr-format t)
(setopt display-time-default-load-average nil)
(display-time-mode 1)

(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook #'local-setup-client-frame)
      (add-hook 'after-make-frame-functions #'local-setup-frame))
  (add-hook 'after-init-hook #'local-setup-global-frame))

;; Modes
(add-hook 'text-mode-hook #'local-setup-text-mode)
(add-hook 'tex-mode-hook #'local-setup-code-mode)
(add-hook 'TeX-mode-hook #'local-setup-code-mode)
(add-hook 'markdown-mode-hook #'local-setup-code-mode)
(add-hook 'conf-mode-hook #'local-setup-code-mode)
(add-hook 'log-edit-mode-hook #'local-setup-code-mode)
(add-hook 'prog-mode-hook #'local-setup-code-mode)

;; (Mini)Buffers
(setopt minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setopt read-extended-command-predicate #'command-completion-default-include-p)
(setopt enable-recursive-minibuffers t)

(setopt resize-mini-windows 'grow-only)

(setopt echo-keystrokes 0.02)

;; Cursor
(setopt x-stretch-cursor nil)
(setopt blink-matching-paren t)
(setopt cursor-in-non-selected-windows nil)

(setopt make-pointer-invisible nil)

(blink-cursor-mode -1)

;; Scrolling/Mouse
(setopt hscroll-margin 0)
(setopt hscroll-step 1)
(setopt scroll-conservatively 101)
(setopt scroll-margin 0)
(setopt scroll-preserve-screen-position t)
(setopt auto-window-vscroll nil)
(setopt fast-but-imprecise-scrolling t)

(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)
(setopt mouse-yank-at-point t)
(setopt mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
(setopt mouse-wheel-scroll-amount-horizontal 2)

(setopt pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode 1)

;; Editor/interaction
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

(setopt shift-select-mode nil)
(repeat-mode 1)


;; Miscellaneous
(setq-default bidi-display-reordering 'left-to-right)
(setopt bidi-paragraph-direction 'left-to-right)

(setopt sentence-end-double-space nil)
(setopt x-underline-at-descent-line nil)

(savehist-mode 1)
(recentf-mode 1)


;;; Keybindings (general)
;; Translations
(keymap-set function-key-map "C-S-<iso-lefttab>" "C-<backtab>")
(keymap-set function-key-map "M-S-<iso-lefttab>" "M-<backtab>")
(keymap-set function-key-map "C-M-S-<iso-lefttab>" "C-M-<backtab>")

;; Movement
(keymap-global-set "C-M-<left>" #'windmove-left)
(keymap-global-set "C-M-<down>" #'windmove-down)
(keymap-global-set "C-M-<up>" #'windmove-up)
(keymap-global-set "C-M-<right>" #'windmove-right)

(keymap-global-set "C-M-S-<left>" #'windmove-swap-states-left)
(keymap-global-set "C-M-S-<down>" #'windmove-swap-states-down)
(keymap-global-set "C-M-S-<up>" #'windmove-swap-states-up)
(keymap-global-set "C-M-S-<right>" #'windmove-swap-states-right)

(keymap-global-set "C-p" #'backward-sexp) ; from: previous-line
(keymap-global-set "C-n" #'forward-sexp)  ; from: next-line

(keymap-global-set "<home>" #'beginning-of-buffer) ; from: beginning-of-line
(keymap-global-set "<end>" #'end-of-buffer)        ; from: end-of-line

(keymap-global-set "C-a" #'move-beginning-of-line-or-indentation)
(keymap-global-set "C-e" #'move-end-of-line-or-whitespace)
(keymap-global-set "M-b" #'duplicate-line-or-lines-in-region) ; from: backward-word

(keymap-global-set "C-`" #'push-mark-no-activate)
(keymap-global-set "M-`" #'pop-to-mark-command)
(keymap-global-set "M-~" #'pop-global-mark) ; from: not-modified
(keymap-global-set "<remap> <exchange-point-and-mark>" #'exchange-point-and-mark-invert)

;; Selection
(keymap-global-set "M-h" #'mark-word) ; from: mark-paragraph

;; Manipulation
(keymap-global-set "M-W" #'kill-ring-save-line)

(keymap-global-set "C-S-y" #'yank-whole-line)

(keymap-global-set "M-<delete>" #'kill-word)
(keymap-global-set "M-D" #'kill-whole-symbol)

(keymap-global-set "C-S-<backspace>" #'backward-kill-line) ; from: kill-whole-line
(keymap-global-set "C-S-<delete>" #'kill-line)
(keymap-global-set "C-S-k" #'kill-whole-line-back-to-indentation)

(keymap-global-set "M-<up>" #'move-it-up)
(keymap-global-set "M-<down>" #'move-it-down)
(keymap-global-set "M-<left>" #'move-it-left)
(keymap-global-set "M-<right>" #'move-it-right)

;; Deleting
(keymap-global-set "M-S-SPC" #'delete-all-space)

;; Miscellaneous
(keymap-global-set "M-t" #'exchange-word) ; from: transpose-words
(keymap-global-set "M-T" #'exchange-word-backward)

(keymap-global-set "C-;" #'comment-line)

;; Management
;; Quitting
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

;; Frames
(defvaralias 'a-frame-map 'ctl-x-5-map
  "Keymap for frame management")
(defalias 'a-frame-map-prefix #'ctl-x-5-prefix
  "Prefix for frame management keymap")

(keymap-set a-frame-map "k" #'delete-frame)
(keymap-set a-frame-map "K" #'delete-other-frames)
(keymap-set a-frame-map "n" #'make-frame-command)
(keymap-set a-frame-map "o" #'other-frame)
(keymap-set a-frame-map "u" #'fit-frame-to-buffer)
(keymap-set a-frame-map "z" #'suspend-frame)

(keymap-set ctl-x-map "+" 'a-frame-map-prefix)

;; Windows
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

(defvaralias 'a-window-map 'ctl-x-4-map
  "Keymap for window management")
(defalias 'a-window-map-prefix #'ctl-x-4-prefix
  "Prefix for window management keymap")

(keymap-set a-window-map "e" #'balance-windows)
(keymap-set a-window-map "h" #'shrink-window)
(keymap-set a-window-map "H" #'enlarge-window)
(keymap-set a-window-map "k" #'delete-window)
(keymap-set a-window-map "K" #'delete-other-windows)
(keymap-set a-window-map "C-k" #'kill-buffer-and-window)
(keymap-set a-window-map "M-k" #'delete-windows-on)
(keymap-set a-window-map "s" #'split-window-horizontally)
(keymap-set a-window-map "S" #'split-window-vertically)
(keymap-set a-window-map "o" #'other-window)
(keymap-set a-window-map "t" #'tear-off-window)
(keymap-set a-window-map "u" #'fit-window-to-buffer)
(keymap-set a-window-map "w" #'shrink-window-horizontally)
(keymap-set a-window-map "W" #'enlarge-window-horizontally)
(keymap-set a-window-map "<left>" #'windmove-left)
(keymap-set a-window-map "<down>" #'windmove-down)
(keymap-set a-window-map "<up>" #'windmove-up)
(keymap-set a-window-map "<right>" #'windmove-right)

(keymap-set ctl-x-map "w" 'a-window-map-prefix)

;; Buffers
(defvar-keymap a-buffer-map
  :doc "Keymap for buffer management"
  :prefix 'a-buffer-map-prefix
  "k" #'kill-current-buffer
  "K" #'kill-some-buffers
  "C-k" #'kill-buffer
  "M-k" #'kill-buffer-and-window
  "g" #'switch-to-buffer
  "G" #'switch-to-buffer-other-window
  "M-g" #'switch-to-buffer-other-frame
  "m" #'switch-to-minibuffer
  "p" #'project-switch-to-buffer
  "s" #'save-buffer
  "S" #'save-some-buffers
  "r" #'revert-buffer
  "R" #'revert-buffer-quick)

(keymap-global-set "C-x b" 'a-buffer-map-prefix)

;; Finding/going/searching/replacing
(defvar-keymap a-find-map
  :doc "Keymap for finding (i.e., searching, but more meta)"
  :prefix 'a-find-map-prefix
  "d" #'dired
  "D" #'dired-jump
  "C-d" #'find-dired
  "M-d" #'dired-as-root
  "C-S-d" #'dired-default-directory-as-root
  "f" #'find-file
  "F" #'find-file-other-window
  "C-f" #'find-file-other-frame
  "M-f" #'find-file-as-root
  "C-S-f" #'reopen-file-as-root
  "l" #'find-library
  "L" #'find-library-other-window
  "C-l" #'find-library-other-frame
  "o" #'find-file-read-only
  "C-o" #'find-file-read-only-other-window
  "M-o" #'find-file-read-only-other-frame
  "r" #'recentf-open
  "x a" #'xref-find-apropos
  "x d" #'xref-find-definitions
  "x r" #'xref-find-references)

(keymap-global-set "M-f" 'a-find-map-prefix)

(keymap-global-set "C-x f" #'find-file)
(keymap-global-set "C-x F" #'find-file-other-window)
(keymap-global-set "C-x C-f" #'find-file-other-frame)
(keymap-global-set "C-x M-f" #'find-file-as-root)
(keymap-global-set "C-x C-r" #'recentf-open)
(keymap-global-set "C-S-d" #'dired-jump)
(keymap-global-set "C-M-S-d" #'dired-default-directory-as-root)

(keymap-set goto-map "b" #'switch-to-buffer)
(keymap-set goto-map "B" #'switch-to-buffer-other-window)
(keymap-set goto-map "C-b" #'switch-to-buffer-other-frame)
(keymap-set goto-map "e p" #'previous-error)
(keymap-set goto-map "e n" #'next-error)
(keymap-set goto-map "f" #'other-frame)
(keymap-set goto-map "l" #'goto-line)
(keymap-set goto-map "m" #'pop-to-mark-command)
(keymap-set goto-map "M" #'pop-global-mark)
(keymap-set goto-map "r" #'jump-to-register)
(keymap-set goto-map "w" #'other-window)
(keymap-set goto-map "#" #'bookmark-jump)
(keymap-set goto-map "C-#" #'bookmark-jump-other-window)
(keymap-set goto-map "M-#" #'bookmark-jump-other-frame)

(keymap-global-set "M-S" #'isearch-forward-thing-at-point)

(keymap-set search-map "b" #'isearch-backward)
(keymap-set search-map "B" #'isearch-backward-regexp)
(keymap-set search-map "f" #'isearch-forward)
(keymap-set search-map "F" #'isearch-forward-regexp)
(keymap-set search-map "s" #'isearch-forward-symbol)
(keymap-set search-map "g" #'find-grep)
(keymap-set search-map "." #'isearch-forward-thing-at-point) ; from: isearch-forward-symbol-at-point
(keymap-set search-map "M-." #'isearch-forward-symbol-at-point) ; from: isearch-forward-thing-at-point

(defvar-keymap a-replace-map
  :doc "Keymap for replacing"
  :prefix 'a-replace-map-prefix
  "i" #'isearch-query-replace
  "I" #'isearch-query-replace-regexp
  "q" #'query-replace
  "Q" #'query-replace-regexp
  "." #'query-replace-thing-at-point
  "M-." #'query-replace-regexp-thing-at-point)

(keymap-global-set "M-r" 'a-replace-map-prefix)
(keymap-global-set "M-R" 'query-replace-thing-at-point)

(keymap-set isearch-mode-map "M-r" #'isearch-query-replace)
(keymap-set isearch-mode-map "M-R" #'isearch-query-replace-regexp)

;;; Packages
;; General
(setopt use-package-always-ensure nil
        use-package-always-defer nil
        use-package-always-pin nil
        use-package-always-demand nil)

;; Base/Built-in
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
          lazy-count-suffix-format " [%s of %s]"))

(use-package imenu
  :init
  ;; Setup and settings
  (setopt imenu-max-item-length 100)
  (setopt imenu-max-items 30))

(use-package dired
  :init
  ;; Setup and settings
  (setopt dired-listing-switches (purecopy "-lahF")
          dired-maybe-use-globstar t
          dired-mouse-drag-files t
          dired-always-read-filesystem t
          dired-auto-revert-buffer #'dired-directory-changed-p
          dired-switches-in-mode-line 'as-is
          dired-kill-when-opening-new-dired-buffer t)

  :config
  ;; Keybindings
  (keymap-set dired-mode-map "RET" #'dired-find-file)
  (keymap-set dired-mode-map "M-RET" #'dired-find-file-other-window)
  (keymap-set dired-mode-map "TAB" #'dired-display-file)
  (keymap-set dired-mode-map "C-<up>" #'dired-prev-marked-file)
  (keymap-set dired-mode-map "C-<down>" #'dired-next-marked-file))

(use-package dabbrev
  :init
  ;; Setup and settings
  (setopt dabbrev-upcase-means-case-search t
          dabbrev-case-distinction nil
          dabbrev-case-replace nil))

(use-package which-key
  :ensure t

  :init
  ;; Setup and settings
  (setopt which-key-idle-delay 0.5
          which-key-max-description-length 0.20
          which-key-add-column-padding 2
          which-key-show-remaining-keys t
          which-key-preserve-window-configuration t
          which-key-sort-uppercase-first nil
          which-key-sort-order 'which-key-key-order-alpha)

  :config
  ;; Keybindings
  (keymap-set which-key-mode-map "C-x <f3>" #'which-key-C-h-dispatch)

  ;; Activation
  (which-key-mode 1))

(use-package completion-preview
  :init
  ;; Setup and settings (before load)
  (setopt completion-preview-minimum-symbol-length 2)

  :config
  ;; Keybindings
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)

  ;; Activation
  (global-completion-preview-mode 1))

;; Helpers
(use-package wgrep
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt wgrep-too-many-file-length 15))

(use-package ultra-scroll
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt scroll-margin 0)

  :config
  ;; Activation
  (ultra-scroll-mode 1))


(use-package move-it
  :ensure t
  :vc (:url "https://github.com/mmctl/move-it"
            :branch "main"
            :rev :newest)

  :bind
  ("M-<left>" . move-it-left)
  ("M-<down>" . move-it-down)
  ("M-<up>" . move-it-up)
  ("M-<right>" . move-it-right))

(use-package goggles
  :ensure t

  :hook (prog-mode text-mode)

  :init
  ;; Setup and settings (before load)
  (setopt goggles-pulse t))

(use-package easy-kill
  :ensure t

  :bind
  ("<remap> <kill-ring-save>" . #'easy-kill)
  ("<remap> <mark-word>" . #'easy-mark)

  :init
  ;; Setup and settings (before load)
  (setopt easy-kill-alist '((?w word           " ")
                            (?s symbol         " ")
                            (?S sexp           "\n")
                            (?h list           "\n")
                            (?f filename       "\n")
                            (?d defun          "\n\n")
                            (?D defun-name     " ")
                            (?l line           "\n")
                            (?b buffer-file-name "\n")))
  (setopt easy-kill-cycle-ignored '(list filename defun defun-name buffer-file-name)
          easy-kill-try-things '(url symbol word line)
          easy-mark-try-things '(url symbol word sexp)))

(use-package undo-tree
  :ensure t

  :init
  ;; Setup and settings
  ;; Create and store undo history directory
  (defconst UNDO_DIR (file-name-as-directory (file-name-concat EMACS_DATA_DIR "undos/"))
    "Directory where (automatically generated) undo (history) files are stored.")
  (unless (file-directory-p UNDO_DIR)
    (make-directory UNDO_DIR t))
  (setopt undo-tree-history-directory-alist `(("." . ,UNDO_DIR)))

  (setopt undo-tree-incompatible-major-modes '(term-mode image-mode doc-view-mode pdf-view-mode))
  (setopt undo-tree-visualizer-diff t)

  :config
  ;; Keybindings
  (keymap-set undo-tree-map "<remap> <undo-redo>" #'undo-tree-redo)

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
  (keymap-set minibuffer-mode-map "C-^" #'marginalia-cycle)
  (keymap-set minibuffer-local-map "C-^" #'marginalia-cycle))

(use-package jinx
  :ensure t

  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages)

  :hook (text-mode prog-mode conf-mode)

  :init
  ;; Setup and settings
  (setopt jinx-languages "en_US")
  (setopt jinx-include-faces '((prog-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (conf-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (yaml-mode . conf-mode)
                               (yaml-ts-mode . conf-mode))))


;; Completion
(use-package orderless
  :ensure t

  :config
  ;; Setup and settings (after load)
  (setopt orderless-matching-styles '(orderless-literal orderless-regexp))

  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion))))

  ;; Custom functionality
  (orderless-define-completion-style orderless-literal-regexp-flex
    (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal))))

(use-package vertico
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt vertico-count 15
          vertico-preselect 'first)

  :config
  ;; Keybindings
  (keymap-set vertico-map "<backtab>" #'minibuffer-complete)
  (keymap-set vertico-map "C-?" #'minibuffer-completion-help)

  ;; Activation
  (vertico-mode 1))

(use-package vertico-directory
  :ensure nil ; Provided by vertico

  :after vertico

  :config
  ;; Keybindings
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)

  ;; Hooks
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy))

(use-package vertico-mouse
  :ensure nil ; Provided by vertico

  :after vertico

  :hook vertico-mode)

(use-package vertico-quick
  :ensure nil ; Provided by vertico

  :after vertico

  :init
  ;; Setup and settings (before load)
  (setopt vertico-quick1 "asdfjkl;")
  (setopt vertico-quick2 "gwerhuio")

  :config
  ;; Keybindings
  (keymap-set vertico-map "C-S-j" #'vertico-quick-exit)
  (keymap-set vertico-map "C-M-j" #'vertico-quick-insert))

(use-package corfu
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt corfu-on-exact-match nil
          corfu-preview-current 'insert
          corfu-left-margin-width 0.5
          corfu-right-margin-width 0.5
          corfu-bar-width 0.25)
  (setopt text-mode-ispell-word-completion nil)

  :config
  ;; Activation
  (global-corfu-mode 1))

(use-package corfu-history
  :ensure nil ; Provided by Corfu

  :after corfu

  :config
  ;; Activation
  (corfu-history-mode 1))

(use-package corfu-quick
  :ensure nil ; Provided by Corfu

  :after corfu

  :init
  ;; Setup and settings (before load)
  (setopt corfu-quick1 "asdfjkl;")
  (setopt corfu-quick2 "gwerhuio")

  :config
  ;; Keybindings
  (keymap-set corfu-map "C-S-j" #'corfu-quick-insert)
  (keymap-set corfu-map "C-M-j" #'corfu-quick-complete))

(use-package corfu-popupinfo
  :ensure nil ; Provided by Corfu

  :after corfu

  :init
  ;; Setup and settings (before load)
  (setopt corfu-popupinfo-max-height (1+ corfu-count))

  :config
  ;; Activation
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt cape-file-prefix '("file:" "f:"))

  :config
  ;; Keybindings
  (keymap-global-set "C-c p" #'cape-prefix-map)

  ;; Hooks
  (defun setup-a-cape-text-mode ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
    (add-hook 'completion-at-point-functions #'cape-dict nil t))
  (defun setup-a-cape-code-mode ()
    (add-hook 'completion-at-point-functions #'cape-keyword nil t))
  (defun setup-a-cape-minibuffer ()
    (add-hook 'completion-at-point-functions #'cape-history nil t)
    (add-hook 'completion-at-point-functions #'cape-file nil t))

  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)

  (add-hook 'text-mode-hook #'setup-a-cape-text-mode)

  (add-hook 'tex-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'TeX-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'conf-mode-hook #'setup-a-cape-code-mode)
  (add-hook 'prog-mode-hook #'setup-a-cape-code-mode)

  (add-hook 'minibuffer-setup-hook #'setup-a-cape-minibuffer))

(use-package tempel
  :ensure t

  :pin melpa

  :bind
  ("M-c" . tempel-complete) ; original: capitalize-word
  ("M-C" . tempel-expand)
  (:prefix-map a-tempel-map :prefix "C-c t" :prefix-docstring "Keymap for tempel (global)"
               ("c" . tempel-complete)
               ("e" . tempel-expand)
               ("i" . tempel-insert))

  :init
  ;; Setup and settings (before load)
  ;; Create and store templates directory
  (defconst TEMPEL_DIR (file-name-as-directory (file-name-concat TEMPLATES_DIR "tempel/"))
    "Directory where tempel templates are stored.")
  (unless (file-directory-p TEMPEL_DIR)
    (make-directory TEMPEL_DIR t))
  (setopt tempel-path (file-name-concat TEMPEL_DIR "*.eld"))

  (setopt tempel-mark #(" " 0 1 (display (space :width (3)) face tempel-field)))

  :config
  ;; Keybindings
  (keymap-unset tempel-map "<remap> <beginning-of-buffer>")
  (keymap-unset tempel-map "<remap> <end-of-buffer>")
  (keymap-unset tempel-map "<remap> <backward-paragraph>")
  (keymap-unset tempel-map "<remap> <forward-paragraph>")
  (keymap-set tempel-map "M-<" #'tempel-beginning)
  (keymap-set tempel-map "M->" #'tempel-end)

  ;; Custom functionality
  (defun a-tempel-include (elt)
    "Define `include' element (taken and slightly adjusted from TempEL github repo)
that allows to include other templates by their name."
    (when (eq (car-safe elt) 'i)
      (when-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template))))
  (add-to-list 'tempel-user-elements #'a-tempel-include))

;;; Actions
(use-package ace-window
  :ensure t

  :bind
  ("<remap> <other-window>" . ace-window)

  :init
  ;; Setup and settings (before load)
  (setopt aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (setopt aw-scope 'visible
          aw-dispatch-always t)
  (setq-default aw-dispatch-alist
                '((?k aw-delete-window "Kill window")
                  (?K delete-other-windows "Kill other windows")
                  (?s aw-swap-window "Swap buffers between windows")
                  (?m aw-move-window "Move current buffer to window")
                  (?w aw-copy-window "Copy current buffer to window")
                  (?g aw-switch-buffer-in-window "Select buffer in window")
                  (?G aw-switch-buffer-other-window "Select buffer in other window")
                  (?r aw-flip-window)
                  (?x aw-execute-command-other-window "Execute command in other window")
                  (?f aw-split-window-fair "Split window fairly")
                  (?h aw-split-window-horz "Split window horizontally")
                  (?v aw-split-window-vert "Split window vertically")
                  (?t aw-transpose-frame "Transpose frames")
                  (?? aw-show-dispatch-help))))

(use-package avy
  :ensure t

  :pin melpa

  :bind
  (:prefix-map an-avy-map :prefix "C-c j" :prefix-docstring "Keymap for avy (global)"
               ("c" . avy-goto-char)
               ("C" . avy-goto-char-2)
               ("t" . avy-goto-char-timer)
               ("w" . avy-goto-word-1)
               ("W" . avy-goto-word-0)
               ("s" . avy-goto-subword-1)
               ("S" . avy-goto-subword-0)
               ("l" . avy-goto-line)
               ("M-l" . avy-kill-ring-save-whole-line)
               ("M-w" . avy-kill-ring-save-region)
               ("C-l" . avy-kill-whole-line)
               ("C-w" . avy-kill-region))
  (:map isearch-mode-map
        ("C-M-j" . avy-isearch))

  :bind*
  ("M-j" . avy-goto-char-timer)

  :init
  ;; Setup and settings (before load)
  (setopt avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
          avy-style 'at-full
          avy-all-windows 'all-frames
          avy-case-fold-search t
          avy-single-candidate-jump nil)
  (setopt avy-timeout-seconds 0.2)

  (setq-default avy-dispatch-alist '((?x . avy-action-kill-move)
                                     (?q . avy-action-kill-stay)
                                     (?m . avy-action-mark)
                                     (?w . avy-action-copy)
                                     (?y . avy-action-yank)
                                     (?Y . avy-action-yank-line)
                                     (?t . avy-action-teleport)
                                     (?z . avy-action-zap-to-char)
                                     (?i . avy-action-ispell))))

(use-package consult
  :ensure t

  :bind
  ("M-l" . consult-line) ; from: downcase-word
  ("M-m" . consult-mark) ; from: back-to-indentation
  ("M-M" . consult-global-mark)
  ("M-+" . consult-store-register)
  ("M-*" . consult-load-register)
  ("C-M-*" . consult-register)
  ("M-#" . consult-bookmark)
  ("<remap> <goto-line>" . consult-goto-line)
  ("<remap> <yank-pop>" . consult-yank-pop)
  ("<remap> <Info-search>" . consult-info)
  (:prefix-map a-consult-map :prefix "C-c h" :prefix-docstring "Keymap for consult (global)"
               ("x" . consult-mode-command)
               ("h" . consult-history)
               ("k" . consult-kmacro)
               ("l" . consult-man)
               ("m" . consult-minor-mode-menu)
               ("i" . consult-info)
               (":" . consult-complex-command))
  (:map ctl-x-r-map
        ("b" . consult-bookmark) ; from: bookmark-jump
        ("j" . consult-register) ; from: jump-to-register
        ("g" . consult-register-load) ; from: insert-register
        ("s" . consult-register-store)) ; from: copy-to-register
  (:map project-prefix-map
        ("b" . consult-project-buffer)) ; from: project-switch-to-buffer
  (:map isearch-mode-map
        ("<remap> <isearch-edit-string>" . consult-isearch-history)
        ("M-h" . consult-isearch-history)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi))
  (:map minibuffer-local-map
        ("M-h" . consult-history))
  (:map goto-map
        ("b" . consult-buffer) ; from: switch-to-buffer
        ("B" . consult-buffer-other-window) ; from: switch-to-buffer-other-window
        ("C-b" . consult-buffer-other-frame)
        ("e" . consult-compile-error) ; from: prefix (error)
        ("d" . consult-flymake)
        ("l" . consult-goto-line) ; from: goto-line
        ("o" . consult-outline)
        ("p" . consult-project-buffer) ; from: previous-error
        ("m" . consult-mark) ; from: pop-to-mark-command
        ("M" . consult-global-mark) ; from: pop-global-mark
        ("i" . consult-imenu) ; from: imenu
        ("I" . consult-imenu-multi)
        ("r" . consult-register-load) ; from: jump-to-register
        ("#" . consult-bookmark)) ; from: bookmark-jump
  (:map search-map
        ("g" . consult-grep) ; from: find-grep
        ("G" . consult-git-grep)
        ("h" . consult-isearch-history) ; from: prefix (highlight)
        ("l" . consult-line)
        ("L" . consult-line-multi)
        ("k" . consult-keep-lines)
        ("r" . consult-ripgrep)
        ("u" . consult-focus-lines))
  (:map a-buffer-map
        ("g" . consult-buffer) ; from: switch-to-buffer
        ("G" . consult-buffer-other-window) ; from: switch-to-buffer-other-window
        ("M-g" . consult-buffer-other-frame) ; from: switch-to-buffer-other-frame
        ("p" . consult-project-buffer)) ; from: project-switch-to-buffer
  (:map a-find-map
        ("c" . consult-fd)
        ("C" . consult-find)
        ("C-c" . consult-recent-file)
        ("M-c" . consult-locate))

  :init
  ;; Setup and settings (before load)
  (setopt consult-narrow-key "<"
          consult-widen-key ">")
  (setopt consult-async-refresh-delay 0.1
          consult-async-input-thottle 0.3
          consult-async-input-debounce 0.1
          consult-async-min-input 2)

  ;; Keybindings
  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-c H" #'consult-org-heading))

  (with-eval-after-load 'org-agenda
    (keymap-set org-agenda-mode-map "C-c H" #'consult-org-agenda))

  :config
  ;; Setup and settings (after load)
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  (setopt register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setopt register-preview-delay 0.5))

(use-package embark
  :ensure t
  :pin melpa

  :bind
  ("M-," . embark-act)
  ("M-." . embark-dwim)
  ("M-o" . embark-select)
  ("M-O" . embark-export)
  ("C-h C-b" . embark-bindings)
  (:prefix-map an-embark-map :prefix "C-c e" :prefix-docstring "Keymap for embark (global)"
               ("a" . embark-act)
               ("A" . embark-act-all)
               ("b" . embark-bindings)
               ("c" . embark-collect)
               ("d" . embark-dwim)
               ("s" . embark-select)
               ("e" . embark-export)
               ("l" . embark-live))
  (:map minibuffer-local-map
        ("M-b" . embark-become))

  :init
  ;; Setup and settings (before load)
  (setopt embark-confirm-act-all t)
  (setq-default prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Keybindings
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

(use-package avy-embark-collect
  :ensure t

  :bind
  (:map an-avy-map
        ("e" . avy-embark-collect-choose)
        ("E" . avy-embark-collect-act)))

(use-package embark-consult
  :ensure t

  :after (embark consult))


;;; Tools
(use-package dired-filter
  :ensure t

  :hook
  (dired-mode . dired-filter-mode)
  (dired-mode . dired-filter-group-mode)

  :bind
  (:map dired-mode-map
        ("M-/" . dired-filter-mode)
        ("M-@" . dired-filter-group-mode))
  (:map dired-filter-group-mode-map
        ("C-p" . 'dired-filter-group-backward-drawer)
        ("C-n" . 'dired-filter-group-forward-drawer))
  (:map dired-filter-group-header-map
        ("TAB" . 'dired-filter-group-toggle-header))

  :init
  ;; Setup and settings (before load)
  (setopt dired-filter-save-with-custom nil)

  (setopt dired-filter-prefix "/"
          dired-filter-mark-prefix "?")

  (setopt dired-filter-group-saved-groups '(("default"
                                             ("Directories" (directory . nil))
                                             ("Files" (file . nil))
                                             ("Symlinks" (symlink . nil)))))

  :config
  ;; Keybindings
  (keymap-unset dired-filter-group-mode-map "<tab>"))

(use-package dired-subtree
  :ensure t

  :preface
  (defvar-keymap a-dired-subtree-map
    :doc "Keymap for dired-subtree (to be bound in `dired-mode-map')"
    :prefix 'a-dired-subtree-map-prefix)

  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)
        ("M-TAB" . dired-subtree-cycle)
        ("M-t" . a-dired-subtree-map-prefix))
  (:map a-dired-subtree-map
        ("a" . dired-subtree-apply-filter)
        ("c" . dired-subtree-cycle)
        ("d" . dired-subtree-only-this-directory)
        ("e" . dired-subtree-end)
        ("f" . dired-subtree-only-this-file)
        ("i" . dired-subtree-insert)
        ("j" . dired-subtree-down)
        ("k" . dired-subtree-up)
        ("m" . dired-subtree-mark-subtree)
        ("n" . dired-subtree-next-sibling)
        ("N" . dired-subtree-narrow)
        ("p" . dired-subtree-previous-sibling)
        ("r" . dired-subtree-revert)
        ("R" . dired-subtree-remove)
        ("t" . dired-subtree-toggle)
        ("u" . dired-subtree-unmark-subtree)
        ("<down>" . dired-subtree-down)
        ("<up>" . dired-subtree-up))

  :config
  ;; Custom functionality
  ;; Prevent dired-insert-subtree from executing on empty directories,
  ;; fixes some unexpected behavior
  (defun dired-subtree-insert-check-empty-directory (dsi &rest args)
    (when-let* ((dfn (dired-get-filename nil t)))
      (if (directory-empty-p dfn)
          (user-error "Directory at point is empty, cannot insert subtree.")
        (apply dsi args))))

  (advice-add #'dired-subtree-insert :around #'dired-subtree-insert-check-empty-directory))

(use-package dired-narrow
  :ensure t

  :bind
  (:map dired-mode-map
        ("M-n" . dired-narrow-fuzzy)
        ("M-N" . dired-narrow))

  :init
  ;; Setup and settings (before load)
  (setopt dired-narrow-blink-time 0.3))

(use-package dired-collapse
  :ensure t

  :hook dired-mode

  :bind
  (:map dired-mode-map
        ("M-_" . dired-collapse-mode)))

(use-package diredfl
  :ensure t
  :pin melpa

  :hook dired-mode

  :init
  ;; Setup and settings (before load)
  (setopt diredfl-ignore-compressed-flag nil))

(use-package ediff
  :bind
  (:prefix-map an-ediff-map :prefix "C-c d" :prefix-docstring "Keymap for ediff entry points (global)"
               ("b" . ediff-buffers)
               ("B" . ediff-buffers3)
               ("d" . ediff-directories)
               ("D" . ediff-directories3)
               ("f" . ediff-files)
               ("F" . ediff-files3)
               ("m b" . ediff-merge-buffers)
               ("m B" . ediff-merge-buffers-with-ancestor)
               ("m d" . ediff-merge-directories)
               ("m D" . ediff-merge-directories-with-ancestor)
               ("m e" . ediff-merge-directories)
               ("m E" . ediff-merge-directories-with-ancestor)
               ("m f" . ediff-merge-files)
               ("m F" . ediff-merge-files-with-ancestor)
               ("m v" . ediff-merge-revisions)
               ("m V" . ediff-merge-revisions-with-ancestor)
               ("p" . ediff-patch-file)
               ("P" . ediff-patch-buffer)
               ("r" . ediff-regions-wordwise)
               ("R" . ediff-regions-linewise)
               ("s" . ediff-show-registry)
               ("v" . ediff-revision)
               ("V" . ediff-directory-revisions)
               ("w" . ediff-windows-wordwise)
               ("W" . ediff-windows-linewise)
               ("?" . ediff-documentation))

  :init
  ;; Setup and settings (before load)
  (setopt ediff-use-last-dir t)
  (setopt ediff-keep-variants nil)
  (setopt ediff-window-setup-function #'ediff-setup-windows-plain
          ediff-split-window-function #'split-window-horizontally)

  :config
  ;; Hooks
  ;; Save/restore window configuration when starting/quitting ediff
  (defvar an-ediff-preceding-window-configuration
    "Window configuration before starting ediff.")
  (defun an-ediff-store-window-configuration ()
    "Stores window configuration in `an-ediff-preceding-window-configuration'"
    (setq an-ediff-preceding-window-configuration (current-window-configuration)))
  (defun an-ediff-restore-window-configuration ()
    "Restores window configuration stored in
`an-ediff-preceding-window-configuration', if any, and resets it."
    (when an-ediff-preceding-window-configuration
      (set-window-configuration an-ediff-preceding-window-configuration)
      (setq an-ediff-preceding-window-configuration nil)))

  (add-hook #'ediff-before-setup-hook #'an-ediff-store-window-configuration)
  (add-hook #'ediff-quit-hook #'an-ediff-restore-window-configuration 90))

(use-package project
  :init
  ;; Setup and settings (before load)
  ;; Create and store file for known projects
  (defconst PROJECT_LIST_FILE (file-name-concat EMACS_DATA_DIR "projects.eld")
    "File where known project's are stored.")
  (setopt project-list-file PROJECT_LIST_FILE)
  (setopt project-mode-line t))

(use-package org
  :ensure t

  :preface
  ;; Setup (preface)
  ;; Create and store org root directory
  (defconst ORG_DIR (file-name-as-directory
                     (if (getenv "XDG_DATA_HOME")
                         (file-name-concat (getenv "XDG_DATA_HOME") "org/")
                       "~/org/"))
    "Directory used as default location for org files.")
  (unless (file-directory-p ORG_DIR)
    (make-directory ORG_DIR t))

  ;; Custom functionality
  (defun find-file-org ()
    "Find file in `ORG_DIR' using `find-file'."
    (interactive)
    (let ((default-directory ORG_DIR))
      (call-interactively #'find-file)))

  :bind
  ("C-c c" . org-capture)
  (:prefix-map an-org-map :prefix "C-c o" :prefix-docstring "Keymap for org (global)"
               ("a" . org-agenda)
               ("c" . org-capture)
               ("f" . find-file-org)
               ("l" . org-store-link))

  :init
  ;; Setup and settings (before load)
  ;; Modules
  (setopt org-modules '(ol-doi ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-eww
                               org-crypt org-habit org-id))

  ;; Create and store org calendar file
  (defconst ORG_CALENDAR_FILE (file-name-concat ORG_DIR "calendar.org")
    "Default file for calendar events created with org.")
  (unless (file-regular-p ORG_CALENDAR_FILE)
    (make-empty-file ORG_CALENDAR_FILE t))

  ;; Create and store org (default) notes file
  (defconst ORG_NOTES_FILE (file-name-concat ORG_DIR "notes.org")
    "Default file for notes (org).")
  (unless (file-regular-p ORG_NOTES_FILE)
    (make-empty-file ORG_NOTES_FILE t))

  ;; Create and store org (default) todos file
  (defconst ORG_TODOS_FILE (file-name-concat ORG_DIR "todos.org")
    "Default file for storing todos (org).")
  (unless (file-regular-p ORG_TODOS_FILE)
    (make-empty-file ORG_TODOS_FILE t))

  ;; Create and store org (default) meetings file
  (defconst ORG_MEETINGS_FILE (file-name-concat ORG_DIR "meetings.org")
    "Default file for meetings (org).")
  (unless (file-regular-p ORG_MEETINGS_FILE)
    (make-empty-file ORG_MEETINGS_FILE t))

  ;; PARA
  ;; Create and store org (default) projects file
  (defconst ORG_PROJECTS_FILE (file-name-concat ORG_DIR "projects.org")
    "Default file for projects (org).")
  (unless (file-regular-p ORG_PROJECTS_FILE)
    (make-empty-file ORG_PROJECTS_FILE t))

  ;; Create and store org (default) projects file
  (defconst ORG_AREAS_FILE (file-name-concat ORG_DIR "areas.org")
    "Default file for areas (org).")
  (unless (file-regular-p ORG_AREAS_FILE)
    (make-empty-file ORG_AREAS_FILE t))

  ;; Auxiliary
  ;; Create and store org (default) ID file
  (defconst ORG_ID_FILE (file-name-concat ORG_DIR ".org-id-locations")
    "Default file for storing identifiers (org).")
  (unless (file-regular-p ORG_ID_FILE)
    (make-empty-file ORG_ID_FILE t))

  (setopt org-default-notes-file ORG_NOTES_FILE)

  (setopt org-return-follows-link t)
  (setopt org-support-shift-select t)

  (setopt org-startup-folded 'content
          org-startup-indented t)

  (setopt org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t)

  (setopt org-log-done 'time
          org-log-refile nil)

  (setopt org-refile-allow-creating-parent-nodes 'confirm
          org-refile-targets '((nil . (:level . 1))
                               (nil . (:tag . "rftarget"))
                               (org-agenda-files . (:level . 1))
                               (org-agenda-files . (:tag . "rftarget")))
          org-refile-use-outline-path t
          org-outline-path-complete-in-steps nil)

  (setopt org-tag-alist
          '((:startgrouptag)
            ("Project" . ?P) (:grouptags) ("{proj@.+}" . ?p)
            (:endgrouptag)
            (:startgrouptag)
            ("Area" . ?A) (:grouptags) ("{area@.+}" . ?a)
            (:endgrouptag)
            ("area@admin" . ?d) ("event" . ?E) ("area@faf". ?f) ("area@home" . ?h)
            ("meeting" . ?M) ("noshow" . ?N) ("area@relation" . ?r) ("rftarget" . ?R)
            ("area@leisure" . ?l) ("area@travel" . ?t) ("area@work" . ?w)))

  (setopt org-tags-exclude-from-inheritance '("rftarget" "noshow"))

  (setopt org-todo-keywords '((sequence "TODO(t)" "DOING(p)" "BLOCKED(b)" "DONE(d)")))
  (setopt org-todo-keyword-faces
          '(("TODO" . (:inherit org-todo :weight bold))
            ("DOING" . (:inherit org-cite :weight medium))
            ("BLOCKED" . (:inherit org-warning :weight bold))
            ("DONE" . (:inherit org-done :weight normal))))

  (setopt org-priority-lowest ?C
          org-priority-hightest ?A)
  (setopt org-priority-faces '((?A . (:inherit org-priority :weight bold))
                               (?B . (:inherit org-warning :weight medium))
                               (?C . (:inherit org-cite :weight normal :slant oblique))))

  (setopt org-capture-templates
          '(("n" "Note"
             entry (file+headline ORG_NOTES_FILE "General Notes")
             "* %?\n:PROPERTIES:\n:Created: %U\n:END:"
             :empty-lines 0)
            ("t" "Todo"
             entry (file+headline ORG_TODOS_FILE "General Tasks")
             "* TODO [#B] %?\n:PROPERTIES:\n:Created: %U\n:END:"
             :empty-lines 0)
            ("e" "Calendar event"
             entry (file+headline ORG_CALENDAR_FILE "Events")
             "* %?\n:PROPERTIES:\n:Created: %U\n:END:\nTime: %^T\n** Notes:%i :noshow:"
             :empty-lines-before 0
             :empty-lines-after 1)
            ("m" "Meeting"
             entry (file+olp+datetree ORG_MEETINGS_FILE)
             "* %? :meeting:%^g\n:PROPERTIES:\n:Created: %U\n:END:\n** Notes:%i :noshow:\n** Action Items: :noshow:\n*** TODO [#B] "
             :tree-type week
             :clock-in t
             :clock-resume t
             :empty-lines-before 0
             :empty-lines-after 1)))

  (setopt org-read-date-popup-calendar t
          org-read-date-display-live t)

  (setopt org-deadline-warning-days 14)

  (setopt org-agenda-span 'day
          org-agenda-start-day "+0"
          org-agenda-start-on-weekday 1)
  (setopt org-agenda-skip-timestamp-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t)
  (setopt org-agenda-current-time-string "<< Now >>")
  (setopt org-agenda-hide-tags-regexp ".*")
  (setopt org-agenda-prefix-format '((agenda . "%-2i %?-12t %?-12s")
                                     (todo . "%-2i %?-12t %?-12s")
                                     (tags . "%-2i %?-12t %?-12s")
                                     (search . "%-2i %?-12t %?-12s")))
  (setopt org-agenda-sorting-strategy '((agenda . (habit-down time-up urgency-down category-keep))
                                        (todo . (urgency-down timestamp-up category-keep todo-state-up))
                                        (tags . (urgency-down category-keep))
                                        (search . (category-keep)))
          org-agenda-sort-notime-is-late t)
  (setopt org-agenda-sticky t)
  (setopt org-agenda-compact-blocks t)

  (with-eval-after-load 'nerd-icons
    (setq-default org-agenda-category-icon-alist
                  `(("Notes" ,(list (nerd-icons-faicon "nf-fa-note_sticky" :face 'nerd-icons-lyellow :v-adjust 0.05)) nil nil :ascent center)
                    ("Tasks" ,(list (nerd-icons-faicon "nf-fa-tasks" :face 'nerd-icons-lgreen :v-adjust 0.05)) nil nil :ascent center)
                    ("Events" ,(list (nerd-icons-faicon "nf-fa-calendar_day" :face 'nerd-icons-lblue :v-adjust 0.05)) nil nil :ascent center)
                    ("Appointments" ,(list (nerd-icons-faicon "nf-fa-user_clock" :face 'nerd-icons-lred :v-adjust 0.05)) nil nil :ascent center)
                    ("Meetings" ,(list (nerd-icons-faicon "nf-fa-users" :face 'nerd-icons-lorange :v-adjust 0.05)) nil nil :ascent center)
                    ("Projects" ,(list (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-lmaroon :v-adjust 0.05)) nil nil :ascent center)
                    ("Study" ,(list (nerd-icons-faicon "nf-fa-book_open" :face 'nerd-icons-lcyan :v-adjust 0.05)) nil nil :ascent center)
                    ("Research" ,(list (nerd-icons-faicon "nf-fa-flask" :face 'nerd-icons-lpurple :v-adjust 0.05)) nil nil :ascent center)
                    ("Development" ,(list (nerd-icons-faicon "nf-fa-code" :face 'nerd-icons-lpink :v-adjust 0.05)) nil nil :ascent center)
                    ("Areas" ,(list (nerd-icons-faicon "nf-fa-layer_group" :face 'nerd-icons-lyellow :v-adjust 0.05)) nil nil :ascent center)
                    ("Administration" ,(list (nerd-icons-faicon "nf-fa-briefcase" :face 'nerd-icons-lmaroon :v-adjust 0.05)) nil nil :ascent center)
                    ("FriendsAndFamily" ,(list (nerd-icons-faicon "nf-fa-user_group" :face 'nerd-icons-lpink :v-adjust 0.05)) nil nil :ascent center)
                    ("Home" ,(list (nerd-icons-faicon "nf-fa-home" :face 'nerd-icons-lgreen :v-adjust 0.05)) nil nil :ascent center)
                    ("Relationship" ,(list (nerd-icons-faicon "nf-fa-heart" :face 'nerd-icons-lred :v-adjust 0.05)) nil nil :ascent center)
                    ("Tinker" ,(list (nerd-icons-faicon "nf-fa-screwdriver_wrench" :face 'nerd-icons-lorange :v-adjust 0.05)) nil nil :ascent center)
                    ("Leisure" ,(list (nerd-icons-faicon "nf-fa-play" :face 'nerd-icons-lorange :v-adjust 0.05)) nil nil :ascent center)
                    ("Travel" ,(list (nerd-icons-faicon "nf-fa-plane_departure" :face 'nerd-icons-lcyan :v-adjust 0.05)) nil nil :ascent center)
                    ("Work" ,(list (nerd-icons-faicon "nf-fa-user_tie" :face 'nerd-icons-lpurple :v-adjust 0.05)) nil nil :ascent center))))

  (setopt org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
          org-id-locations-file ORG_ID_FILE
          org-id-locations-file-relative t)

  (setopt org-habit-graph-column 60
          org-habit-preceding-days 14)

  :config
  ;; Setup and settings (after load)
  (add-to-list 'org-agenda-files ORG_CALENDAR_FILE)
  (add-to-list 'org-agenda-files ORG_TODOS_FILE)
  (add-to-list 'org-agenda-files ORG_MEETINGS_FILE)
  (add-to-list 'org-agenda-files ORG_PROJECTS_FILE)
  (add-to-list 'org-agenda-files ORG_AREAS_FILE)

  ;; Keybindings
  (keymap-unset org-mode-map "C-M-S-<left>") ; original: org-decrease-number-at-point
  (keymap-unset org-mode-map "C-M-S-<right>")) ; original org-increase-number-at-point

(use-package org-super-agenda
  :ensure t

  :after org-agenda
  :hook org-agenda-mode

  :init
  (setopt org-super-agenda-header-prefix "")
  (setopt org-super-agenda-final-group-separator "\n")

  (setq-default org-super-agenda-groups
                '((:name "  Today"
                         :time-grid t
                         :date today
                         :scheduled today
                         :deadline today
                         :order 1)
                  (:name "  Overdue (Deadline/Schedule)"
                         :scheduled past
                         :deadline past
                         :order 2
                         :face org-warning)
                  (:name "  Upcoming (Deadline/Schedule)"
                         :scheduled future
                         :deadline future
                         :order 3
                         :face org-upcoming-deadline)
                  (:name "  Miscellaneous"
                         :date t
                         :order 4)))

  (add-to-list 'org-agenda-custom-commands
               '("g" "Project/Area view (projects, areas; todos, notes)"
                 ((tags "+{^proj@.*}-noshow-rftarget-TODO=\"DONE\""
                        ((org-agenda-files `(,ORG_PROJECTS_FILE))
                         (org-agenda-overriding-header " Projects")
                         (org-super-agenda-groups
                          '((:auto-outline-path t)))))
                  (tags "+{^area@.*}-noshow-rftarget-TODO=\"DONE\""
                        ((org-agenda-files `(,ORG_AREAS_FILE))
                         (org-agenda-overriding-header " Areas")
                         (org-super-agenda-groups
                          '((:auto-outline-path t))))))
                 ((org-agenda-compact-blocks nil))))
  (add-to-list 'org-agenda-custom-commands
               '("c" "Comprehensive todo view (projects, areas, misc)"
                 ((alltodo ""
                           ((org-agenda-files `(,ORG_PROJECTS_FILE))
                            (org-agenda-overriding-header " Projects")
                            (org-super-agenda-groups
                             '((:auto-outline-path t)))))
                  (alltodo ""
                           ((org-agenda-files `(,ORG_AREAS_FILE))
                            (org-agenda-overriding-header " Areas")
                            (org-super-agenda-groups
                             '((:auto-outline-path t)))))
                  (alltodo ""
                           ((org-agenda-files `(,ORG_TODOS_FILE ,ORG_MEETINGS_FILE))
                            (org-agenda-overriding-header "Miscellaneous")
                            (org-super-agenda-groups
                             '((:auto-outline-path t))))))
                 ((org-agenda-compact-blocks nil))))
  (add-to-list 'org-agenda-custom-commands
               '("p" "Priority view (TODOs)"
                 alltodo ""
                 ((org-agenda-files `(,ORG_TODOS_FILE ,ORG_PROJECTS_FILE ,ORG_AREAS_FILE ,ORG_MEETINGS_FILE))
                  (org-agenda-overriding-header "TODOs, Prioritized")
                  (org-super-agenda-groups
                   '((:name "  Overdue"
                            :scheduled past
                            :deadline past
                            :order 1
                            :face 'org-warning)
                     (:name "  Critical (#A)"
                            :priority "A"
                            :order 2)
                     (:name "  Non-Critical (< #A)"
                            :priority< "A"
                            :order 3))))))
  (add-to-list 'org-agenda-custom-commands
               '("o" "Organize view (TODOs)"
                 ((alltodo ""
                           ((org-agenda-overriding-header "TODOs, To Schedule")
                            (org-super-agenda-groups
                             '((:name "Unscheduled" :scheduled nil)
                               (:discard (:anything t))))))
                  (tags-todo "-{.*}"
                             ((org-agenda-overriding-header "TODOs, To Tag")
                              (org-super-agenda-groups
                               '((:name "Untagged" :anything t))))))
                 ((org-agenda-files `(,ORG_TODOS_FILE ,ORG_PROJECTS_FILE ,ORG_AREAS_FILE ,ORG_MEETINGS_FILE))))))

(use-package org-modern
  :ensure t

  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)

  :init
  (setopt org-modern-star 'replace)
  (setopt org-modern-hide-stars nil)
  (setopt org-modern-table nil)
  (setopt org-modern-block-name '("‣" . "‣"))
  (setopt org-modern-list '((?* . "•") (?+ . "‣")))
  (setopt org-modern-todo-faces '(("TODO" . (:inherit org-todo :weight bold :inverse-video t))
                                  ("DOING" . (:inherit org-cite :weight medium :inverse-video t))
                                  ("BLOCKED" . (:inherit org-warning :weight bold :inverse-video t))
                                  ("DONE" . (:inherit org-done :weight normal :inverse-video t))))
  (setopt org-modern-priority-faces '((?A . (:inherit org-priority :weight bold :inverse-video t))
                                      (?B . (:inherit org-warning :weight medium :inverse-video t))
                                      (?C . (:inherit org-cite :weight normal :slant oblique :inverse-video t))))
  :config
  (when (string-match-p "^Iosevka.*" (face-attribute 'default :family))
    (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
    (set-face-attribute 'org-modern-label nil :height 0.9 :width 'semi-condensed :weight 'medium)))

(use-package org-modern-indent
  :ensure t
  :vc (:url https://github.com/jdtsmith/org-modern-indent
            :branch main
            :rev :newest)

  :after org

  :config
  (defun setup-an-org-modern-indent-mode ()
    (org-indent-mode 1)
    (org-modern-indent-mode 1))

  (add-hook 'org-mode #'setup-an-org-modern-indent-mode 90))


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
  (keymap-set pdf-view-mode-map "<end>" #'pdf-view-last-page)
  (keymap-set pdf-view-mode-map "<home>" #'pdf-view-first-page)
  (keymap-set pdf-view-mode-map "z" #'pdf-view-shrink)
  (keymap-set pdf-view-mode-map "Z" #'pdf-view-enlarge)
  (keymap-set pdf-view-mode-map "r" #'revert-buffer)
  (keymap-set pdf-view-mode-map "v c" #'pdf-view-center-in-window)
  (keymap-set pdf-view-mode-map "v l" #'pdf-view-align-left)
  (keymap-set pdf-view-mode-map "v r" #'pdf-view-align-right)
  (keymap-set pdf-view-mode-map "v w" #'pdf-view-fit-width-to-window)
  (keymap-set pdf-view-mode-map "v h" #'pdf-view-fit-height-to-window)
  (keymap-set pdf-view-mode-map "v p" #'pdf-view-fit-page-to-window)
  (keymap-set pdf-view-mode-map "v d" #'pdf-view-dark-minor-mode)
  (keymap-set pdf-view-mode-map "v m" #'pdf-view-midnight-minor-mode)
  (keymap-set pdf-view-mode-map "v t" #'pdf-view-themed-minor-mode)
  (keymap-set pdf-view-mode-map "v p" #'pdf-view-printer-minor-mode)
  (keymap-set pdf-view-mode-map "m" #'pdf-view-position-to-register)
  (keymap-set pdf-view-mode-map "M" #'pdf-view-jump-to-register)

  ;; Hooks
  (add-hook 'pdf-tools-enabled-hook
            #'(lambda ()
                (keymap-unset pdf-sync-minor-mode-map "<double-mouse-1>"))))

;; Development
(use-package diff-hl
  :ensure t
  :pin melpa

  :init
  ;; Setup and settings (before load)
  (setopt diff-hl-global-modes '(not term-mode image-mode doc-view-mode pdf-view-mode))
  (setopt diff-hl-update-async t)

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

  :bind
  ("C-x m" . magit-status)
  ("C-c m" . magit-dispatch)
  ("C-c f" . magit-file-dispatch)

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
  (keymap-set magit-diff-section-map "M-RET" #'magit-diff-visit-worktree-file)

  ;; Activation
  (magit-wip-mode 1))

(use-package xref
  :defer t

  :config
  (keymap-set xref--xref-buffer-mode-map "C-p" #'xref-prev-group)
  (keymap-set xref--xref-buffer-mode-map "C-n" #'xref-next-group))

(use-package flymake
  :defer t

  :init
  (setopt flymake-show-diagnostics-at-end-of-line 'short)

  (keymap-set flymake-mode-map "M-P" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-N" #'flymake-goto-next-error))

(use-package eglot
  :bind
  (:prefix-map an-eglot-map :prefix "C-c l" :prefix-docstring "Keymap for eglot (global)"
               ("a" . eglot-code-actions)
               ("d" . eglot-find-declaration)
               ("e" . eglot-code-action-extract)
               ("f" . eglot-format)
               ("F" . eglot-format-buffer)
               ("i" . eglot-find-implementation)
               ("I" . eglot-code-action-inline)
               ("o" . eglot-code-action-organize-imports)
               ("q" . eglot-shutdown)
               ("Q" . eglot-shutdown-all)
               ("r" . eglot-rename)
               ("R" . eglot-code-action-rewrite)
               ("s" . eglot)
               ("t" . eglot-find-typeDefinition)
               ("x" . eglot-code-action-quickfix)
               ("C-c" . eglot-clear-status)
               ("C-e" . eglot-events-buffer)
               ("C-S-e" . eglot-stderr-buffer)
               ("C-f" . eglot-forget-pending-continuations)
               ("C-l" . eglot-list-connections)
               ("C-m" . eglot-manual)
               ("C-r" . eglot-reconnect)
               ("C-u" . eglot-upgrade-eglot)
               ("C-w" . eglot-show-workspace-configuration))

  :init
  (setopt eglot-autoshutdown t))

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
  (setopt TeX-master nil)
  (setopt TeX-parse-self t)
  (setopt TeX-auto-save t
          TeX-auto-untabify t)
  (setopt TeX-electric-math '("$" . "$"))

  :config
  ;; Hooks
  (add-hook 'TeX-language-en-hook (lambda () (jinx-languages "en_US")))
  (add-hook 'TeX-language-nl-hook (lambda () (jinx-languages "nl")))

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Swap to \( and \) instead of $ and $ (when using LaTeX instead of TeX)
  (defun setup-a-latex-mode-electric-math ()
    (setq-local TeX-electric-math '("\\(" . "\\)")))

  (add-hook 'LaTeX-mode-hook #'setup-a-latex-mode-electric-math)

  ;; Activation
  (TeX-source-correlate-mode 1))

(use-package cdlatex
  :ensure t

  :hook (latex-mode LaTeX-mode)

  :init
  ;; Setup and settings (before load)
  (setopt cdlatex-auto-help-delay 1)
  (setopt cdlatex-use-dollar-to-ensure-math t)
  (setopt cdlatex-insert-auto-labels-in-env-templates nil)
  (setopt cdlatex-sub-super-scripts-outside-math-mode nil)

  :config
  ;; Keybindings
  (keymap-unset cdlatex-mode-map "TAB")
  (keymap-set cdlatex-mode-map "<backtab>" #'cdlatex-tab)

  ;; Ensure Corfu is not in automatic mode, as to not interfere with templates
  (defun setup-a-cdlatex-corfu-mode ()
    (with-eval-after-load 'corfu
      (setq-local corfu-auto nil)))

  ;; Swap to \( and \) instead of $ and $ (when using LaTeX instead of TeX)
  (defun setup-a-latex-mode-not-use-dollar ()
    (setq-local cdlatex-use-dollar-to-ensure-math nil))

  ;; Hooks
  (add-hook 'cdlatex-mode-hook #'setup-a-cdlatex-corfu-mode)
  (add-hook 'latex-mode-hook #'setup-a-latex-mode-not-use-dollar)
  (add-hook 'LaTeX-mode-hook #'setup-a-latex-mode-not-use-dollar))

(use-package math-delimiters
  :ensure t
  :vc (:url https://github.com/oantolin/math-delimiters
            :branch main
            :rev :newest)

  :defer t

  :init
  ;; Setup and setting (before load)
  (setopt math-delimiters-inline '("$" . "$")) ; Supported by both TeX and LaTeX
  (setopt math-delimiters-compressed-display-math nil)

  ;; Set and unset appropriate keybinding upon loading relevant features
  (with-eval-after-load 'org
    (keymap-set org-mode-map "$" #'math-delimiters-insert))

  (with-eval-after-load 'tex ; AUCTeX
    (keymap-set TeX-mode-map "$" #'math-delimiters-insert))

  (with-eval-after-load 'tex-mode ; Built-in
    (keymap-set tex-mode-map "$" #'math-delimiters-insert))

  (with-eval-after-load 'cdlatex
    (keymap-unset cdlatex-mode-map "$" t))

  ;; Swap to \( and \) instead of $ and $ (when using LaTeX instead of TeX)
  (defun setup-a-latex-mode-math-delimiters ()
    (setq-local math-delimiters-inline '("\\(" . "\\)")))

  ;; Hooks
  (add-hook 'LaTeX-mode-hook #'setup-a-latex-mode-math-delimiters)
  (add-hook 'latex-mode-hook #'setup-a-latex-mode-math-delimiters))

(use-package markdown-mode
  :ensure t

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
  (keymap-set markdown-view-mode-map "<prior>" #'scroll-up-command)
  (keymap-set markdown-view-mode-map "<next>" #'scroll-down-command)
  (keymap-set markdown-view-mode-map "<home>" #'beginning-of-buffer)
  (keymap-set markdown-view-mode-map "<end>" #'end-of-buffer))

;; Shell/Bash
(use-package sh-script
  :defer t

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(bash-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(bash "https://github.com/tree-sitter/tree-sitter-bash"
                        "v0.23.3")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'bash)
      (treesit-install-language-grammar 'bash))))

;; C
(use-package c-ts-mode
  :defer t

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

  (setopt c-ts-mode-indent-offset 4)

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(c "https://github.com/tree-sitter/tree-sitter-c"
                     "v0.23.6")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'c)
      (treesit-install-language-grammar 'c))))

;; Python
(use-package python
  :defer t

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(python "https://github.com/tree-sitter/tree-sitter-python"
                          "v0.23.6")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'python)
      (treesit-install-language-grammar 'python))))

;; Rust
(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

  (setopt rust-ts-mode-indent-offset 4)

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(rust "https://github.com/tree-sitter/tree-sitter-rust"
                        "v0.23.3")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'rust)
      (treesit-install-language-grammar 'rust))))

;; Go
(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mod-mode . go-mod-ts-mode))

  (setopt go-ts-mode-indent-offset 4)

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(go "https://github.com/tree-sitter/tree-sitter-go"
                      "v0.23.4")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'go)
      (treesit-install-language-grammar 'go))
    (add-to-list 'treesit-language-source-alist
                 '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"
                         "v1.0.2")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'gomod)
      (treesit-install-language-grammar 'gomod))))

;; Yaml
(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))
    (unless (treesit-language-available-p 'yaml)
      (treesit-install-language-grammar 'yaml))))

;; Toml
(use-package toml-ts-mode
  :mode ("\\.toml\\'" . toml-ts-mode)

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(toml-mode . toml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
    (unless (treesit-language-available-p 'toml)
      (treesit-install-language-grammar 'toml))))

;; Json
(use-package json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode)

  :init
  ;; Setup and settings (before load)
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(json "https://github.com/tree-sitter/tree-sitter-json"))
    (unless (treesit-language-available-p 'json)
      (treesit-install-language-grammar 'json))))

;; OCaml
;; (use-package neocaml
;; (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "v0.24.0" "grammars/ocaml/src")
;; )


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

  :hook (tuareg-mode caml-mode))

;; Proof General (EasyCrypt)
;; Note, proof.el (which is provided by the proof-general package) is what is
;; actually loaded by the proof assistants, not proof-general.el.
;; Hence, we use `use-package proof :ensure proof-general` to
;; use deferred loading as usual
(use-package proof
  :ensure proof-general
  :pin melpa

  :defer t

  :init
  ;; Setup and settings (before load)
  ;; General
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
  ;; EasyCrypt
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
                     (bufhist-first . "<: Go to first history element")
                     (bufhist-last . ">: Go to last history element")
                     (bufhist-delete . "d: Delete current history element")))
    "p" #'bufhist-prev
    "n" #'bufhist-next
    "<" #'bufhist-first
    ">" #'bufhist-last
    "d" #'bufhist-delete)

  (defun setup-a-bufhist-map ()
    (keymap-set bufhist-mode-map "p" #'bufhist-prev)
    (keymap-set bufhist-mode-map "n" #'bufhist-next)
    (keymap-set bufhist-mode-map "<" #'bufhist-first)
    (keymap-set bufhist-mode-map ">" #'bufhist-last)
    (keymap-set bufhist-mode-map "c" #'bufhist-clear)
    (keymap-set bufhist-mode-map "d" #'bufhist-delete))
  (defun setup-a-proof-mode-map ()
    (keymap-unset proof-mode-map "M-<up>")
    (keymap-unset proof-mode-map "M-<down>")
    (keymap-unset proof-mode-map "C-M-<up>")
    (keymap-unset proof-mode-map "C-M-<down>")
    (keymap-unset proof-mode-map "C-c v")
    (keymap-set proof-mode-map "C-S-u" #'proof-undo-last-successful-command)
    (keymap-set proof-mode-map "C-S-p" #'proof-undo-last-successful-command)
    (keymap-set proof-mode-map "C-S-n" #'proof-assert-next-command-interactive)
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
    (keymap-set proof-mode-map "C-M-S-n" #'pg-next-matching-input)
    (keymap-set proof-mode-map "C-c M-v" #'pg-toggle-visibility))
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
  ;; Remove bufhist buttons
  (defun silence-bufhist-insert-buttons (&rest args)
    (setq-local bufhist-top-point (point-min)))

  (advice-add 'bufhist-insert-buttons :override #'silence-bufhist-insert-buttons))


;; EasyCrypt (extension)
(use-package easycrypt-ext
  :ensure t
  :vc (:url "https://github.com/mmctl/easycrypt-ext"
            :branch "main"
            :rev :newest)

  :after proof

  :hook
  (easycrypt-mode . easycrypt-ext-mode)
  (easycrypt-goals-mode . easycrypt-ext-goals-mode)
  (easycrypt-response-mode . easycrypt-ext-response-mode)

  :config
  ;; External integration
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(easycrypt-mode :types
                                  ((?t "Types" font-lock-type-face)
                                   (?o "Operators" font-lock-function-name-face)
                                   (?c "Constants" font-lock-constant-face)
                                   (?m "Modules" font-lock-property-use-face)
                                   (?M "Module Types" font-lock-type-face)
                                   (?a "Axioms" font-lock-builtin-face)
                                   (?l "Lemmas" font-lock-keyword-face)
                                   (?T "Theories" font-lock-type-face)))))

  ;; Keybindings
  (keymap-set easycrypt-ext-general-map "C-c C-p" #'ece-proofshell-print)
  (keymap-set easycrypt-ext-general-map "C-c l p" #'ece-proofshell-print)
  (keymap-set easycrypt-ext-general-map "C-c l P" #'ece-proofshell-prompt-print)
  (keymap-set easycrypt-ext-general-map "C-c l l" #'ece-proofshell-locate)
  (keymap-set easycrypt-ext-general-map "C-c l L" #'ece-proofshell-prompt-locate)
  (keymap-set easycrypt-ext-general-map "C-c l m" #'ece-proofshell-prompt-pragma)
  (keymap-set easycrypt-ext-general-map "C-c C-s" #'ece-proofshell-search)
  (keymap-set easycrypt-ext-general-map "C-c l s" #'ece-proofshell-search)
  (keymap-set easycrypt-ext-general-map "C-c l S" #'ece-proofshell-prompt-search)
  (keymap-set easycrypt-ext-general-map "C-c l t" 'ece-template-map-prefix)
  (keymap-set easycrypt-ext-general-map "C-c C-e" 'ece-exec-map-prefix)
  (keymap-set easycrypt-ext-general-map "C-c l e" 'ece-exec-map-prefix))

(use-package easycrypt-ext-cape
  :ensure nil ; Provided by `easycrypt-ext'

  :hook
  (easycrypt-ext-mode . easycrypt-ext-mode-cape-setup))

(use-package easycrypt-ext-tempel
  :ensure nil ; Provided by `easycrypt-ext'

  :hook
  (easycrypt-ext-mode . easycrypt-ext-mode-tempel-setup)

  :init
  (setopt ece-tempel-template-map-prefix "C-c l t"))

(use-package easycrypt-ext-avy
  :ensure nil ; Provided by `easycrypt-ext'

  :hook
  (easycrypt-ext-mode . easycrypt-ext-mode-avy-setup)
  (easycrypt-ext-goals-mode . easycrypt-ext-goals-mode-avy-setup)
  (easycrypt-ext-response-mode . easycrypt-ext-response-mode-avy-setup))

;; Themes
;; Doom-themes (general)
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
    '(visual-replace-delete-match :inherit 'anzu-replace-highlight)
    '(visual-replace-replacement :inherit 'anzu-replace-to)
    '(aw-background-face :inherit 'avy-background-face)
    '(aw-leading-char-face :inherit 'avy-lead-face)
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
    '(org-super-agenda-header :inherit 'org-agenda-date :height 1.1)
    '(proof-mouse-highlight-face :inherit 'lazy-highlight)
    '(proof-region-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
    '(proof-command-mouse-highlight-face :inherit 'proof-mouse-highlight-face)
    '(proof-active-area-face :inherit 'secondary-selection)
    '(proof-error-face :inherit 'error :weight 'semi-bold)
    '(proof-warning-face :inherit 'warning :weight 'semi-bold)
    '(proof-eager-annotation-face :inherit 'proof-warning-face :weight 'normal)
    '(easycrypt-tactics-tacticals-face :inherit 'proof-tacticals-name-face)
    '(easycrypt-tactics-closing-face :inherit 'warning)
    '(easycrypt-tactics-dangerous-face :inherit 'error))

  ;; Dummy face definitions
  ;; (applying theme settings that inherit from these faces
  ;; without explicitly loading the packages that
  ;; initially define them)
  (defface anzu-replace-highlight '((t . (:inherit default)))
    "Dummy definition for `anzu-replace-highlight'")
  (defface anzu-replace-to '((t . (:inherit default)))
    "Dummy definition for `anzu-replace-to'")
  (defface avy-background-face '((t . (:inherit default)))
    "Dummy definition for `avy-background-face'")
  (defface avy-lead-face '((t . (:inherit default)))
    "Dummy definition for `avy-lead-face'")
  (defface avy-lead-face-1 '((t . (:inherit default)))
    "Dummy definition for `avy-lead-face-1'"))

;; Doom-themes specific
;; Nord <3
(use-package doom-nord-theme
  :disabled t ; Don't use this theme
  ;; :demand t ; Use this theme

  :ensure nil ; Provided by doom-themes

  :init
  ;; Setup and settings (before load)
  (setopt doom-nord-brighter-modeline t
          doom-nord-brighter-comments nil
          doom-nord-comment-bg nil
          doom-nord-padded-modeline t
          doom-nord-region-highlight 'snowstorm)

  :config
  ;; Setup and settings (after load)
  (load-theme 'doom-nord t)
  (doom-themes-set-faces 'doom-nord
    '(cursor :background success)
    '(trailing-whitespace :background magenta)
    '(proof-queue-face :background magenta)
    '(proof-locked-face :background base3)
    '(proof-highlight-dependent-name-face :foreground magenta)
    '(proof-highlight-dependency-name-face :foreground orange)
    '(proof-declaration-name-face :foreground type)
    '(proof-tacticals-name-face :foreground numbers)
    '(proof-tactics-name-face :foreground functions)
    '(proof-script-sticky-error-face :background error :underline warning)
    '(proof-script-highlight-error-face :inherit 'proof-script-sticky-error-face
                                        :weight 'semi-bold :slant 'italic)
    '(proof-debug-message-face :foreground orange)
    '(proof-boring-face :foreground doc-comments))

  ;; Hooks
  (add-hook 'after-init-hook #'(lambda () (unless (daemonp) (enable-theme 'doom-nord)))))

(use-package doom-solarized-dark-theme
  :disabled t ; Don't use this theme
  ;; :demand t ; Use this theme

  :ensure nil ; Provided by doom-themes

  :init
  ;; Setup and settings (before load)
  (setopt doom-solarized-dark-brighter-modeline t
          doom-solarized-dark-brighter-comments nil
          doom-solarized-dark-brighter-text t
          doom-solarized-dark-padded-modeline t)

  :config
  ;; Setup and settings (after load)
  (load-theme 'doom-solarized-dark t)
  (doom-themes-set-faces 'doom-solarized-dark
    '(cursor :background highlight)
    '(trailing-whitespace :background magenta)
    '(proof-queue-face :background teal)
    '(proof-locked-face :background base4)
    '(proof-highlight-dependent-name-face :foreground violet)
    '(proof-highlight-dependency-name-face :foreground orange)
    '(proof-declaration-name-face :foreground type)
    '(proof-tacticals-name-face :foreground numbers)
    '(proof-tactics-name-face :foreground functions)
    '(proof-script-sticky-error-face :background error :underline warning)
    '(proof-script-highlight-error-face :inherit 'proof-script-sticky-error-face
                                        :weight 'semi-bold :slant 'italic)
    '(proof-debug-message-face :foreground orange)
    '(proof-boring-face :foreground doc-comments)
    '(font-lock-type-face :foreground type :slant 'normal))


  ;; Hooks
  (add-hook 'after-init-hook
            #'(lambda ()
                (unless (daemonp) (enable-theme 'doom-solarized-dark)))))

(use-package doom-solarized-light-theme
  ;; :disabled t ; Don't use this theme
  :demand t ; Use this theme

  :ensure nil ; Provided by doom-themes

  :init
  ;; Setup and settings (before load)
  (setopt doom-solarized-light-brighter-modeline t
          doom-solarized-light-brighter-comments nil
          doom-solarized-light-padded-modeline t)

  :config
  ;; Setup and settings (after load)
  (load-theme 'doom-solarized-light t)
  (doom-themes-set-faces 'doom-solarized-light
    '(cursor :background highlight)
    '(trailing-whitespace :background magenta)
    '(proof-queue-face :background teal)
    '(proof-locked-face :background base4)
    '(proof-highlight-dependent-name-face :foreground violet)
    '(proof-highlight-dependency-name-face :foreground orange)
    '(proof-declaration-name-face :foreground type)
    '(proof-tacticals-name-face :foreground numbers)
    '(proof-tactics-name-face :foreground functions)
    '(proof-script-sticky-error-face :background error :underline warning)
    '(proof-script-highlight-error-face :inherit 'proof-script-sticky-error-face
                                        :weight 'semi-bold :slant 'italic)
    '(proof-debug-message-face :foreground orange)
    '(proof-boring-face :foreground doc-comments)
    '(font-lock-type-face :foreground type :slant 'normal))

  ;; Hooks
  (add-hook 'after-init-hook
            #'(lambda ()
                (unless (daemonp) (enable-theme 'doom-solarized-light)))))

(use-package solaire-mode
  :ensure t

  :hook (after-init . solaire-global-mode))

(use-package nerd-icons
  :ensure t

  :init
  ;; Setup and settings (before load)
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t

  :hook dired-mode)

(use-package doom-modeline
  :ensure t

  :hook after-init

  :init
  ;; Setup and settings (before load)
  (setopt doom-modeline-buffer-encoding nil
          doom-modeline-default-coding-system 'utf-8
          doom-modeline-time-icon nil
          doom-modeline-time-live-icon nil
          doom-modeline-time-analogue-clock nil
          doom-modeline-percent-position nil
          doom-modeline-vcs-max-length 20))

(use-package keycast
  :ensure t

  :init
  (setopt keycast-mode-line-format "%10s%k%c%R%10s")

  :config
  ;; Setup and settings (after load)
  ;; Replacements
  (setopt keycast-substitute-alist
          (append keycast-substitute-alist '((self-insert-command t Typing...)
                                             ("<wheel-up>" t Scrolling...)
                                             ("<double-wheel-up>" t Scrolling...)
                                             ("<triple-wheel-up>" t Scrolling...)
                                             ("<wheel-up>" t Scrolling...)
                                             ("<double-wheel-up>" t Scrolling...)
                                             ("<triple-wheel-up>" t Scrolling...)
                                             ("<wheel-down>" t Scrolling...)
                                             ("<double-wheel-down>" t Scrolling...)
                                             ("<triple-wheel-down>" t Scrolling...))))

  ;; Custom global minor mode for compatibility with `doom-modeline'
  (define-minor-mode keycast-mode
	  "Show current command and its key binding in the mode line, for use with
`doom-modeline'."
	  :global t
	  (if keycast-mode
		    (add-hook 'pre-command-hook 'keycast--update nil t)
      (remove-hook 'pre-command-hook 'keycast--update t)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

;; Local/cross-package enhancements
(use-package local-pkgs
  :ensure nil ; Provided locally

  :bind
  (:map an-avy-map
        ("r" . an-avy-region-char-1)
        ("R" . an-avy-region-timer))
  :bind*
  ("M-J" . an-avy-region-timer)

  :init
  ;; Setup and settings (before load)
  (with-eval-after-load 'avy
    (add-to-list 'avy-dispatch-alist '(?p . avy-action-a-push-mark-no-activate) t)
    (add-to-list 'avy-dispatch-alist '(?P . avy-action-a-push-mark-activate) t)
    (add-to-list 'avy-dispatch-alist '(?X . avy-action-a-kill-line-move) t)
    (add-to-list 'avy-dispatch-alist '(?\C-x . avy-action-a-kill-whole-line-move) t)
    (add-to-list 'avy-dispatch-alist '(?Q . avy-action-a-kill-line-stay) t)
    (add-to-list 'avy-dispatch-alist '(?\C-q . avy-action-a-kill-whole-line-stay) t)
    (add-to-list 'avy-dispatch-alist '(?W . avy-action-a-copy-line) t)
    (add-to-list 'avy-dispatch-alist '(?\C-w . avy-action-a-copy-whole-line) t)
    (add-to-list 'avy-dispatch-alist '(?\C-y . avy-action-a-yank-whole-line) t)
    (add-to-list 'avy-dispatch-alist '(?T . avy-action-a-teleport-line) t)
    (add-to-list 'avy-dispatch-alist '(?\C-t . avy-action-a-teleport-whole-line) t)
    (add-to-list 'avy-dispatch-alist '(?o . avy-action-an-embark-select) t)
    (add-to-list 'avy-dispatch-alist '(?, . avy-action-an-embark-act) t)
    (add-to-list 'avy-dispatch-alist '(?. . avy-action-an-embark-dwim) t))

  (with-eval-after-load 'vertico
    (add-hook 'minibuffer-setup-hook (lambda ()
                                       (when (bound-and-true-p vertico--input)
                                         (keymap-set minibuffer-local-map "M-P" #'an-embark-select-vertico-previous)
                                         (keymap-set minibuffer-local-map "M-N" #'an-embark-select-vertico-next)))))
  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-c M-t" #'an-org-todo-manipulate-time))
  (with-eval-after-load 'org-agenda
    (keymap-set org-agenda-mode-map "C-c M-t" #'an-org-todo-manipulate-time)))

;; Corfu + Vertico
(use-package corfu
  :after vertico

  :init
  ;; Setup and settings (before load of this package, but after load of packages listed in `:after')
  (setopt global-corfu-minibuffer
          (lambda ()
            (not (or (bound-and-true-p vertico--input)
                     (eq (current-local-map) read-passwd-map))))))
