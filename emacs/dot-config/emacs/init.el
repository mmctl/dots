;; -*- lexical-binding: t; -*-
;; init.el
(setopt custom-file CUSTOM_FILE)

;;; Metadata

;; Name
(setopt user-full-name "Matthias Meijers")

;; Location (approximate)
(setopt calendar-latitude 51.441643)
(setopt calendar-longitude 5.469722)


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

;; Byte (and, if possible, natively) compile all local Elisp files (in LOCAL_DIR
;; and subdirectories)
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
(setopt auto-save-file-name-transforms `((".*" ,(expand-file-name "\\1" AUTOSAVES_DIR) t))
        auto-save-visited-file-name nil
        auto-save-interval 50
        auto-save-timeout 20
        auto-save-default t
        delete-auto-save-files t
        auto-save-list-file-prefix nil)

;; Locks
(setopt lock-file-name-transforms `((".*" ,(expand-file-name "\\1" LOCKS_DIR) t))
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
(setopt switch-to-buffer-in-dedicated-window 'pop)

(setopt fit-window-to-buffer-horizontally t)
(setopt window-sides-vertical nil)
(setopt window-sides-slots '(1 0 1 1))

(setopt help-window-select t)

(setopt display-buffer-base-action
        '((display-buffer-reuse-window
           display-buffer-in-previous-window
           display-buffer-pop-up-window
           display-buffer-use-some-window
           display-buffer-same-window)
          (reusable-frames . visible)
          (lru-frames . nil)))

(setq display-buffer-alist
      '(((or (derived-mode . Info-mode)
             (derived-mode . help-mode)
             (derived-mode . shortdoc-mode)
             (derived-mode . epa-info-mode)
             (derived-mode . apropos-mode)
             (derived-mode . man-common)
             (derived-mode . ibuffer-mode)
             (derived-mode . occur-mode)
             (derived-mode . grep-mode)
             (derived-mode . xref--xref-buffer-mode))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (reusable-frames . nil)
         (side . right)
         (slot . 0)
         (window-width . fit-lr-side-window-to-buffer)
         (preserve-size . (t . nil)))
        ((or (derived-mode . messages-buffer-mode)
             (derived-mode . compilation-mode)
             (derived-mode . emacs-lisp-compilation-mode)
             (derived-mode . backtrace-mode))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (reusable-frames . visible)
         (side . bottom)
         (slot . 0)
         (window-height . fit-bt-side-window-to-buffer)
         (preserve-size . (nil . t)))
        ((or (derived-mode . eshell-mode)
             (derived-mode . comint-mode)
             (derived-mode . term-mode)
             (category . comint)
             (category . tex-shell))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (reusable-frames . visible)
         (side . bottom)
         (slot . 0)
         (window-height . fit-bt-side-window-to-buffer)
         (preserve-size . (nil . t)))))

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
(add-hook 'special-mode-hook #'local-setup-special-mode)
(add-hook 'tex-mode-hook #'local-setup-code-mode)
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

(setq mouse-drag-and-drop-region-cross-program t)

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
;; (repeat-mode 1)


;; Miscellaneous
(setq-default bidi-display-reordering 'left-to-right)
(setopt bidi-paragraph-direction 'left-to-right)

(setopt sentence-end-double-space nil)
(setopt x-underline-at-descent-line nil)

(setopt url-configuration-directory (file-name-as-directory (expand-file-name "url/" EMACS_DATA_DIR)))
(setopt tramp-histfile-override (expand-file-name "tramp_shell_history" TRAMP_DIR))
(setopt tramp-persistency-file-name (expand-file-name "tramp_connection_history" TRAMP_DIR))

(setopt bookmark-file (expand-file-name "bookmarks" EMACS_DATA_DIR))
(setopt savehist-file (expand-file-name "history" EMACS_DATA_DIR))
(setopt recentf-save-file (expand-file-name "recentf" EMACS_DATA_DIR))

(savehist-mode 1)
(recentf-mode 1)

(setopt delete-by-moving-to-trash t)

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
(keymap-global-set "M-B" #'comment-and-duplicate-line-or-lines-in-region)

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
(keymap-set a-window-map "C-s" #'split-root-window-below)
(keymap-set a-window-map "M-s" #'split-root-window-right)
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
  "e" #'eldoc-print-current-symbol-info
  "E" #'eldoc-doc-buffer
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
  "x b" #'xref-go-back
  "x d" #'xref-find-definitions
  "x D" #'xref-find-definitions-other-window
  "x C-d" #'xref-find-definitions-other-frame
  "x f" #'xref-go-forward
  "x r" #'xref-find-references)

(keymap-global-set "M-f" 'a-find-map-prefix)

(keymap-global-set "C-x f" #'find-file)
(keymap-global-set "C-x F" #'find-file-other-window)
(keymap-global-set "C-x C-f" #'find-file-other-frame)
(keymap-global-set "C-x M-f" #'find-file-as-root)
(keymap-global-set "C-x C-r" #'recentf-open)
(keymap-global-set "C-x C-d" #'find-dired) ; from: list-directory
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

(setopt use-package-enable-imenu-support t)

;; Base/Built-in
(use-package epg-config
  :init
  (setopt epg-pinentry-mode 'loopback))

(use-package auth-source
  :init
  (setopt auth-sources (list AUTHINFO_FILE)))

(use-package esh-mode
  :init
  (setopt eshell-directory-name (file-name-as-directory
                                 (expand-file-name "eshell/" EMACS_DATA_DIR))))

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
  (setopt dired-listing-switches (purecopy "-l --almost-all --human-readable --group-directories-first --no-group")
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
          which-key-use-C-h-commands nil
          which-key-preserve-window-configuration t
          which-key-sort-uppercase-first nil
          which-key-sort-order 'which-key-key-order-alpha)

  :config
  ;; Keybindings
  (keymap-set which-key-mode-map "C-x <f3>" #'which-key-C-h-dispatch)
  (setq prefix-help-command #'a-which-key-repeated-prefix-help-command)

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

(use-package grep
  :init
  ;; Setup and settings (before load)
  (setopt grep-use-headings t))

;; Helpers
(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package scratch
  :ensure t

  :bind
  ("C-c s" . scratch))

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
  (defconst UNDO_DIR (file-name-as-directory (expand-file-name "undos/" EMACS_DATA_DIR))
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
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle))

(use-package jinx
  :ensure t

  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages)

  :hook (text-mode prog-mode conf-mode)

  :init
  ;; Setup and settings (before load)
  (setopt jinx-languages "en_US")
  (setopt jinx-include-faces '((prog-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (conf-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (yaml-mode . conf-mode)
                               (yaml-ts-mode . conf-mode))))

(use-package popper
  :ensure t
  :pin melpa

  :init
  ;; Setup and settings (before load)
  ;; Additional functionality
  (defun a-popper-group-by-directory-home-default ()
    "Returns an identifier to group popups, defaulting
to the project root (according to `project.el') if found,
with `default-directory' as fallback. In case
`default-directory' is the home directory, return `nil'
to assign to the default group."
    (or (and (fboundp 'project-root)
             (when-let* ((project (project-current)))
               (project-root project)))
        (unless (file-equal-p (expand-file-name "~/")
                              (expand-file-name default-directory))
          default-directory)))

  (setopt popper-reference-buffers
          '(messages-buffer-mode
            help-mode
            info-mode
            Man-mode
            woman-mode
            compilation-mode
            backtrace-mode
            debugger-mode
            emacs-lisp-compilation-mode
            flymake-diagnostics-buffer-mode
            ibuffer-mode
            occur-mode
            grep-mode
            xref--xref-buffer-mode
            "\\`\\*Async.*\\*\\'"))
  (setopt popper-display-control nil)
  ;; (setopt popper-group-function #'popper-group-by-directory)
  (setopt popper-group-function #'a-popper-group-by-directory-home-default)
  (setopt popper-mode-line nil)

  :config
  ;; Setup and settings (after load)
  ;; Additional functionality
  (defun a-popper-toggle-next (&optional arg)
    "Toggle next popup in group without burying current one through
 providing `popper-toggle', which see, a single prefix argument (by
default). With prefix argument ARG, calls `popper-toggle' with an
additional prefix argument."
    (interactive "p")
    (popper-toggle (* 4 arg)))

  (defun a-popper-cycle-default-group ()
    "Cycle to next popup in default group by calling `popper-cycle',
which see, with `0' as argument."
    (interactive)
    (popper-cycle 0))

  ;; Keybindings
  (keymap-global-set "M-o" #'popper-toggle)
  (keymap-global-set "M-O" #'a-popper-toggle-next)
  (keymap-global-set "C-M-o" #'popper-cycle) ; from: split-line
  (keymap-global-set "C-S-o" #'a-popper-cycle-default-group)
  (keymap-global-set "C-M-S-o" #'popper-toggle-type)

  (defvar-keymap a-popper-map
    :doc "Keymap for popper (global)"
    :prefix 'a-popper-map-prefix
    "k" #'popper-kill-latest-popup
    "l" #'popper-lower-to-popup
    "t" #'popper-toggle
    "T" #'popper-toggle-type
    "r" #'popper-raise-popup
    "^" #'popper-raise-popup
    "_" #'popper-lower-to-popup
    "<left>" #'popper-cycle-backwards
    "<right>" #'popper-cycle)
  (keymap-global-set "C-c p" 'a-popper-map-prefix)

  (defvar-keymap a-popper-cycle-repeat-map
    :doc "Keymap (repeatable) for popper cycling"
    :repeat t
    "<left>" #'popper-cycle-backwards
    "<right>" #'popper-cycle)

  ;; Activation
  (popper-mode 1))

(use-package popper-echo
  :ensure nil ; Provided by popper

  :after popper

  :init
  ;; Setup and settings (before load)
  (setopt popper-echo-lines 2)
  (setopt popper-echo-dispatch-actions t)
  (setopt popper-echo-dispatch-keys '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

  :config
  ;; Setup and settings (after load)
  ;; Faces
  (set-face-attribute 'popper-echo-area-buried nil :inherit 'custom-comment)
  (set-face-attribute 'popper-echo-dispatch-hint nil :inherit 'custom-comment :weight 'bold)
  (popper-echo-mode 1))

;; Messages/Email
(use-package message
  :init
  ;; Sysvar
  (setopt message-directory (or (getenv "MAILDIR")
                                (file-name-as-directory
                                 (expand-file-name "mail/" (or (getenv "XDG_DATA_HOME") "~/")))))

  (setopt message-kill-buffer-on-exit t)
  (setopt message-confirm-send t)
  (setopt send-mail-function #'smtpmail-send-it
          message-send-mail-function #'smtpmail-send-it)

  (setopt message-required-mail-headers
          '(From Subject Date (optional . In-Reply-To) Message-ID))

  (setopt gnus-inhibit-images t)
  (setopt gnus-buttonized-mime-types '("multipart/signed"))

  (with-eval-after-load 'mm-decode
    ;; Discourage rendering of rich-text formats
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (add-to-list 'mm-discouraged-alternatives "image/.*")
    (setopt mm-enable-external 'ask)
    (setopt gnus-mime-display-multipart-related-as-mixed t)))


;; Requires external installation and setup
;; Dependencies: mbsync, mu, mu4e
;; see: https://www.djcbsoftware.nl/code/mu/mu4e/
(use-package mu4e
  :init

  ;; Sysvar: with recent versions of mbsync, the config file explicitly given
  ;; here is the first default checked (so not needed to provide explicitly)
  (setopt mu4e-get-mail-command (concat "mbsync -a"
                                        (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
                                          (concat " -c " (shell-quote-argument
                                                          (expand-file-name "isyncrc" xdgcnf))))))
  (setopt mu4e-update-interval 300)
  (setopt mu4e-change-filenames-when-moving t)

  (setopt mu4e-main-hide-personal-addresses t) ; Hide personal addresses because we use many
  (setopt mu4e-use-fancy-chars t)

  (setopt mu4e-compose-format-flowed t)

  (with-eval-after-load 'vertico
    (setopt mu4e-completing-read-function #'completing-read)
    (setopt mu4e-read-option-use-builtin nil))

  ;; Consider following if indexing is slow
  ;; (setopt mu4e-index-cleanup nil)
  ;; (setopt mu4e-index-lazy-check nil)

  (setq-default mu4e-headers-attach-mark '("a" . "∀"))

  :config
  (require 'local-mu4e)

  (setopt mail-user-agent (mu4e-user-agent)
          message-mail-user-agent t)

  (setopt mu4e-sent-folder #'a-determine-mu4e-sent-folder
          mu4e-drafts-folder #'a-determine-mu4e-drafts-folder
          mu4e-trash-folder #'a-determine-mu4e-trash-folder
          mu4e-refile-folder #'a-determine-mu4e-refile-folder)

  (setopt mu4e-contexts
          `(,(make-mu4e-context
              :name "Personal"
              :enter-func (lambda () (mu4e-message "Entering context: Personal"))
              :leave-func (lambda () (mu4e-message "Leaving context: Personal"))
              :match-func (lambda (msg)
                            (when msg
                              (member (a-mailroot-from-mu4e-message msg)
                                      PERSONAL_MAILROOTS)))
              :vars
              `((mu4e-maildir-shortcuts . ((:maildir "/personal-mmeijers/INBOX" :key ?p)
                                           (:maildir "/kernel-mmeijers/INBOX" :key ?k)
                                           (:maildir "/kem-mmeijers/INBOX" :key ?r)))
                (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query PERSONAL_MAILROOTS))
                                   (:name "All unread" :key ?u :query ,(concat "("
                                                                               (a-mu4e-inbox-roots-query PERSONAL_MAILROOTS)
                                                                               ") AND flag:unread"))
                                   (:name "Personal unread" :key ?p :query "maildir:/personal-mmeijers/INBOX AND flag:unread")
                                   (:name "Kernel unread" :key ?k :query "maildir:/kernel-mmeijers/INBOX AND flag:unread")
                                   (:name "KeM unread" :key ?r :query "maildir:/kem-mmeijers/INBOX AND flag:unread")))
                (mu4e-get-mail-command . ,(concat "mbsync"
                                                  (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
                                                    (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
                                                  " personal"))))
            ,(make-mu4e-context
              :name "Work"
              :enter-func (lambda () (mu4e-message "Entering context: Work"))
              :leave-func (lambda () (mu4e-message "Leaving context: Work"))
              :match-func (lambda (msg)
                            (when msg
                              (member (a-mailroot-from-mu4e-message msg)
                                      PERSONAL_MAILROOTS)))
              :vars
              `((mu4e-maildir-shortcuts . ((:maildir "/research-mmeijers/INBOX" :key ?r)
                                           (:maildir "/teaching-mmeijers/INBOX" :key ?t)))
                (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query WORK_MAILROOTS))
                                   (:name "All unread" :key ?u :query ,(concat "("
                                                                               (a-mu4e-inbox-roots-query WORK_MAILROOTS)
                                                                               ") AND flag:unread"))
                                   (:name "Research unread" :key ?r :query "maildir:/research-mmeijers/INBOX AND flag:unread")
                                   (:name "Teaching unread" :key ?t :query "maildir:/reaching-mmeijers/INBOX AND flag:unread")))
                (mu4e-get-mail-command . ,(concat "mbsync"
                                                  (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
                                                    (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
                                                  " work"))))))

  (setopt mu4e-context-policy 'ask-if-none)
  (setopt mu4e-compose-context-policy nil)
  (setopt message-send-mail-function #'an-smtpmail-configure-and-send-it)

  (advice-add #'mu4e--draft :around #'an-around-advice-draft-configure))


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
  (keymap-set vertico-map "C-M-<tab>" #'minibuffer-complete)
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
  (keymap-global-set "C-c w" #'cape-prefix-map)

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
  ("M-c" . tempel-complete) ; from: capitalize-word
  ("M-C" . tempel-expand)
  (:prefix-map a-tempel-map :prefix "C-c t" :prefix-docstring "Keymap for tempel (global)"
               ("c" . tempel-complete)
               ("e" . tempel-expand)
               ("i" . tempel-insert))

  :init
  ;; Setup and settings (before load)
  ;; Create and store templates directory
  (defconst TEMPEL_DIR (file-name-as-directory (expand-file-name "tempel/" TEMPLATES_DIR))
    "Directory where tempel templates are stored.")
  (unless (file-directory-p TEMPEL_DIR)
    (make-directory TEMPEL_DIR t))
  (setopt tempel-path (expand-file-name "*.eld" TEMPEL_DIR))

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
  (setopt aw-keys '(?f ?j ?s ?l ?a ?\;))
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
                  (?d aw-split-window-fair "Split window fairly")
                  (?h aw-split-window-horz "Split window horizontally")
                  (?v aw-split-window-vert "Split window vertically")
                  (?t aw-transpose-frame "Transpose frames")
                  (?? aw-show-dispatch-help)))

  ;; Additional functionality
  (defun an-ace-window-prefix ()
    "Sets `ace-window' as the function to choose window for displaying the
buffer of the next command.

The next buffer is the buffer displayed by the next command invoked
immediately after this command, ignoring reading from the minibuffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let ((window (aw-select (propertize " ACE" 'face 'mode-line-highlight)))
             (type 'reuse))
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  ;; Keybindings
  (keymap-set a-window-map "o" #'an-ace-window-prefix))


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
  ("M-j" . avy-goto-char-2)
  ("M-J" . avy-goto-char-timer)

  :init
  ;; Setup and settings (before load)
  (setopt avy-keys '(?f ?j ?s ?l ?a ?\;)
          avy-style 'at-full
          avy-all-windows 'all-frames
          avy-case-fold-search t
          avy-single-candidate-jump nil)
  (setopt avy-timeout-seconds 0.2)

  (setq-default avy-dispatch-alist '((?x . avy-action-kill-move)
                                     (?m . avy-action-mark)
                                     (?w . avy-action-copy)
                                     (?k . avy-action-kill-stay)
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
        ("M-h" . consult-history)
        ("S-<up>" . previous-line-or-history-element)
        ("S-<down>" . next-line-or-history-element))
  (:map goto-map
        ("b" . consult-buffer) ; from: switch-to-buffer
        ("B" . consult-buffer-other-window) ; from: switch-to-buffer-other-window
        ("C-b" . consult-buffer-other-frame)
        ("e" . consult-compile-error) ; from: prefix (error)
        ("`" . consult-flymake)
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
  (setopt consult-preview-key '("S-<up>" "S-<down>" "M-V")
          consult-narrow-key "<"
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
  (setopt register-preview-delay 0.5)

  (setopt consult-ripgrep-args (concat consult-ripgrep-args " --no-config"))

  ;; Patches
  (defun filter-return-advice-preview-buffer-no-obey-display-actions (ret)
    "Filter return advice for consult's preview buffer function
(`consult--buffer-preview') to execute it in environment where
`switch-to-buffer' does not obey display actions and typically
uses window unless, e.g., dedicated."
    (lambda (action cand)
      (let* ((switch-to-buffer-obey-display-actions nil))
        (funcall ret action cand))))

  ;; Preview buffers without obeying display actions
  (advice-add #'consult--buffer-preview :filter-return #'filter-return-advice-preview-buffer-no-obey-display-actions))

(use-package consult-dir
  :ensure t

  :bind
  ("<remap> <find-dired>" . consult-dir)
  (:map minibuffer-local-map
        ("M-d" . consult-dir)
        ("M-D" . consult-dir-jump-file))
  (:map a-find-map
        ("C-d" . consult-dir))

  :init
  (setopt consult-dir-jump-file-command #'consult-fd))

(use-package embark
  :ensure t
  :pin melpa

  :bind
  ("M-," . embark-act)
  ("M-." . embark-dwim)
  ("M-u" . embark-select) ; from: upcase-word
  ("M-U" . embark-export)
  ("C-h C-b" . embark-bindings)
  (:prefix-map an-embark-map :prefix "C-c a" :prefix-docstring "Keymap for embark (global)"
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
  (setopt embark-verbose-indicator-display-action
          '((display-buffer-reuse-window display-buffer-in-side-window)
            (reusable-frames . nil)
            (side . right)
            (slot . 0)
            (window-width . fit-lr-side-window-to-buffer)))

  ;; (setq-default prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Keybindings
  (keymap-set embark-general-map "C-u" #'embark-select)

  (keymap-set embark-file-map "F" #'find-file-other-window)
  (keymap-set embark-file-map "C-f" #'find-file-other-frame)
  (keymap-set embark-file-map "M-f" #'find-file-as-root)
  (keymap-set embark-file-map "l" #'find-file-literally)
  (keymap-set embark-file-map "C" #'copy-directory)

  (keymap-set embark-library-map "F" #'find-library-other-window)
  (keymap-set embark-library-map "C-f" #'find-library-other-frame)

  (keymap-set embark-buffer-map "g" #'switch-to-buffer)
  (keymap-set embark-buffer-map "G" #'switch-to-buffer-other-window)
  (keymap-set embark-buffer-map "M-g" #'switch-to-buffer-other-frame)

  (keymap-set embark-identifier-map "f" #'xref-find-definitions)
  (keymap-set embark-identifier-map "F" #'xref-find-definitions-other-window)
  (keymap-set embark-identifier-map "C-f" #'xref-find-definitions-other-frame)

  (keymap-set embark-symbol-map "f" #'embark-find-definition)

  (keymap-set embark-package-map "f" #'describe-package)

  (keymap-set embark-become-file+buffer-map "F" #'find-file-other-window)
  (keymap-set embark-become-file+buffer-map "B" #'switch-to-buffer-other-window)

  ;; Display
  (add-to-list 'display-buffer-alist
               '((major-mode . embark-collect-mode)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side . right)
                 (slot . 0)
                 (window-width . fit-lr-side-window-to-buffer)
                 (preserve-size . (t . nil))))

  (with-eval-after-load 'popper
    (add-to-list 'popper-reference-buffers 'embark-collect-mode)
    (when popper-mode
      (popper-mode 1))))

(use-package avy-embark-collect
  :ensure t
  :pin melpa

  :bind
  (:map an-avy-map
        ("e" . avy-embark-collect-choose)
        ("E" . avy-embark-collect-act)))

(use-package embark-consult
  :ensure t
  :pin melpa

  :after (embark consult))

;;; Tools
;; Enhanced dired-like manager
;; Depends on (mandatory): (GNU) ls, fd,
;; Depends on (optional): poppler-utils ffmpegthumbnailer mediainfo libvips-tools 7zip imagemagick
(use-package dirvish
  :ensure t
  :demand t

  :preface
  (defun a-dirvish-fd-default-directory (pattern)
    "Simple wrapper around `dirvish-fd', with target directory fixed to
`default-directory'"
    (interactive (list (completing-read-multiple "Pattern: " nil)))
    (dirvish-fd default-directory pattern))

  (defun a-dirvish-fd-full ()
    "Simple wrapper around `dirvish-fd', with `current-prefix-arg'
set to '(16) (so it asks to provide both arguments)."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'dirvish-fd)))

  (defun a-dirvish-side-quit ()
    "Quits/kills `dirvish-side' session/window if it is visible (else does
nothing)."
    (interactive)
    (when-let* ((viswin (dirvish-side--session-visible-p)))
      (with-selected-window viswin
        (dirvish-quit))))

  :init
  ;; Add, load, and compile extensions (seems due to bug (?))
  (when-let* ((libdir (locate-library "dirvish"))
              (extdir (expand-file-name "extensions/" (file-name-parent-directory libdir)))
              ((file-directory-p extdir))
              (alfile (expand-file-name "dirvish-extensions-autoloads.el" extdir)))
    (add-to-list 'load-path extdir)
    (unless (file-exists-p alfile)
      (loaddefs-generate extdir alfile))
    (load alfile))

  (setopt dirvish-cache-dir (file-name-as-directory (expand-file-name "dirvish/" EMACS_CACHE_DIR)))
  (setopt dirvish-fd-switches "--full-path --color=never")
  (setopt dirvish-attributes '(vc-state subtree-state nerd-icons collapse file-size file-modes file-time))

  (setopt dirvish-header-line-format '(:left (path) :right (free-space))
          dirvish-mode-line-format '(:left (sort symlink) :right (omit yank vc-info index)))
  (setopt dirvish-use-header-line 'global
          dirvish-use-mode-line t)
  (setopt dirvish-default-layout '(1 0.10 0.40))

  (setopt dirvish-quick-access-entries
          `(("h" ,(file-name-as-directory (expand-file-name "~/")) "Home")
            ("p" ,(file-name-as-directory (expand-file-name "projects/" "~/")) "Projects")
            ("a" ,(file-name-as-directory (expand-file-name "areas/" "~/")) "Areas")
            ("r" ,(file-name-as-directory (expand-file-name "resources/" "~/")) "Resources")
            ("A" ,(file-name-as-directory (expand-file-name "archive/" "~/")) "Archive")
            ("c" ,(file-name-as-directory (expand-file-name (or (getenv "XDG_CONFIG_HOME")
                                                                "~/.config/")))
             "User config")
            ("C" "/etc/" "System config")
            ("d" ,(file-name-as-directory (expand-file-name (or (getenv "XDG_DATA_HOME")
                                                                "~/.local/share/")))
             "User data")
            ("D" "/usr/share/" "System data")))

  (setopt dirvish-side-mode-line-format '(:left (sort vc-info)))
  (setopt dirvish-side-attributes '(vc-state subtree-state nerd-icons))

  (setopt dirvish-collapse-separator "/")

  :bind
  (:prefix-map a-dirvish-map :prefix "C-c d" :prefix-docstring "Keymap for dirvish (global)"
               ("d" . dirvish-dwim)
        ("D" . dirvish)
        ("j" . dirvish-quick-access)
        ("s" . dirvish-side)
        ("S" . a-dirvish-side-quit)
        ("f" . a-dirvish-fd-default-directory)
        ("F" . a-dirvish-fd-full)
        ("C-f" . dirvish-fd))

  :config
  (keymap-set dirvish-mode-map "?" #'dirvish-dispatch)
  (keymap-set dirvish-mode-map "a" #'dirvish-chxxx-menu)
  (keymap-set dirvish-mode-map "e" #'dirvish-renaming-menu)
  (keymap-set dirvish-mode-map "h" #'dirvish-history-menu)
  (keymap-set dirvish-mode-map "f" #'dirvish-file-info-menu)
  (keymap-set dirvish-mode-map "j" #'dirvish-quick-access)
  (keymap-set dirvish-mode-map "s" #'dirvish-quicksort)
  (keymap-set dirvish-mode-map "r" #'dirvish-history-jump)
  (keymap-set dirvish-mode-map "l" #'dirvish-ls-switches-menu)
  (keymap-set dirvish-mode-map "v" #'dirvish-vc-menu)
  (keymap-set dirvish-mode-map "*" #'dirvish-mark-menu)
  (keymap-set dirvish-mode-map ":" #'dirvish-epa-dired-menu)
  (keymap-set dirvish-mode-map "y" #'dirvish-yank-menu)
  (keymap-set dirvish-mode-map "Y" #'dirvish-yank)
  (keymap-set dirvish-mode-map "N" #'dirvish-narrow)
  (keymap-set dirvish-mode-map "TAB" #'dirvish-subtree-toggle)
  (keymap-set dirvish-mode-map "/" #'a-dirvish-fd-default-directory)
  (keymap-set dirvish-mode-map "M-/" #'a-dirvish-fd-full)
  (keymap-set dirvish-mode-map "{" #'dirvish-history-go-backward)
  (keymap-set dirvish-mode-map "}" #'dirvish-history-go-forward)
  (keymap-set dirvish-mode-map "M-}" #'dirvish-history-last)
  (keymap-set dirvish-mode-map "M-a" #'dirvish-setup-menu)
  (keymap-set dirvish-mode-map "M-e" #'dirvish-emerge-menu)
  (keymap-set dirvish-mode-map "M-t" #'dirvish-layout-toggle)
  (keymap-set dirvish-mode-map "<left>" #'dired-up-directory)
  (keymap-set dirvish-mode-map "<right>" #'dired-find-file)
  (keymap-set dirvish-mode-map "<mouse-1>" #'dirvish-subtree-toggle-or-open)
  (keymap-set dirvish-mode-map "<mouse-2>" #'dired-mouse-find-file-other-window)
  (keymap-set dirvish-mode-map "<mouse-3>" #'dired-mouse-find-file)

  ;; Activation
  (dirvish-override-dired-mode 1)
  (dirvish-side-follow-mode 1))

(use-package dirvish-extras
  :ensure nil ; Provided by `dirvish'
  :after dirvish
  :config
  ;; Remove non-existent suffix (bug)
  (transient-remove-suffix 'dirvish-dispatch #'dirvish-fd-jump))

(use-package diredfl
  :ensure t
  :pin melpa

  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode)

  :init
  ;; Setup and settings (before load)
  (setopt diredfl-ignore-compressed-flag nil))

(use-package ediff
  :bind
  (:prefix-map an-ediff-map :prefix "C-c e" :prefix-docstring "Keymap for ediff entry points (global)"
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
  (setopt project-list-file (expand-file-name "projects.eld" EMACS_DATA_DIR))
  (setopt project-mode-line t))

(use-package org
  :ensure t

  :preface
  ;; Setup (preface)
  ;; Create and store org root directory
  (defconst ORG_DIR (file-name-as-directory
                     (if (getenv "XDG_DATA_HOME")
                         (expand-file-name "org/" (getenv "XDG_DATA_HOME"))
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
  (defconst ORG_CALENDAR_FILE (expand-file-name "calendar.org" ORG_DIR)
    "Default file for calendar events created with org.")
  (unless (file-regular-p ORG_CALENDAR_FILE)
    (make-empty-file ORG_CALENDAR_FILE t))

  ;; Create and store org (default) notes file
  (defconst ORG_NOTES_FILE (expand-file-name "notes.org" ORG_DIR)
    "Default file for notes (org).")
  (unless (file-regular-p ORG_NOTES_FILE)
    (make-empty-file ORG_NOTES_FILE t))

  ;; Create and store org (default) todos file
  (defconst ORG_TODOS_FILE (expand-file-name "todos.org" ORG_DIR)
    "Default file for storing todos (org).")
  (unless (file-regular-p ORG_TODOS_FILE)
    (make-empty-file ORG_TODOS_FILE t))

  ;; Create and store org (default) meetings file
  (defconst ORG_MEETINGS_FILE (expand-file-name "meetings.org" ORG_DIR)
    "Default file for meetings (org).")
  (unless (file-regular-p ORG_MEETINGS_FILE)
    (make-empty-file ORG_MEETINGS_FILE t))

  ;; PARA
  ;; Create and store org (default) projects file
  (defconst ORG_PROJECTS_FILE (expand-file-name "projects.org" ORG_DIR)
    "Default file for projects (org).")
  (unless (file-regular-p ORG_PROJECTS_FILE)
    (make-empty-file ORG_PROJECTS_FILE t))

  ;; Create and store org (default) projects file
  (defconst ORG_AREAS_FILE (expand-file-name "areas.org" ORG_DIR)
    "Default file for areas (org).")
  (unless (file-regular-p ORG_AREAS_FILE)
    (make-empty-file ORG_AREAS_FILE t))

  ;; Auxiliary
  ;; Create and store org (default) ID file
  (defconst ORG_ID_FILE (expand-file-name ".org-id-locations" ORG_DIR)
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
  (keymap-unset org-mode-map "C-M-S-<left>") ; from: org-decrease-number-at-point
  (keymap-unset org-mode-map "C-M-S-<right>")) ; from: org-increase-number-at-point

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
  (setopt pdf-view-use-unicode-ligther t)

  (pdf-loader-install t)

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

  ;; Display
  (defun an-around-advice-display-synctex (syncfun &rest args)
    "Around advice that (locally) adds an entry to `display-buffer-alist'
to reuse windows containing buffers with modes derived from
TeX-mode (for opening other such buffers).

Meant to be used with `synctex' functionality, so as to not pop up a new
window when syncing to a location in a project TeX file that is not yet
opened."
    (let* ((display-buffer-alist (cons '((derived-mode . TeX-mode)
                                         (display-buffer-reuse-window display-buffer-reuse-mode-window)
                                         (reusable-frames . visible))
                                       display-buffer-alist)))
      (apply syncfun args)))
  (advice-add #'pdf-sync-backward-search :around #'an-around-advice-display-synctex)

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

(use-package transient
  :init
  (defconst TRANSIENT_DIR (file-name-as-directory
                           (expand-file-name "transient/" EMACS_DATA_DIR))
    "Directory used to store transient control files (e.g., history, values).")
  (unless (file-directory-p TRANSIENT_DIR)
    (make-directory TRANSIENT_DIR t))

  (setopt transient-history-file (expand-file-name "history.el" TRANSIENT_DIR)
          transient-levels-file (expand-file-name "levels.el" TRANSIENT_DIR)
          transient-values-file (expand-file-name "values.el" TRANSIENT_DIR)))

(use-package magit
  :ensure t

  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-dispatch)
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
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setopt git-commit-major-mode #'log-edit-mode)

  :config
  ;; Setup and settings (after load)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  ;; Keybindings
  (keymap-set magit-diff-section-map "M-RET" #'magit-diff-visit-worktree-file)

  ;; Display
  (add-to-list 'display-buffer-alist
               '((major-mode . magit-process-mode)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side . bottom)
                 (slot . 0)
                 (window-height . fit-bt-side-window-to-buffer)
                 (preserve-size . (nil . t))))

  (with-eval-after-load 'popper
    (add-to-list 'popper-reference-buffers 'magit-process-mode)
    (when popper-mode
      (popper-mode 1)))

  ;; Activation
  (magit-wip-mode 1))

(use-package forge
  :ensure t
  :after magit

  :init
  ;; Setup and settings (before load)
  (setopt forge-owned-accounts '(("MM45" . nil)
                                 ("mmctl" . nil)))

  (defconst FORGE_DIR (file-name-as-directory
                       (expand-file-name "forge/" EMACS_DATA_DIR))
    "Directory used to store Forge data (e.g., database and drafts).")
  (unless (file-directory-p FORGE_DIR)
    (make-directory FORGE_DIR t))

  (setopt forge-database-file (expand-file-name "forge-database.sqlite" FORGE_DIR))
  (setopt forge-post-fallback-directory
          (file-name-as-directory
           (expand-file-name "drafts/" FORGE_DIR)))

  :config
  ;; Setup and settings (after load)
  ;; Keybindings
  (keymap-set forge-common-map "M-<return>" #'forge--list-menu))

(use-package xref
  :defer t

  :config
  (keymap-set xref--xref-buffer-mode-map "C-p" #'xref-prev-group)
  (keymap-set xref--xref-buffer-mode-map "C-n" #'xref-next-group))

(use-package flymake
  :defer t

  :init
  (setopt flymake-show-diagnostics-at-end-of-line 'short)

  :config
  (keymap-set flymake-mode-map "M-P" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-N" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "C-c `" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "C-c C-`" #'flymake-show-buffer-diagnostics))

(use-package eldoc
  :init
  (setopt eldoc-echo-area-display-truncation-message nil)
  (setopt eldoc-echo-area-use-multiline-p nil)
  (setopt eldoc-echo-area-prefer-doc-buffer 'maybe))

(use-package eglot
  :bind
  (:prefix-map an-eglot-map :prefix "C-c l" :prefix-docstring "Keymap for eglot (global)"
               ("`" . flymake-goto-next-error)
               ("a a" . eglot-code-actions)
               ("a e" . eglot-code-action-extract)
               ("a i" . eglot-code-action-inline)
               ("a o" . eglot-code-action-organize-imports)
               ("a r" . eglot-code-action-rewrite)
               ("f e" . eldoc-print-current-symbol-info)
               ("f E" . eldoc-doc-buffer)
               ("f i" . eglot-find-implementation)
               ("f t" . eglot-find-typeDefinition)
               ("f c" . eglot-find-declaration)
               ("f d" . xref-find-definitions)
               ("f D" . xref-find-definitions-other-window)
               ("f C-d" . xref-find-definitions-other-frame)
               ("f i" . eglot-find-implementation)
               ("f t" . eglot-find-typeDefinition)
               ("f r" . xref-find-references)
               ("F" . eglot-format)
               ("q" . eglot-shutdown)
               ("Q" . eglot-shutdown-all)
               ("r" . eglot-rename)
               ("s" . eglot)
               ("x" . eglot-code-action-quickfix)
               ("C-`" . flymake-show-buffer-diagnostics)
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
  ;; Setup and settings (before load)
  (setopt eglot-autoshutdown t)

  :config
  ;; Setup and settings (after load)
  ;; Display
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc.*\\*\\'"
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (reusable-frames . nil)
                 (direction . right)
                 (window . main)
                 (window-width . (lambda (window)
                                   (balance-windows (window-parent window)))))))

(use-package dockerfile-mode
  :ensure t

  :init
  (setopt dockerfile-build-progress "plain")
  (setopt dockerfile-use-buildkit t)
  (setopt dockerfile-indent-offset 2))

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
  ;; Setup and settings (after load)
  ;; Hooks
  (add-hook 'TeX-mode-hook #'local-setup-code-mode)

  (add-hook 'TeX-language-en-hook (lambda () (jinx-languages "en_US")))
  (add-hook 'TeX-language-nl-hook (lambda () (jinx-languages "nl")))

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Swap to \( and \) instead of $ and $ (when using LaTeX instead of TeX)
  (defun setup-a-latex-mode-electric-math ()
    (setq-local TeX-electric-math '("\\(" . "\\)")))

  (add-hook 'LaTeX-mode-hook #'setup-a-latex-mode-electric-math)

  ;; Display
  (add-to-list 'display-buffer-alist
               '((derived-mode . TeX-output-mode)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side . bottom)
                 (slot . 0)
                 (window-height . fit-bt-side-window-to-buffer)
                 (preserve-size . (nil . t))))
  (add-to-list 'display-buffer-alist
               '((major-mode . TeX-special-mode)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side . right)
                 (slot . 0)
                 (window-width . fit-lr-side-window-to-buffer)
                 (preserve-size . (t . nil))))

  (with-eval-after-load 'popper
    (add-to-list 'popper-reference-buffers 'TeX-special-mode)
    (add-to-list 'popper-reference-buffers 'TeX-output-mode)
    (add-to-list 'popper-reference-buffers 'TeX-error-overview-mode)
    (when popper-mode
      (popper-mode 1)))

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
  ;; Hooks
  (add-hook 'markdown-mode-hook #'local-setup-code-mode)

  ;; Keybindings
  (keymap-set markdown-view-mode-map "<prior>" #'scroll-up-command)
  (keymap-set markdown-view-mode-map "<next>" #'scroll-down-command)
  (keymap-set markdown-view-mode-map "<home>" #'beginning-of-buffer)
  (keymap-set markdown-view-mode-map "<end>" #'end-of-buffer))

;;; Programming
(defconst TREESIT_DIR (file-name-as-directory
                       (expand-file-name "tree-sitter/" (or (getenv "XDG_DATA_HOME") EMACS_DATA_DIR)))
  "Directory used to store tree-sitter grammars")
(unless (file-directory-p TREESIT_DIR)
  (make-directory TREESIT_DIR t))
(add-to-list 'treesit-extra-load-path TREESIT_DIR)

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
      (treesit-install-language-grammar 'bash TREESIT_DIR))))

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
      (treesit-install-language-grammar 'c TREESIT_DIR))))

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
      (treesit-install-language-grammar 'python TREESIT_DIR))))

;; Rust
(use-package rust-mode
  :ensure t

  :mode ("\\.rs\\'" . rust-mode)

  :init
  (setopt rust-mode-treesitter-derive t)

  :config
  ;; Setup and settings (after load)
  ;; Tree-sitter
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(rust "https://github.com/tree-sitter/tree-sitter-rust"
                        "v0.23.3")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'rust)
      (treesit-install-language-grammar 'rust TREESIT_DIR)))

  ;; Project (root finding)
  (defun project-find-cargo-toml (dir)
    (when-let ((root (locate-dominating-file dir "Cargo.toml")))
      (cons 'cargo-toml root)))
  (cl-defmethod project-root ((project (head cargo-toml)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-cargo-toml))

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
      (treesit-install-language-grammar 'go TREESIT_DIR))
    (add-to-list 'treesit-language-source-alist
                 '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"
                         "v1.0.2")) ; Fixed tag to match ABI of Emacs's tree-sitter
    (unless (treesit-language-available-p 'gomod)
      (treesit-install-language-grammar 'gomod TREESIT_DIR)))

  ;; Project (root finding)
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

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
      (treesit-install-language-grammar 'yaml TREESIT_DIR))))

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
      (treesit-install-language-grammar 'toml TREESIT_DIR))))

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
      (treesit-install-language-grammar 'json TREESIT_DIR))))

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

  (defun ece-consult-ripgrep-standard-library ()
    "Performs `consult-ripgrep' with EasyCrypt's standard library root as
starting directory."
    (interactive)
    (consult-ripgrep (ece--standard-library-root-canonical)))

  (defun ece-consult-fd-standard-library ()
    "Performs `consult-fd' with EasyCrypt's standard library root as
starting directory."
    (interactive)
    (consult-fd (ece--standard-library-root-canonical)))

  ;; Keybindings
  (keymap-set easycrypt-ext-general-map "C-c C-p" #'ece-proofshell-print)
  (keymap-set easycrypt-ext-general-map "C-c z p" #'ece-proofshell-print)
  (keymap-set easycrypt-ext-general-map "C-c z P" #'ece-proofshell-prompt-print)
  (keymap-set easycrypt-ext-general-map "C-c z l" #'ece-proofshell-locate)
  (keymap-set easycrypt-ext-general-map "C-c z L" #'ece-proofshell-prompt-locate)
  (keymap-set easycrypt-ext-general-map "C-c z m" #'ece-proofshell-prompt-pragma)
  (keymap-set easycrypt-ext-general-map "C-c C-s" #'ece-proofshell-search)
  (keymap-set easycrypt-ext-general-map "C-c z s" #'ece-proofshell-search)
  (keymap-set easycrypt-ext-general-map "C-c z S" #'ece-proofshell-prompt-search)
  (keymap-set easycrypt-ext-general-map "C-c z f" #'ece-find-file-standard-library)
  (keymap-set easycrypt-ext-general-map "C-c z t" 'ece-template-map-prefix)
  (keymap-set easycrypt-ext-general-map "C-c C-e" 'ece-exec-map-prefix)
  (keymap-set easycrypt-ext-general-map "C-c z e" 'ece-exec-map-prefix)

  (with-eval-after-load 'consult
    (keymap-set easycrypt-ext-general-map "C-c z F" #'ece-consult-fd-standard-library)
    (keymap-set easycrypt-ext-general-map "C-c z r" #'ece-consult-ripgrep-standard-library)))

(use-package easycrypt-ext-cape
  :ensure nil ; Provided by `easycrypt-ext'

  :hook
  (easycrypt-ext-mode . easycrypt-ext-mode-cape-setup))

(use-package easycrypt-ext-tempel
  :ensure nil ; Provided by `easycrypt-ext'

  :hook
  (easycrypt-ext-mode . easycrypt-ext-mode-tempel-setup)

  :init
  (setopt ece-tempel-template-map-prefix "C-c z t"))

(use-package easycrypt-ext-avy
  :ensure nil ; Provided by `easycrypt-ext'

  :hook
  (easycrypt-ext-mode . easycrypt-ext-mode-avy-setup)
  (easycrypt-ext-goals-mode . easycrypt-ext-goals-mode-avy-setup)
  (easycrypt-ext-response-mode . easycrypt-ext-response-mode-avy-setup))

;; Lean
(use-package nael
  :ensure t

  :mode ("\\.lean\\'" . 'nael-mode)

  :hook (nael-mode . abbrev-mode)

  :init
  ;; Setup and settings (before load)
  (setopt nael-prepare-lsp nil))

;; Themes
;; EF themes
(use-package ef-themes
  :ensure t

  :defer t

  :init
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs nil)
  (setopt modus-themes-prompts '(italic))

  :config
  (modus-themes-include-derivatives-mode 1)

  (setopt modus-operandi-tinted-palette-overrides
          modus-themes-preset-overrides-warmer)
  (setopt modus-vivendi-tinted-palette-overrides
          modus-themes-preset-overrides-cooler))

(use-package circadian
  :ensure t

  :config

  (setopt circadian-themes '((:sunrise . modus-operandi-tinted)
                             (:sunset . modus-vivendi-tinted)))
  (circadian-setup))

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
  (:map minibuffer-local-map
        ("<backtab>" . an-embark-act-with-completing-read))
  (:map an-avy-map
        ("r" . an-avy-region-char-1)
        ("R" . an-avy-region-timer))
  ;; :bind*
  ;; ("M-J" . an-avy-region-timer)

  :init
  ;; Setup and settings (before load)
  (with-eval-after-load 'avy
    (add-to-list 'avy-dispatch-alist '(?p . avy-action-a-push-mark-no-activate) t)
    (add-to-list 'avy-dispatch-alist '(?P . avy-action-a-push-mark-activate) t)
    (add-to-list 'avy-dispatch-alist '(?X . avy-action-a-kill-line-move) t)
    (add-to-list 'avy-dispatch-alist '(?\C-x . avy-action-a-kill-whole-line-move) t)
    (add-to-list 'avy-dispatch-alist '(?K . avy-action-a-kill-line-stay) t)
    (add-to-list 'avy-dispatch-alist '(?\C-k . avy-action-a-kill-whole-line-stay) t)
    (add-to-list 'avy-dispatch-alist '(?W . avy-action-a-copy-line) t)
    (add-to-list 'avy-dispatch-alist '(?\C-w . avy-action-a-copy-whole-line) t)
    (add-to-list 'avy-dispatch-alist '(?\C-y . avy-action-a-yank-whole-line) t)
    (add-to-list 'avy-dispatch-alist '(?T . avy-action-a-teleport-line) t)
    (add-to-list 'avy-dispatch-alist '(?\C-t . avy-action-a-teleport-whole-line) t)
    (add-to-list 'avy-dispatch-alist '(?o . avy-action-an-embark-select) t)
    (add-to-list 'avy-dispatch-alist '(?, . avy-action-an-embark-act) t)
    (add-to-list 'avy-dispatch-alist '(?. . avy-action-an-embark-dwim) t))

  (with-eval-after-load 'embark
    (defvar-keymap an-embark-completing-read-prompter-map
      :doc "Keymap for Embark's completing read prompter"
      "<backtab>" #'abort-recursive-edit)
    (advice-add 'embark-completing-read-prompter :around
                (an-around-advice-with-minibuffer-keymap an-embark-completing-read-prompter-map))
    (keymap-set embark-file-map "o" #'an-embark-ace-window-find-file)
    (keymap-set embark-library-map "o" #'an-embark-ace-window-find-library)
    (keymap-set embark-buffer-map "o" #'an-embark-ace-window-pop-to-buffer)
    (keymap-set embark-bookmark-map "o" #'an-embark-ace-window-bookmark-jump)
    (keymap-set embark-command-map "o" #'an-embark-ace-window-xref-find-definitions)
    (keymap-set embark-function-map "o" #'an-embark-ace-window-xref-find-definitions)
    (keymap-set embark-symbol-map "o" #'an-embark-ace-window-xref-find-definitions)
    (keymap-set embark-identifier-map "o" #'an-embark-ace-window-xref-find-definitions))

  (with-eval-after-load 'vertico
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (when (bound-and-true-p vertico--input)
                  (keymap-set vertico-map "M-P" #'an-embark-select-vertico-previous)
                  (keymap-set vertico-map "M-N" #'an-embark-select-vertico-next)))))

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

;; Load custom file
(load custom-file)
