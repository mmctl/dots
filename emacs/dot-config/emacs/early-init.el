;; early-init.el
;; (See https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html)

;; Frame parameters
;; (See https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Parameters.html)
(setopt default-frame-alist
        '((fullscreen . maximize)
          (fullscreen-restore . fullheight)
          (border-width . 0)
          (internal-border-width . 0)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (tab-bar-lines . 0)
          (minibuffer. t)
          (top-visible . 5)
          (bottom-visible . 5)
          (visibility . t)
          (auto-raise . t)
          (auto-lower . nil)
          (left-fringe . 0)
          (left-divider-width . 3)
          (right-divider-width . 3)
          (cursor-type . bar)
          ;; Non-Lucid builds: (alpha-background . 0.9)))
          (alpha . 0.9)))

;; Garbage collection
(setopt gc-cons-threshold 33554432 ; 32 MB
        gc-cons-percentage 0.15)

