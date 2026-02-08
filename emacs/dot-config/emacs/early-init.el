;; -*- lexical-binding: t; -*-
;; early-init.el
;; (See https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html)

;; ;;
;; (defconst EMACS_CONFIG_DIR (file-name-as-directory
;;                             (if (getenv "XDG_CONFIG_HOME")
;;                                 (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/")
;;                               user-emacs-directory))
;;   "Directory where Emacs configuration is stored.")

;; (defconst THEMES_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "themes/"))
;;   "Directory where (custom) themes are stored.")

;; (defconst LOCAL_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "local/"))
;;   "Directory where (custom) local functionalities/packages are defined.")

;; (defconst TEMPLATES_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "templates/"))
;;   "Directory where (custom) templates are defined.")

;; (defconst MISC_DIR (file-name-as-directory (file-name-concat EMACS_CONFIG_DIR "misc/"))
;;   "Directory where (custom) miscellaneous configuration/settings are stored.")

;; (defconst CUSTOM_FILE (file-name-concat MISC_DIR "custom-set.el")
;;   "File where (automatically generated) customization settings are stored.")

;; ;; Data
;; (defconst EMACS_DATA_DIR (file-name-as-directory
;;                           (if (getenv "XDG_DATA_HOME")
;;                               (file-name-concat (getenv "XDG_DATA_HOME") "emacs/")
;;                             user-emacs-directory))
;;   "Directory where (additional) Emacs data is stored.")

;; (defconst BACKUPS_DIR (file-name-as-directory (file-name-concat EMACS_DATA_DIR "backups/"))
;;   "Directory where (automatically generated) backup files are stored.")

;; (defconst AUTHINFO_FILE (file-name-concat EMACS_DATA_DIR ".authinfo.gpg")
;;   "File where (encrypted) authentication information is stored.")

;; ;; Cache
;; (defconst EMACS_CACHE_DIR (file-name-as-directory
;;                            (if (getenv "XDG_CACHE_HOME")
;;                                (file-name-concat (getenv "XDG_CACHE_HOME") "emacs/")
;;                              user-emacs-directory))
;;   "Directory where Emacs cache is stored.")

;; (defconst AUTOSAVES_DIR (file-name-as-directory (file-name-concat EMACS_CACHE_DIR "autosaves/"))
;;   "Directory where auto-save files are stored.")

;; (defconst LOCKS_DIR (file-name-as-directory (file-name-concat EMACS_CACHE_DIR "locks/"))
;;   "Directory where lock files are stored.")

;; (unless (file-directory-p THEMES_DIR)
;;   (make-directory THEMES_DIR t))

;; (unless (file-directory-p LOCAL_DIR)
;;   (make-directory LOCAL_DIR t))

;; (unless (file-directory-p TEMPLATES_DIR)
;;   (make-directory TEMPLATES_DIR t))

;; (unless (file-directory-p MISC_DIR)
;;   (make-directory MISC_DIR t))

;; (unless (file-directory-p BACKUPS_DIR)
;;   (make-directory BACKUPS_DIR t))

;; (unless (file-directory-p AUTOSAVES_DIR)
;;   (make-directory AUTOSAVES_DIR t))

;; (unless (file-directory-p LOCKS_DIR)
;;   (make-directory LOCKS_DIR t))

;; ;; Custom file
;; (unless (file-exists-p CUSTOM_FILE)
;;   (make-empty-file CUSTOM_FILE))
;; (setopt custom-file CUSTOM_FILE)



;; ;; Byte/Native compilation and loading
;; (setopt load-prefer-newer t)
;; (setopt native-comp-jit-compilation t)
;; (setopt native-comp-async-query-on-exit t)
;; (setopt package-native-compile t)

;; ;; Frame parameters
;; ;; (See https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Parameters.html)
;; (setopt default-frame-alist
;;         '((fullscreen . maximize)
;;           (fullscreen-restore . fullheight)
;;           (border-width . 0)
;;           (internal-border-width . 0)
;;           (vertical-scroll-bars . nil)
;;           (horizontal-scroll-bars . nil)
;;           (menu-bar-lines . 0)
;;           (tool-bar-lines . 0)
;;           (tab-bar-lines . 0)
;;           (minibuffer. t)
;;           (top-visible . 5)
;;           (bottom-visible . 5)
;;           (visibility . t)
;;           (auto-raise . t)
;;           (auto-lower . nil)
;;           (left-fringe . 8)
;;           (right-fringe . 8)
;;           (left-divider-width . 3)
;;           (right-divider-width . 3)
;;           (cursor-type . (hbar . 3))))
;;           ;; Transparency non-Lucid builds: (alpha-background . 0.9)))
;;           ;; Transparency Lucid builds: (alpha . 0.9)

;; ;; Garbage collection
;; (setopt gc-cons-threshold 33554432
;;         gc-cons-percentage 0.15)

;;; Base directories and files

;; Configuration
(defconst EMACS_CONFIG_DIR (file-name-as-directory
                            (if (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "emacs/" (getenv "XDG_CONFIG_HOME"))
                              user-emacs-directory))
  "Directory where Emacs configuration is stored.")

(defconst MISC_DIR (file-name-as-directory (expand-file-name "misc/" EMACS_CONFIG_DIR))
  "Directory where (custom) miscellaneous configuration/settings are stored.")

(defconst CUSTOM_FILE (expand-file-name "custom-set.el" MISC_DIR)
  "File where (automatically generated) customization settings are stored.")

(unless (file-directory-p MISC_DIR)
  (make-directory MISC_DIR t))

(unless (file-exists-p CUSTOM_FILE)
  (make-empty-file CUSTOM_FILE))

;; Data
(defconst EMACS_DATA_DIR (file-name-as-directory
                          (if (getenv "XDG_DATA_HOME")
                              (expand-file-name "emacs/" (getenv "XDG_DATA_HOME"))
                            user-emacs-directory))
  "Directory where (additional) Emacs data is stored.")

(defconst PACKAGE_DIR (file-name-as-directory (expand-file-name "elpa/" EMACS_DATA_DIR))
  "Directory where packages are stored.")

(defconst NATIVE_COMP_DIR (file-name-as-directory (expand-file-name "eln-cache/" EMACS_DATA_DIR))
  "Directory where natively compiled files are stored.")

(defconst BACKUPS_DIR (file-name-as-directory (expand-file-name "backups/" EMACS_DATA_DIR))
  "Directory where (automatically generated) backup files are stored.")

(defconst AUTOSAVES_DIR (file-name-as-directory (expand-file-name "autosaves/" EMACS_DATA_DIR))
  "Directory where auto-save files are stored.")

(defconst THEMES_DIR (file-name-as-directory (expand-file-name "themes/" EMACS_DATA_DIR))
  "Directory where (custom) themes are stored.")

(defconst LOCAL_DIR (file-name-as-directory (expand-file-name "local/" EMACS_DATA_DIR))
  "Directory where (custom) local functionalities/packages are defined.")

(defconst TEMPLATES_DIR (file-name-as-directory (expand-file-name "templates/" EMACS_DATA_DIR))
  "Directory where (custom) templates are defined.")

(defconst TRAMP_DIR (file-name-as-directory (expand-file-name "tramp/" EMACS_DATA_DIR))
  "Directory where tramp-related data is stored.")

(defconst AUTHINFO_FILE (expand-file-name ".authinfo.gpg" EMACS_DATA_DIR)
  "File where (encrypted) authentication information is stored.")

(unless (file-directory-p PACKAGE_DIR)
  (make-directory PACKAGE_DIR t))

(unless (file-directory-p NATIVE_COMP_DIR)
  (make-directory NATIVE_COMP_DIR t))

(unless (file-directory-p BACKUPS_DIR)
  (make-directory BACKUPS_DIR t))

(unless (file-directory-p AUTOSAVES_DIR)
  (make-directory AUTOSAVES_DIR t))

(unless (file-directory-p THEMES_DIR)
  (make-directory THEMES_DIR t))

(unless (file-directory-p LOCAL_DIR)
  (make-directory LOCAL_DIR t))

(unless (file-directory-p TEMPLATES_DIR)
  (make-directory TEMPLATES_DIR t))

(unless (file-directory-p TRAMP_DIR)
  (make-directory TRAMP_DIR t))

;; Cache
(defconst EMACS_CACHE_DIR (file-name-as-directory
                           (if (getenv "XDG_CACHE_HOME")
                               (expand-file-name "emacs/" (getenv "XDG_CACHE_HOME"))
                             user-emacs-directory))
  "Directory where Emacs cache is stored.")

(defconst LOCKS_DIR (file-name-as-directory (expand-file-name "locks/" EMACS_CACHE_DIR))
  "Directory where lock files are stored.")

(unless (file-directory-p LOCKS_DIR)
  (make-directory LOCKS_DIR t))

;;; Native compilation

(setopt native-comp-jit-compilation t
        native-comp-async-query-on-exit t)
(setopt package-native-compile t)

(startup-redirect-eln-cache NATIVE_COMP_DIR)

;;; Garbage collection

(setopt gc-cons-threshold 33554432
        gc-cons-percentage 0.15)

;;; Package system and management

(setopt package-user-dir PACKAGE_DIR)

;;; Default/Base frame

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
          (left-fringe . 8)
          (right-fringe . 8)
          (left-divider-width . 3)
          (right-divider-width . 3)
          (cursor-type . (hbar . 3))))

;;; Miscellaneous

;; File loading
(setopt load-prefer-newer t)
