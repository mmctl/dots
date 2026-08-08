;; -*- lexical-binding: t -*-
;; local-para.el

(require 'para)


;;;; Configuration constants

(defconst PARA_ITEM_WORKSPACE_DIR_NAME "workspace"
  "Directory name used for workspace root in PARA items.")

(defconst PARA_DENOTE_INDEX_KEYWORD "index"
  "Denote keyword used to identify PARA index notes.")

(defconst PARA_ORG_AGENDA_FILE_NAME "agenda.org"
  "File name used for Org agenda files in PARA items.")

(defconst PARA_ORG_AGENDA_ITEM_TYPES '(project area)
  "PARA item types whose agenda files are included in `org-agenda-files'.")


;;; Item creation
(defun a-para-create-index-file-denote (item)
  "Create a Denote index note for ITEM.

Use the item name as the initial title and add `index' and the item's
PARA type as the initial keywords.  Store the resulting file name as
the transient `index-file' property of ITEM for use by subsequent
creation functions."
  (require 'denote)
  (let* ((type (symbol-name (para-item-type item)))
         (title (denote-title-prompt (para-item-name item) "Index file TITLE"))
         (keywords (denote-keywords-prompt "Index file KEYWORDS" (string-join (list PARA_DENOTE_INDEX_KEYWORD type) ",")))
         (denote-use-directory (para-item-root item))
         (denote-use-title title)
         (denote-use-keywords keywords))
    (para-item-put item 'title title)
    (para-item-put item 'index-file (denote))))

(defun a-para-org-agenda-file-name (item)
  "Return the Org agenda file name for ITEM."
  (para-item-expand-file-name PARA_ORG_AGENDA_FILE_NAME item))

(defun a-para-org-agenda-file-front-matter (item)
  "Return Org agenda front matter for ITEM."
  (let ((title (para-item-get item 'title))
        (type (capitalize (symbol-name (para-item-type item)))))
    (format "#+TITLE: %s\n#+CATEGORY: %s\n#+FILETAGS: :%s:\n\n"
            title title type)))

(defun a-para-org-agenda-file-main-matter ()
  "Return Org agenda main matter for ITEM."
  (format "* Tasks%s" (if ORG_REFILE_TARGET_TAG
                          (concat " :" ORG_REFILE_TARGET_TAG ":")
                        "")))

(defun a-para-create-agenda-file-org (item)
  "Create the Org agenda file for ITEM with appropriately initialized content."
  (with-temp-file (a-para-org-agenda-file-name item)
    (insert (a-para-org-agenda-file-front-matter item))
    (insert (a-para-org-agenda-file-main-matter))))

(defun a-para-create-workspace (item)
  "Create a workspace directory for ITEM."
  (make-directory (file-name-as-directory (para-item-expand-file-name PARA_ITEM_WORKSPACE_DIR_NAME item))))

(defun a-para-visit-created-index (item)
  "Visit the Denote index note created for ITEM.

This function is intended for use during the item-creation pipeline:
it relies on the transient `index-file' property established by
`a-para-create-index-file-denote'."
  (find-file (para-item-get item 'index-file)))


;;; Org agenda integration
(defun a-para-org-agenda-roots ()
  "Return all roots that may contain PARA-managed Org agenda files.

Include active and archived roots so stale agenda entries left behind
by moves or archiving can also be recognized and removed."
  (apply #'append
         (mapcar (lambda (type)
                   (append (para-type-directories type)
                           (para-archive-type-directories type)))
                 PARA_ORG_AGENDA_ITEM_TYPES)))

(defun a-para-managed-agenda-file-p (file)
  "Return non-nil when FILE is a PARA-managed Org agenda file.

A managed agenda file is named `PARA_ORG_AGENDA_FILE_NAME' and is
located directly inside an item directory below one of the active or
archived roots in `PARA_ORG_AGENDA_ITEM_TYPES'."
  (and (stringp file)
       (string= (file-name-nondirectory file) PARA_ORG_AGENDA_FILE_NAME)
   (seq-some (lambda (directory)
               (let* ((relative (file-relative-name (expand-file-name file)
                                                    (file-name-as-directory (expand-file-name directory))))
                      (components (split-string relative "/" t)))
                 (and (= (length components) 2)
                      (not (member (car components) '("." "..")))
                      (string= (cadr components) PARA_ORG_AGENDA_FILE_NAME))))
             (a-para-org-agenda-roots))))

(defun a-para-agenda-files ()
  "Return Org agenda files belonging to active PARA items."
  (seq-keep (lambda (item)
              (let ((file (a-para-org-agenda-file-name item)))
                (and (file-regular-p file) file)))
            (para-active-items PARA_ORG_AGENDA_ITEM_TYPES)))

(defun a-para-refresh-agenda-files (&rest _)
  "Synchronize PARA-managed entries in `org-agenda-files'.

Preserve agenda files not managed by PARA, remove stale PARA entries,
and append the agenda files of all currently active PARA projects and
areas."
  (unless (and (boundp 'org-agenda-files) (listp org-agenda-files))
    (user-error "`org-agenda-files' void or not configured as a list"))
  (setq org-agenda-files
        (append (seq-remove #'a-para-managed-agenda-file-p org-agenda-files)
                (a-para-agenda-files))))


(provide 'local-para)

;;; local-para.el ends here
