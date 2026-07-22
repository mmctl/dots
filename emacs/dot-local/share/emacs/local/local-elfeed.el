;; -*- lexical-binding: t; -*-
;; local-elfeed.el

(require 'elfeed)
(require 'elfeed-show)

(require 'url)
(require 'url-parse)
(require 'url-util)

(require 'cl-lib)


;;; Utilities
(defun a-local-filename-for-url (url)
  "Return a source-qualified local filename for URL."
  (let* ((parsed-url (url-generic-parse-url url))
         (host (or (url-host parsed-url) "unknown-host"))
         (path (car (url-path-and-query parsed-url)))
         (components (mapcar #'url-unhex-string (split-string path "/" t)))
         (filename (mapconcat #'identity (cons (downcase host) components) "-")))
    (replace-regexp-in-string "[^[:alnum:]._-]+" "-" filename)))

(defun a-maybe-download-url-to-directory (url directory filename &optional redownload)
  "Download (synchronously) URL to DIRECTORY as FILENAME.

If the target already exists, reuses it unless REDOWNLOAD is non-nil.
Returns the resulting absolute path in any case."
  (unless (file-directory-p directory)
    (user-error "Chosen download directory does not exist: %s" directory))
  (let ((path (expand-file-name filename directory)))
    (when (file-directory-p path)
      (user-error "Download path is a directory: %s" path))
    (when (or redownload (not (file-exists-p path)))
      (unless (file-writable-p directory)
        (user-error "Chosen download directory is not writable: %s" directory))
      (message "Downloading %s..." url)
      (url-copy-file url path t))
    path))


;;; Helpers
(defun an-elfeed-select-enclosure (entry)
  "Return an enclosure selected from those associated with ENTRY."
  (let ((enclosures (elfeed-entry-enclosures entry)))
    (pcase (length enclosures)
      (0 (user-error "No enclosure to this entry"))
      (1 (elt enclosures 0))
      (count
       (let ((index (read-number (format "Enclosure to open (1-%d): " count))))
         (unless (and (integerp index) (<= 1 index count))
           (user-error "Enclosure number must be between 1 and %d" count))
         (elt enclosures (1- index)))))))

(defun an-elfeed-apply-tag-map (entry tag-map)
  "Apply TAG-MAP to ENTRY based on its Elfeed metadata.

TAG-MAP maps Elfeed metadata keys to alists whose string keys are
members of the corresponding list-valued entry metadata and whose
values are Elfeed tag symbols."
  (cl-loop for (key . sub-tag-map) in tag-map
           do
           (dolist (value (elfeed-meta entry key))
             (when-let* ((tag (alist-get value sub-tag-map
                                         nil nil #'string=)))
               (elfeed-tag entry tag)))))


;;; Transformers and handlers (general/default)
;; Transformers
(defun an-elfeed-transformer-identity (url)
  "Apply no transformation and return URL."
  url)

(defalias 'an-elfeed-transformer-default #'an-elfeed-transformer-identity
  "Default transformer: `an-elfeed-transformer-identity'")

;; Handlers
(defun an-elfeed-handler-browse-url (url &optional _filename _redownload _mime-type)
  "Open URL using `browse-url'."
  (browse-url url))

(defun an-elfeed-handler-download-and-visit-url (url &optional filename redownload mime-type)
  "Download URL if necessary and visit the resulting local file.

Use FILENAME as the local filename. If FILENAME is nil, ask for one,
defaulting to the source-qualified filename derived from URL.

Store the file in `elfeed-enclosure-default-dir'. Reuse an existing
local copy unless REDOWNLOAD is non-nil."
  (let ((filename (or filename
                      (read-string "Filename: " (a-local-filename-for-url url)))))
    (when (string-empty-p filename)
      (user-error "Filename may not be empty"))
    (let* ((directory (if (functionp elfeed-enclosure-default-dir)
                          (funcall elfeed-enclosure-default-dir filename mime-type)
                        elfeed-enclosure-default-dir))
           (path (a-maybe-download-url-to-directory url directory filename redownload)))
      (find-file path))))

(defalias 'an-elfeed-handler-default #'an-elfeed-handler-browse-url
  "Default handler: `an-elfeed-handler-browse-url'")


;;; Data structures
;; Feed configurations
(cl-defstruct an-elfeed-feed-config
  "Configuration for an Elfeed feed."
  (url nil
       :documentation
       "URL of the feed.")
  (default-tags nil
                :documentation
                "Tags applied by default to every entry from the feed.")
  (tag-map nil
           :documentation
           "Alist with mappings from feed metadata keys/values to Elfeed tags.")
  (link-transformer #'an-elfeed-transformer-default
                    :documentation
                    "Function used to transform entry links.")
  (link-handler #'an-elfeed-handler-default
                :documentation
                "Function used to handle transformed entry links.")
  (enclosure-handler #'an-elfeed-handler-default
                     :documentation
                     "Function used to handle enclosure URLs."))


;;; Feeds
;; IACR ePrint
;; Transformers
(defun an-elfeed-transformer-iacr-eprint-entry-to-pdf (url)
  "Return the PDF URL corresponding to an IACR ePrint entry URL."
  (concat url ".pdf"))

;; Configuration
(defconst IACR_EPRINT_FEED_CONFIG
  (make-an-elfeed-feed-config
   :url "https://eprint.iacr.org/rss/atom.xml?order=recent"
   :default-tags '(research cryptography iacr eprint)
   :tag-map '((:categories
               . (("Applications" . iacr-applications)
                  ("Foundations" . iacr-foundations)
                  ("Implementation" . iacr-implementation)
                  ("Public-key cryptography" . iacr-public-key)
                  ("Secret-key cryptography" . iacr-secret-key)
                  ("Cryptographic protocols" . iacr-protocols)
                  ("Attacks and cryptanalysis" . iacr-attacks-cryptanalysis))))
   :link-transformer #'an-elfeed-transformer-iacr-eprint-entry-to-pdf)
  "Feed configuration for IACR Cryptology ePrint Archive.")

;; Shtetl Optimized (Scott Aaronson)
(defconst SHTETL_OPTIMIZED_FEED_CONFIG
  (make-an-elfeed-feed-config
   :url "https://scottaaronson.blog/?feed=rss2"
   :default-tags '(blog quantum shtetl))
  "Feed configuration for Shtetl Optimized.")

;; Machine Logic (Lawrence C Paulson)
(defconst MACHINE_LOGIC_FEED_CONFIG
  (make-an-elfeed-feed-config
   :url "https://lawrencecpaulson.github.io/feed.xml"
   :default-tags '(blog formal-methods machine-logic))
  "Feed configuration for Machine Logic.")

;; All feed configurations
(defconst ALL_FEED_CONFIGS (list IACR_EPRINT_FEED_CONFIG
                                 SHTETL_OPTIMIZED_FEED_CONFIG
                                 MACHINE_LOGIC_FEED_CONFIG)
  "List of all feed configurations.")


;;; Helpers
(defun an-elfeed-feed-config-from-entry (entry)
  "Return a feed configuration for (the feed of) ENTRY, as per `ALL_FEED_CONFIGS'.

Signal a user error when no such specification exists."
  (let* ((feed-url (elfeed-feed-url (elfeed-entry-feed entry)))
         (feed-config (cl-find feed-url ALL_FEED_CONFIGS
                               :key #'an-elfeed-feed-config-url
                               :test #'string=)))
    (or feed-config
        (user-error "No feed configuration for feed %s" feed-url))))

(defun an-elfeed-tag-entry-from-feed (entry)
  "Tag ENTRY according to its feed-related metadata, as per the tag map
defined in the feed's configuration."
  (when-let* ((feed-config (an-elfeed-feed-config-from-entry entry))
              (feed-tag-map (an-elfeed-feed-config-tag-map feed-config)))
    (an-elfeed-apply-tag-map entry feed-tag-map)))


;;; Commands
(defun an-elfeed-handle-link (&optional redownload)
  "Handle link associated with the shown/visited Elfeed entry.

Obtains the transformer and handler from the relevant feed's
configuration in `ALL_FEED_CONFIGS', which see.

Interactively, pass the prefix argument as REDOWNLOAD. Download
handlers may use it to replace an existing local copy; handlers for
which it is irrelevant may ignore it."
  (interactive "P" elfeed-show-mode)
  (let* ((entry elfeed-show-entry)
         (link (or (elfeed-entry-link entry)
                   (user-error "No link to this entry")))
         (feed-config (an-elfeed-feed-config-from-entry entry))
         (link-transformer (an-elfeed-feed-config-link-transformer feed-config))
         (link-handler (an-elfeed-feed-config-link-handler feed-config)))
    (unless (functionp link-transformer)
      (user-error "Feed configuration has no valid link transformer"))
    (unless (functionp link-handler)
      (user-error "Feed configuration has no valid link handler"))
    (let ((url (funcall link-transformer link)))
      (unless (and (stringp url) (not (string-empty-p url)))
        (user-error "Link transformer returned an invalid URL"))
      (funcall link-handler url (a-local-filename-for-url url) redownload))))

(defun an-elfeed-handle-enclosure (&optional redownload)
  "Handle enclosure associated with the shown/visited Elfeed entry.

Obtains the transformer and handler from the relevant feed's
configuration in `ALL_FEED_CONFIGS', which see.

Interactively, pass the prefix argument as REDOWNLOAD. Download
handlers may use it to replace an existing local copy; handlers for
which it is irrelevant may ignore it."
  (interactive "P" elfeed-show-mode)
  (let* ((entry elfeed-show-entry)
         (enclosure (an-elfeed-select-enclosure entry))
         (url (car enclosure))
         (mime-type (cadr enclosure))
         (filename (funcall elfeed-show-enclosure-filename-function
                            entry url))
         (feed-config (an-elfeed-feed-config-from-entry entry))
         (enclosure-handler (an-elfeed-feed-config-enclosure-handler feed-config)))
    (unless (functionp enclosure-handler)
      (user-error "Feed configuration has no valid enclosure handler"))
    (funcall enclosure-handler url filename redownload mime-type)))


(provide 'local-elfeed)

;;; local-elfeed.el ends here
