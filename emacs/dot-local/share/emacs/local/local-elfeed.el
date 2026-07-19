;; -*- lexical-binding: t; -*-
;; local-elfeed.el

(require 'elfeed)
(require 'elfeed-show)

(require 'url)
(require 'url-parse)
(require 'url-util)


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

If a file with the name derived from URL already exists in DIRECTORY,
only redownloads if REDOWNLOAD is non-nil. Returns the resulting path in
any case (barring a potential error)."
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

(defun an-elfeed-select-enclosure (entry)
  "Return an enclosure selected from those associated with ENTRY."
  (let ((enclosures (elfeed-entry-enclosures entry)))
    (pcase (length enclosures)
      (0 (user-error "No enclosure to this entry"))
      (1 (elt enclosures 0))
      (count
       (let ((index (read-number (format "Enclosure to open (1-%d): " count))))
         (unless (<= 1 index count)
           (user-error "Enclosure number must be between 1 and %d" count))
         (elt enclosures (1- index)))))))


;;; Feeds
;; IACR ePrint
(defconst IACR_EPRINT_FEED_ATOM "https://eprint.iacr.org/rss/atom.xml?order=recent"
  "Atom feed for IACR Cryptology ePrint Archive.")

(defconst IACR_EPRINT_CATEGORY_TAGS
  '(("Applications" . iacr-applications)
    ("Foundations" . iacr-foundations)
    ("Implementation" . iacr-implementation)
    ("Public-key cryptography" . iacr-public-key)
    ("Secret-key cryptography" . iacr-secret-key)
    ("Cryptographic protocols" . iacr-protocols)
    ("Attacks and cryptanalysis" . iacr-attacks-cryptanalysis))
  "Mapping from IACR ePrint categories to Elfeed tags.")

(defun an-iacr-eprint-entry-url-to-pdf-url (url)
  "Return the PDF URL corresponding to an IACR ePrint entry URL."
  (concat url ".pdf"))

(defun an-elfeed-tag-iacr-eprint-entry (entry)
  "Tag IACR ePrint ENTRY according to its upstream category, as per
`IACR_EPRINT_CATEGORY_TAGS'."
  (when (equal (elfeed-feed-url (elfeed-entry-feed entry)) IACR_EPRINT_FEED_ATOM)
    (dolist (category (elfeed-meta entry :categories))
      (when-let* ((tag (cdr (assoc-string category IACR_EPRINT_CATEGORY_TAGS t))))
        (elfeed-tag entry tag)))))


;;; Handlers
(defun an-elfeed-download-and-visit-url (url &optional filename redownload mime-type)
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

(defun an-elfeed-browse-url (url &optional _filename _redownload _mime-type)
  "Open URL using `browse-url'."
  (browse-url url))

(defconst ELFEED_URL_HANDLING_SPECIFICATIONS
  `(((feed . ,IACR_EPRINT_FEED_ATOM)
     :transformer an-iacr-eprint-entry-url-to-pdf-url
     :handler an-elfeed-browse-url))
  "Mapping from Elfeed sources to transformed-url handling specifications.

Each key has the form (TYPE . VALUE), where TYPE is either `feed'
or `host'. Each corresponding value is a property list containing
`:transformer' and `:handler'.

Feed-specific rules take precedence over host-wide rules.")

(defun an-elfeed-url-handling-spec-for-entry (entry &optional default)
  "Return a registered url-handling specification for ENTRY.

Prefers an exact feed rule over a host-wide rule. Return DEFAULT when
there is no matching rule, or signal a user error when additionally
DEFAULT is nil."
  (let* ((feed (elfeed-entry-feed entry))
         (feed-url (and feed (elfeed-feed-url feed)))
         (host (and feed-url (url-host (url-generic-parse-url feed-url))))
         (host (and host (downcase host))))
    (or (and feed-url
             (alist-get (cons 'feed feed-url)
                        ELFEED_URL_HANDLING_SPECIFICATIONS
                        nil nil #'equal))
        (and host
             (alist-get (cons 'host host)
                        ELFEED_URL_HANDLING_SPECIFICATIONS
                        nil nil #'equal))
        default
        (user-error "No url-handling specification for feed %s or host %s"
                    feed-url host))))

(defun an-elfeed-handle-link (&optional redownload)
  "Handle link associated with the shown/visited Elfeed entry.

Selects a url transformer and handler and from
`ELFEED_URL_HANDLING_SPECIFICATIONS' as per
`an-elfeed-url-handling-spec-for-entry', which see.

Interactively, pass the prefix argument as REDOWNLOAD. Download
handlers may use it to replace an existing local copy; handlers for
which it is irrelevant may ignore it."
  (interactive "P" elfeed-show-mode)
  (let* ((entry elfeed-show-entry)
         (link (or (elfeed-entry-link entry)
                   (user-error "No link to this entry")))
         (spec (an-elfeed-url-handling-spec-for-entry entry))
         (transformer (plist-get spec :transformer))
         (handler (plist-get spec :handler)))
    (unless (functionp transformer)
      (user-error "Registry entry has no valid transformer"))
    (unless (functionp handler)
      (user-error "Registry entry has no valid handler"))
    (let ((url (funcall transformer link)))
      (unless (and (stringp url) (not (string-empty-p url)))
        (user-error "Transformer returned an invalid URL"))
      (funcall handler url (a-local-filename-for-url url) redownload))))

(defun an-elfeed-handle-enclosure (&optional redownload)
  "Handle enclosure associated with the shown/visited Elfeed entry.

Selects a url transformer and handler and from
`ELFEED_URL_HANDLING_SPECIFICATIONS' as per
`an-elfeed-url-handling-spec-for-entry', which see. When no source-specific
specification exists, download and visit the enclosure directly.

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
         (spec (an-elfeed-url-handling-spec-for-entry
                entry
                '(:handler an-elfeed-download-and-visit-url)))
         (handler (plist-get spec :handler)))
    (unless (functionp handler)
      (user-error "Registry entry has no valid handler"))
    (funcall handler url filename redownload mime-type)))


(provide 'local-elfeed)

;;; local-elfeed.el ends here
